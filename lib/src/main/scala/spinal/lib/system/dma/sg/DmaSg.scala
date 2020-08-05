package spinal.lib.system.dma.sg

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.bus.bmb.sim.{BmbDriver, BmbMemoryAgent}
import spinal.lib.bus.bsb.{BsbParameter, _}
import spinal.lib.bus.bsb.sim.BsbMonitor
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.sim.{MemoryRegionAllocator, SparseMemory, StreamDriver, StreamReadyRandomizer}
import spinal.lib.system.dma.sg.DmaSg.{Channel, Parameter}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


object DmaSg{

  def getCtrlCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = 12,
    dataWidth = 32
  )

  def getReadRequirements(p : Parameter) = BmbAccessParameter(
    addressWidth = p.readAddressWidth,
    dataWidth    = p.readDataWidth
  ).addSources(p.channels.count(_.canRead), BmbSourceParameter(
    contextWidth = widthOf(p.FetchContext()),
    lengthWidth = p.readLengthWidth,
    canWrite = false
  ))

  def getWriteRequirements(p : Parameter) = BmbAccessParameter(
    addressWidth = p.writeAddressWidth,
    dataWidth    = p.writeDataWidth
  ).addSources(p.channels.count(_.canWrite), BmbSourceParameter(
    contextWidth = widthOf(p.WriteContext()),
    lengthWidth = p.writeLengthWidth,
    canRead = false
  ))

  case class Parameter(readAddressWidth : Int,
                       readDataWidth : Int,
                       readLengthWidth : Int,
                       writeAddressWidth : Int,
                       writeDataWidth : Int,
                       writeLengthWidth : Int,
                       memory : DmaMemoryLayout,
                       outputs : Seq[BsbParameter],
                       inputs : Seq[BsbParameter],
                       channels : Seq[Channel],
                       bytePerTransferWidth : Int,
                       pendingWritePerChannel : Int = 15){
    val outputsSourceWidth = outputs.map(_.sourceWidth).fold(0)(Math.max)
    val outputsSinkWidth   = outputs.map(_.sinkWidth).fold(0)(Math.max)
    val inputsSourceWidth  = inputs.map(_.sourceWidth).fold(0)(Math.max)
    val inputsSinkWidth    = inputs.map(_.sinkWidth).fold(0)(Math.max)
    val readWriteMinDataWidth = Math.min(readDataWidth, writeDataWidth)
    val readWriteMaxDataWidth = Math.max(readDataWidth, writeDataWidth)
    val writeByteCount = writeDataWidth/8
    assert(outputs.map(_.byteCount*8).fold(0)(Math.max) <= readWriteMinDataWidth)
    assert(inputs.map(_.byteCount*8).fold(0)(Math.max) <= readWriteMinDataWidth)
    assert(readWriteMaxDataWidth <= memory.bankCount*memory.bankWidth)

    def canRead = channels.exists(_.canRead)
    def canWrite = channels.exists(_.canWrite)
    case class FetchContext() extends Bundle{
      val channel = UInt(log2Up(channels.count(_.canRead)) bits)
      val start, stop = UInt(log2Up(readDataWidth/8) bits)
      val last = Bool()
    }

    case class WriteContext() extends Bundle{
      val channel = UInt(log2Up(channels.count(_.canWrite)) bits)
    }
  }



  case class Channel(memoryToMemory : Boolean,
                     inputsPorts : Seq[Int],
                     outputsPorts : Seq[Int]){
    def canRead = memoryToMemory || outputsPorts.nonEmpty
    def canWrite = memoryToMemory || inputsPorts.nonEmpty
    def canInput = inputsPorts.nonEmpty
    def canOutput = outputsPorts.nonEmpty
  }

  case class ChannelIo(p : Channel) extends Bundle{
    val interrupt = out Bool()
  }

  case class Core[CTRL <: Data with IMasterSlave](p : Parameter, ctrlType : HardType[CTRL], slaveFactory : CTRL => BusSlaveFactory) extends Component{
    val io = new Bundle {
      val read = p.canRead generate master(Bmb(getReadRequirements(p)))
      val write = p.canWrite generate master(Bmb(getWriteRequirements(p)))
      val outputs = Vec(p.outputs.map(s => master(Bsb(s))))
      val inputs = Vec(p.inputs.map(s => slave(Bsb(s))))
      val interrupts = out Bits(p.channels.size bits)
      val ctrl = slave(ctrlType())
    }

    val ctrl = slaveFactory(io.ctrl)

    val ptrWidth = log2Up(p.memory.bankWords*p.memory.bankCount) + 1
    val ptrType = HardType(UInt(ptrWidth bits))

    case class InputContext(ip : BsbParameter, portId : Int) extends Bundle {
      val channel = Bits(p.channels.count(_.inputsPorts.contains(portId)) bits)
      val bytes = UInt(log2Up(ip.byteCount + 1) bits)
      val flush = Bool()
      val veryLast = Bool()
    }
    case class M2bWriteContext() extends Bundle{
      val last = Bool()
      val channel = UInt(log2Up(p.channels.count(_.canRead)) bits)
      val loadByteInNextBeat = UInt(log2Up(p.readDataWidth/8 + 1) bits)
    }
    case class B2sReadContext(portId : Int) extends Bundle {
      val channel = Bits(p.channels.count(_.outputsPorts.contains(portId)) bits)
      val veryLast = Bool()
    }

    val memory = new Area{
      val writesParameter = ArrayBuffer[DmaMemoryCoreWriteParameter]()
      val readsParameter = ArrayBuffer[DmaMemoryCoreReadParameter]()

      writesParameter ++= p.inputs.zipWithIndex.map{case (p, portId) => DmaMemoryCoreWriteParameter(p.byteCount, widthOf(InputContext(p, portId)), false)}
      readsParameter ++= p.outputs.zipWithIndex.map{case (p, portId) => DmaMemoryCoreReadParameter(p.byteCount, widthOf(B2sReadContext(portId)), false)}

      if(p.canRead) writesParameter += DmaMemoryCoreWriteParameter(io.read.p.access.byteCount, widthOf(M2bWriteContext()), true)
      if(p.canWrite) readsParameter += DmaMemoryCoreReadParameter(io.write.p.access.byteCount, ptrWidth + 1, true)

      val core = DmaMemoryCore(DmaMemoryCoreParameter(
        layout = p.memory,
        writes = writesParameter,
        reads  = readsParameter
      ))

      val ports = new Area{
        val m2b = core.io.writes.last
        val b2m = core.io.reads.last
        val s2b = core.io.writes.take(io.inputs.size)
        val b2s = core.io.reads.take(io.outputs.size)
      }
    }

    class Interrupt(fire : Bool) extends Area{
      val enable = Reg(Bool) init(False)
      val valid = Reg(Bool) init(False) setWhen(enable && fire)
    }

    class ChannelLogic(val id : Int) extends Area{
      val cp = p.channels(id)
      val start = False
      val stop = Reg(Bool)
      val done = False
      val valid = RegInit(False) setWhen(start) clearWhen(done)
      val bytes = Reg(UInt(p.bytePerTransferWidth bits)) //minus one
      val priority = Reg(UInt(p.memory.priorityWidth bits))
      val selfRestart = Reg(Bool)


      val fifo = new Area{
        def bytesType = UInt(log2Up(p.memory.bankWords*p.memory.bankCount*p.memory.bankWidth/8+1) bits)

        val base = Reg(ptrType)
        val words = Reg(ptrType) //Minus one

        val push = new Area {
          val available = Reg(ptrType)
          val availableDecr = ptrType()
          availableDecr := 0

          val ptr = Reg(ptrType)
          val ptrWithBase = (base & ~words) | (ptr & words)
          val ptrIncr = DataOr(ptrType)
          ptr := ptr + ptrIncr.value

          when(start){
            ptr := 0
          }
        }

        val pop = new Area {
          val ptr = Reg(ptrType)
          val bytes = Reg(bytesType)
          val ptrWithBase = (base & ~words) | (ptr.resized & words)

          val bytesIncr = DataOr(bytesType)
          val bytesDecr = DataOr(bytesType)
          bytes := bytes + bytesIncr.value - bytesDecr.value


          val empty = ptr === push.ptr


          val ptrIncr = DataOr(ptrType)
          ptr := ptr + ptrIncr.value



        }

        val empty = push.ptr === pop.ptr

      }


      val push = new Area{
        val memory  = Reg(Bool)
        val loadDone = RegInit(True)
        val bytesLeft = Reg(UInt(p.bytePerTransferWidth bits)) //minus one

        when(start){
          bytesLeft := bytes
          loadDone := False
        }

        val m2b = cp.canRead generate new Area {
          val address = Reg(UInt(io.read.p.access.addressWidth bits))
          val bytePerBurst = Reg(UInt(io.read.p.access.lengthWidth bits)) //minus one
          val loadRequest = !loadDone && memory && fifo.push.available > (bytePerBurst >> log2Up(p.memory.bankWidth/8))
        }

        val s2b = cp.inputsPorts.nonEmpty generate new Area {
          val portId = Reg(UInt(log2Up(p.inputs.size) bits))
          val sinkId = Reg(UInt(p.inputsSinkWidth bits))
        }
      }

      val pop = new Area{
        val memory  = Reg(Bool)
        val b2s = cp.canOutput generate new Area{
          val last  = Reg(Bool)
          val portId = Reg(UInt(log2Up(p.outputs.size) bits))
          val sinkId = Reg(UInt(p.outputsSinkWidth bits))

          val veryLastTrigger = False
          val veryLastValid = Reg(Bool) setWhen(veryLastTrigger) clearWhen(start)
          val veryLastPtr = Reg(ptrType)
          when(veryLastTrigger){
            veryLastPtr := fifo.push.ptrWithBase
          }
        }


        val pushDone = Reg(Bool) clearWhen(start)
        val popDone = Reg(Bool) clearWhen(start)

        val b2m = cp.canWrite generate new Area{
          val bytePerBurst = Reg(UInt(io.write.p.access.lengthWidth bits))
          val flush = Reg(Bool) setWhen(pushDone.rise())

          val fire = False
          val memRsp = False
          val memPending = Reg(UInt(log2Up(p.pendingWritePerChannel) bits)) init(0)
          val address = Reg(UInt(io.write.p.access.addressWidth bits))

          val addressBurstOffset = (address.resized & bytePerBurst)
          val commitFromBytePerBurst = addressBurstOffset + fifo.pop.bytes > bytePerBurst
          val bytesInBurst = commitFromBytePerBurst ? (bytePerBurst-addressBurstOffset) | (fifo.pop.bytes-1).resized // minus one
//          val bytesInBurstP1 = bytesInBurst + 1
          val request = valid && memory && (commitFromBytePerBurst || flush && fifo.pop.bytes =/= 0 ) && memPending =/= p.pendingWritePerChannel
          val bytesToSkip = Reg(UInt(log2Up(p.writeByteCount) bits))

          val decrBytes = fifo.pop.bytesDecr.newPort()


          memPending := memPending + U(fire) - U(memRsp)

          decrBytes := 0


          when(valid && memory && pushDone && memPending === 0 && fifo.pop.bytes === 0){
            popDone := True
          }

          when(start){
            popDone := False
            bytesToSkip := 0
            flush := False
          }
        }
      }

      when(valid && pop.popDone/* && !RegNext(start)*/){
        when(selfRestart && !stop){
          start := True
          if(cp.canWrite) pop.b2m.address := pop.b2m.address - bytes - 1
          if(cp.canRead)  push.m2b.address := push.m2b.address - bytes - 1
        } otherwise {
          done := True
        }
      }

      val s2b = new Area{
        val full = fifo.push.available < p.memory.bankCount || push.loadDone
      }

      fifo.push.available := fifo.push.available - Mux(push.memory, fifo.push.availableDecr, fifo.push.ptrIncr.value) + fifo.pop.ptrIncr.value


      val interrupts = new Area{
        val completion = new Interrupt(valid && pop.popDone)
      }


      when(start){
        fifo.push.ptr := 0
        fifo.push.available := fifo.words + 1
        fifo.pop.ptr := 0
        fifo.pop.bytes := 0
      }
    }

    val channels = for((ep, id) <- p.channels.zipWithIndex) yield new ChannelLogic(id)




    val s2b = for(portId <- 0 until p.inputs.size) yield new Area{
      val ps = p.inputs(portId)
      val channels = Core.this.channels.filter(_.cp.inputsPorts.contains(portId))

      def memoryPort = memory.ports.s2b(portId)

      def sink = io.inputs(portId)
      val bankPerBeat = sink.p.byteCount * 8 / p.memory.bankWidth

      val cmd = new Area {
        val throwUntilFirst = Reg(Bits(1 << ps.sinkWidth bits)) init(0)
        val channelsOh = B(channels.map(c => c.valid && !c.push.memory && c.push.s2b.portId === portId && c.push.s2b.sinkId === sink.sink))
        val noHit = !channelsOh.orR
        val channelsFull = B(channels.map(_.s2b.full))
        val sinkHalted = sink.throwWhen(noHit || throwUntilFirst(sink.sink)).haltWhen((channelsOh & channelsFull).orR)
        val used = Reg(Bits(ps.byteCount bits)) init(0)
        val maskNoSat = sinkHalted.mask & ~used
        val byteValidId = U(0) +: CountOneOnEach(maskNoSat.dropHigh(1))
        val byteLeft = MuxOH(channelsOh, channels.map(_.push.bytesLeft))
        val mask = B((0 until ps.byteCount).map(byteId => maskNoSat(byteId) && byteValidId(byteId) <= byteLeft))
        val byteInUse = CountOne(maskNoSat).min(byteLeft).resized(log2Up(ps.byteCount + 1))
        val context = InputContext(ps, portId)
        val byteCount = CountOne(mask)
        val veryLast = byteLeft < byteCount
        context.channel := channelsOh
        context.bytes := byteCount
        context.flush := sink.last || veryLast
        context.veryLast := veryLast
        sinkHalted.ready     := memoryPort.cmd.ready && mask === maskNoSat // Do not consume transactions when they are partialy used
        memoryPort.cmd.valid := sinkHalted.valid
        memoryPort.cmd.address := MuxOH(channelsOh, channels.map(_.fifo.push.ptrWithBase)).resized
        memoryPort.cmd.data := sinkHalted.data
        memoryPort.cmd.mask := mask
        memoryPort.cmd.priority := MuxOH(channelsOh, channels.map(_.priority))
        memoryPort.cmd.context := B(context)
        for ((channel, ohId) <- channels.zipWithIndex) {
          val hit = channelsOh(ohId) && memoryPort.cmd.fire
          channel.fifo.push.ptrIncr.newPort() := ((hit && memoryPort.cmd.mask.orR) ? U(bankPerBeat) | U(0)).resized
          when(hit) {
            channel.push.bytesLeft := byteLeft - byteCount
            channel.push.loadDone setWhen(veryLast)
          }
        }
        when(memoryPort.cmd.fire){
          used := used | memoryPort.cmd.mask
        }
        when(sink.ready){
          used := 0
        }

        when(sink.fire){
          when(noHit){
            throwUntilFirst(sink.sink) := True
          }
          when(sink.last) {
            throwUntilFirst(sink.sink) := False
          }
        }
      }

      val rsp = new Area{
        val context = memoryPort.rsp.context.as(InputContext(ps, portId))
        for ((channel, ohId) <- channels.zipWithIndex) {
          val hit = memoryPort.rsp.fire && context.channel(ohId)
          channel.fifo.pop.bytesIncr.newPort := (hit ? context.bytes | U(0)).resized
          if(channel.cp.canWrite) channel.pop.b2m.flush setWhen(hit && context.flush)
          channel.pop.pushDone setWhen(hit && context.veryLast)
        }
      }
    }

    val b2s = for(portId <- 0 until p.outputs.size) yield new Area{
      val channels = Core.this.channels.filter(_.cp.outputsPorts.contains(portId))

      def source = io.outputs(portId)
      val bankPerBeat = source.p.byteCount * 8 / p.memory.bankWidth
      def memoryPort = memory.ports.b2s(portId)

      val cmd = new Area{
        //TODO better arbitration
        val channelsOh = B(OHMasking.first(channels.map(c => c.valid &&  !c.pop.memory && c.pop.b2s.portId === portId && !c.fifo.pop.empty)))
        val context = B2sReadContext(portId)
        val groupRange = log2Up(p.readDataWidth/p.memory.bankWidth) -1 downto log2Up(bankPerBeat)
        val addressRange = ptrWidth-1 downto log2Up(p.readDataWidth/p.memory.bankWidth)
        val veryLastPtr = MuxOH(channelsOh, channels.map(_.pop.b2s.veryLastPtr))
        val address = MuxOH(channelsOh, channels.map(_.fifo.pop.ptrWithBase))
        context.channel := channelsOh
        context.veryLast :=  MuxOH(channelsOh, channels.map(_.pop.b2s.veryLastValid)) && address(addressRange) === veryLastPtr(addressRange) && address(groupRange) === (1 << groupRange.size)-1

        memoryPort.cmd.valid := channelsOh.orR
        memoryPort.cmd.address := address.resized
        memoryPort.cmd.context := B(context)
        memoryPort.cmd.priority := MuxOH(channelsOh, channels.map(_.priority))

        for ((channel, ohId) <- channels.zipWithIndex) {
          channel.fifo.pop.ptrIncr.newPort() := ((channelsOh(ohId) && memoryPort.cmd.ready) ? U(bankPerBeat) | U(0)).resized
        }
      }

      val rsp = new Area{
        val context = memoryPort.rsp.context.as(B2sReadContext(portId))
        source.arbitrationFrom(memoryPort.rsp)
        source.data   := memoryPort.rsp.data
        source.mask   := memoryPort.rsp.mask
        source.sink   := MuxOH(context.channel, channels.map(_.pop.b2s.sinkId))
        source.last   := context.veryLast && MuxOH(context.channel, channels.map(_.pop.b2s.last))

        when(source.fire) {
          for ((channel, ohId) <- channels.zipWithIndex) when(context.channel(ohId) && context.veryLast) {
            channel.pop.popDone := True
          }
        }
      }
    }


    val m2b = p.canRead generate new Area {
      val channels = Core.this.channels.filter(_.cp.canRead)
      val cmd = new Area {
        val arbiter = StreamArbiterFactory.roundRobin.noLock.build(NoData, channels.size)
        (arbiter.io.inputs, channels.map(_.push.m2b.loadRequest)).zipped.foreach(_.valid := _)
        arbiter.io.output.ready := False

        val valid = RegInit(False)
        val chosen = Reg(UInt(log2Up(channels.size) bits))

        when(!valid){
          valid := arbiter.io.inputs.map(_.valid).orR
          chosen := arbiter.io.chosen
          arbiter.io.output.ready := True
        }

        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(chosen)


        val address = channel(_.push.m2b.address)
        val bytesLeft = channel(_.push.bytesLeft)
        val readAddressBurstRange = address(io.read.p.access.lengthWidth-1 downto 0) //address(log2Up(io.read.p.access.byteCount) downto log2Up(io.read.p.access.byteCount))
        val lengthHead = ~readAddressBurstRange & channel(_.push.m2b.bytePerBurst)
        val length = lengthHead.min(bytesLeft).resize(io.read.p.access.lengthWidth)
        val lastBurst = bytesLeft === length

        val context = p.FetchContext()
        context.channel := chosen
        context.start := address.resized
        context.stop := (address + length).resized
        context.last := lastBurst

        io.read.cmd.valid := False
        io.read.cmd.last := True
        io.read.cmd.source := chosen
        io.read.cmd.opcode := Bmb.Cmd.Opcode.READ
        io.read.cmd.address := address & (address.maxValue-p.readDataWidth/8+1)
        io.read.cmd.length  := address(log2Up(io.read.p.access.byteCount)-1 downto 0) + length | p.readDataWidth/8-1
        io.read.cmd.context := B(context)



        val beatPerBurst = (length >> log2Up(p.readDataWidth/8)) + 1
        val bankPerBurst = beatPerBurst << log2Up(p.readDataWidth/p.memory.bankWidth)

        when(valid) {
          io.read.cmd.valid := True
          when(io.read.cmd.ready) {
            valid := False
            address := address + length + 1
            bytesLeft := bytesLeft - length - 1
            channel(_.fifo.push.availableDecr) := ((io.read.cmd.length + 1) >> log2Up(p.memory.bankWidth/8)).resized
            when(lastBurst) {
              channel(_.push.loadDone) := True
            }
          }
        }
      }

      val rsp = new Area {
        val context = io.read.rsp.context.as(p.FetchContext())

        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(context.channel)

        val veryLast = context.last && io.read.rsp.last
        when(io.read.rsp.fire && veryLast) {
          for((channel, ohId) <- channels.zipWithIndex; if channel.cp.canOutput) when(context.channel === ohId){
            channel.pop.b2s.veryLastTrigger := True
          }
        }

        val first = io.read.rsp.first
        val last = io.read.rsp.last
        for (byteId <- memory.ports.m2b.cmd.mask.range) {
          val toLow = first && byteId < context.start
          val toHigh = last && byteId > context.stop
          memory.ports.m2b.cmd.mask(byteId) := !toLow && !toHigh
        }

        val writeContext = M2bWriteContext()
        writeContext.last := veryLast
        writeContext.channel := context.channel
        writeContext.loadByteInNextBeat := ((last ? context.stop | context.stop.maxValue) -^ (first ? context.start | 0))
        memory.ports.m2b.cmd.address := channel(_.fifo.push.ptrWithBase).resized
        memory.ports.m2b.cmd.arbitrationFrom(io.read.rsp)
        memory.ports.m2b.cmd.data := io.read.rsp.data
        memory.ports.m2b.cmd.context := B(writeContext)


        for ((channel, ohId) <- channels.zipWithIndex) {
          val fire = memory.ports.m2b.cmd.fire && context.channel === ohId
          channel.fifo.push.ptrIncr.newPort := (fire ? U(io.read.p.access.dataWidth / p.memory.bankWidth) | U(0)).resized
        }

      }

      val writeRsp = new Area{
        val context = memory.ports.m2b.rsp.context.as(M2bWriteContext())
        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(context.channel)
        when(memory.ports.m2b.rsp.fire && context.last) {
          for((channel, ohId) <- channels.zipWithIndex) when(context.channel === ohId){
            channel.pop.pushDone := True
            if(channel.cp.canWrite) channel.pop.b2m.flush := True
          }
        }
        for ((channel, ohId) <- channels.zipWithIndex) {
          val fire = memory.ports.m2b.rsp.fire && context.channel === ohId
          channel.fifo.pop.bytesIncr.newPort := (fire ? (context.loadByteInNextBeat + 1) | U(0)).resized
        }
      }
    }


    val b2m = p.canWrite generate new Area {
      val channels = Core.this.channels.filter(_.cp.canWrite)

      val fsm = new Area {
        val sel = new Area {
          val valid = RegInit(False)
          val ready = Bool()
          val channel = Reg(UInt(log2Up(channels.size) bits))
          val bytesInBurst = Reg(UInt(io.write.p.access.lengthWidth bits))
          val address = Reg(UInt(p.writeAddressWidth bits))
          val ptr = Reg(ptrType())
          val ptrMask = Reg(ptrType())
          val commitFromBytePerBurst = Reg(Bool)
          def fire = valid && ready
          def isStall = valid && !ready
        }

        val arbiter = new Area {
          val core = StreamArbiterFactory.roundRobin.noLock.build(NoData, channels.size)
          (core.io.inputs, channels.map(_.pop.b2m.request)).zipped.foreach(_.valid := _)
          core.io.output.ready := False
          def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(core.io.chosen)
          when(sel.ready){
            sel.valid := False
          }
          when(!sel.valid && core.io.output.valid) {
            core.io.output.ready := True
            sel.valid := True
            sel.channel := core.io.chosen
            sel.bytesInBurst := channel(_.pop.b2m.bytesInBurst)
            sel.address := channel(_.pop.b2m.address)
            sel.ptr := channel(_.fifo.pop.ptrWithBase)
            sel.ptrMask := channel(_.fifo.words)
            sel.commitFromBytePerBurst := channel(_.pop.b2m.commitFromBytePerBurst)
            channel(_.pop.b2m.fire) := True
          }
        }

//        val sel = arbiter.sel.halfPipe()
        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(sel.channel)

        val bytesInBurstP1 = sel.bytesInBurst + 1
        val addressNext = sel.address + bytesInBurstP1
        when(sel.valid.rise(False)){
          for((channel, ohId) <- channels.zipWithIndex) when(sel.channel === ohId){
            channel.pop.b2m.decrBytes := bytesInBurstP1.resized
            channel.pop.b2m.address := addressNext
            when(!sel.commitFromBytePerBurst) {
              channel.pop.b2m.flush := False
            }
          }
        }


        val toggle = RegInit(False)
        toggle := toggle ^ sel.fire


        def address = sel.address

        case class FetchContext() extends Bundle {
          val ptr = ptrType()
          val toggle = Bool()
        }
        val fetch = new Area {
          sel.ready := False


          val context = FetchContext()
          context.ptr := channel(_.fifo.pop.ptr)
          context.toggle := toggle

          memory.ports.b2m.cmd.valid := sel.valid
          memory.ports.b2m.cmd.address := sel.ptr.resized
          memory.ports.b2m.cmd.context := B(context)

          for ((channel, ohId) <- channels.zipWithIndex) {
            when(sel.valid && sel.channel === ohId && memory.ports.b2m.cmd.ready) {
              sel.ptr.getDrivingReg := (sel.ptr & ~sel.ptrMask) | ((sel.ptr + U(io.write.p.access.dataWidth / p.memory.bankWidth) & sel.ptrMask))
            }
          }
        }


        val aggregate = new Area {
          val context = memory.ports.b2m.rsp.context.as(FetchContext())
          val memoryPort = memory.ports.b2m.rsp.throwWhen(context.toggle =/= toggle)


          val engine = Aggregator(AggregatorParameter(
            byteCount = p.writeByteCount,
            burstLength = p.writeLengthWidth,
            context = NoData
          ))

          val first = Reg(Bool) clearWhen(memoryPort.fire) setWhen(!sel.isStall)
          val bytesToSkip = channel(_.pop.b2m.bytesToSkip)
          val bytesToSkipMask = B((0 until p.writeByteCount).map(byteId => !first || byteId >= bytesToSkip))
          engine.io.input.arbitrationFrom(memoryPort)
          engine.io.input.data := memoryPort.data
          engine.io.input.mask := memoryPort.mask & bytesToSkipMask
          engine.io.offset := sel.address.resized
          engine.io.burstLength := sel.bytesInBurst
          engine.io.flush := ! (RegNext(sel.isStall) init(False))
        }


        val cmd = new Area {

          def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(sel.channel)

          val beatCounter = Reg(UInt(io.write.p.access.beatCounterWidth bits))

          val maskFirstTrigger = address.resize(log2Up(p.writeByteCount) bits)
          val maskLastTrigger = maskFirstTrigger + sel.bytesInBurst.resized
          val maskLast = B((0 until p.writeByteCount).map(byteId => byteId <= maskLastTrigger))
          val maskFirst = B((0 until p.writeByteCount).map(byteId => byteId >= maskFirstTrigger))
          val enoughAggregation = sel.valid && !aggregate.engine.io.flush && (io.write.cmd.last ? ((aggregate.engine.io.output.mask & maskLast) === maskLast) | aggregate.engine.io.output.mask.andR)

          aggregate.engine.io.output.enough := enoughAggregation
          aggregate.engine.io.output.consume := io.write.cmd.fire
          aggregate.engine.io.output.lastByteUsed := maskLastTrigger
          io.write.cmd.valid := enoughAggregation
          io.write.cmd.last := beatCounter === io.write.cmd.length >> log2Up(io.write.p.access.byteCount)
          io.write.cmd.address := address & (address.maxValue-p.readDataWidth/8+1)
          io.write.cmd.opcode := Bmb.Cmd.Opcode.WRITE
          io.write.cmd.data := aggregate.engine.io.output.data
          io.write.cmd.mask := ~((io.write.cmd.first ? ~maskFirst | B(0)) | (io.write.cmd.last ? ~maskLast| B(0)))
          io.write.cmd.length := address(log2Up(io.write.p.access.byteCount)-1 downto 0) + sel.bytesInBurst | p.readDataWidth/8-1
          io.write.cmd.source := sel.channel

          for((channel, ohId) <- channels.zipWithIndex){
            channel.fifo.pop.ptrIncr.newPort() := ((sel.valid  && sel.channel === ohId && (aggregate.engine.io.output.consumed || io.write.cmd.lastFire && aggregate.engine.io.output.usedUntil === aggregate.engine.io.output.usedUntil.maxValue)) ? U(io.write.p.access.dataWidth/p.memory.bankWidth) | U(0)).resized
          }

          val context = p.WriteContext()
          context.channel := sel.channel
          io.write.cmd.context := B(context)

          when(io.write.cmd.fire){
            beatCounter := beatCounter + 1
          }
          when(io.write.cmd.lastFire || !sel.valid){
            beatCounter := 0
          }

          when(io.write.cmd.lastFire){
            sel.ready := True
            channel(_.pop.b2m.bytesToSkip) := aggregate.engine.io.output.usedUntil + 1
          }
        }
      }



      val rsp = new Area{
        io.write.rsp.ready := True
        val context = io.write.rsp.context.as(p.WriteContext())
        when(io.write.rsp.fire){
          channels.map(_.pop.b2m.memRsp).write(context.channel, True)
        }
      }
    }

    io.interrupts := 0
    val mapping = new Area{
      for(channel <- channels){
        val a = 0x800+channel.id*0x40

        if(channel.cp.canRead)  ctrl.writeMultiWord(channel.push.m2b.address, a+0x00)
        if(channel.cp.canInput) ctrl.write(channel.push.s2b.portId,           a+0x08, 0)
        if(channel.cp.canInput) ctrl.write(channel.push.s2b.sinkId,           a+0x08, 16)
        if(channel.cp.canRead)  ctrl.write(channel.push.m2b.bytePerBurst,     a+0x0C, 0)
        ctrl.write(channel.push.memory,           a+0x0C, 12)

        if(channel.cp.canWrite)  ctrl.writeMultiWord(channel.pop.b2m.address, a+0x10)
        if(channel.cp.canOutput) ctrl.write(channel.pop.b2s.portId,           a+0x18, 0)
        if(channel.cp.canOutput) ctrl.write(channel.pop.b2s.sinkId,           a+0x18, 16)
        if(channel.cp.canWrite)  ctrl.write(channel.pop.b2m.bytePerBurst,     a+0x1C, 0)
        ctrl.write(channel.pop.memory,           a+0x1C, 12)
        if(channel.cp.canOutput) ctrl.write(channel.pop.b2s.last,             a+0x1C, 13)

        ctrl.write(channel.bytes, a+0x20, 0)
        ctrl.setOnSet(channel.start, a+0x2C, 0)
        ctrl.read(channel.valid, a+0x2C, 0)
        ctrl.write(channel.selfRestart, a + 0x2C, 1)
        ctrl.write(channel.stop, a + 0x2C, 2)

        ctrl.write(channel.fifo.base, a+0x30, log2Up(p.memory.bankWidth/8))
        ctrl.write(channel.fifo.words, a+0x30, 16 + log2Up(p.memory.bankWidth/8))
        ctrl.write(channel.priority, a+0x34, 0)
//        ctrl.write(channel.priority, a+0x3C, 0)

        def map(interrupt : Interrupt, id : Int): Unit ={
          ctrl.write(interrupt.enable, a+0x38, id)
          ctrl.clearOnSet(interrupt.valid, a+0x3C, id)
          io.interrupts(channel.id) setWhen(interrupt.valid)
        }

        map(channel.interrupts.completion, 0)
      }
    }



  }


  case class AggregatorParameter[T <: Data](byteCount : Int, burstLength : Int, context : HardType[T])
  case class AggregatorCmd[T <: Data](p : AggregatorParameter[T]) extends Bundle{
    val data = Bits(p.byteCount*8 bits)
    val mask = Bits(p.byteCount bits)
    val context = p.context()
  }
  case class AggregatorRsp[T <: Data](p : AggregatorParameter[T]) extends Bundle{
    val data = out Bits(p.byteCount*8 bits)
    val mask = out Bits(p.byteCount bits)
    val enough = in Bool()
    val consume = in Bool()
    val context = out (p.context())
    val consumed = out Bool()
    val lastByteUsed = in UInt(log2Up(p.byteCount) bits)
    val usedUntil = out UInt(log2Up(p.byteCount) bits)
  }
  case class Aggregator[T <: Data](p : AggregatorParameter[T]) extends Component {
    val io = new Bundle {
      val input = slave Stream(AggregatorCmd(p))
      val output = AggregatorRsp(p)
      val flush = in Bool()
      val offset = in UInt(log2Up(p.byteCount) bits)
      val burstLength = in UInt(p.burstLength bits)
    }


    val s0 = new Area{
      val countOnes = CountOneOnEach(io.input.mask)

      val offset = Reg(UInt(log2Up(p.byteCount) bits))
      val offsetNext = offset + countOnes.last
      when(io.input.fire){
        offset := offsetNext.resized
      }
      when(io.flush){
        offset := io.offset
      }

      val byteCounter = Reg(UInt(p.burstLength + 1 bits))
      when(io.input.fire){
        byteCounter := byteCounter + countOnes.last
      }
      when(io.flush){
        byteCounter := 0
      }

      val inputIndexes = Vec((U(0) +: countOnes.dropRight(1)).map(_ + offset))
      case class S0Output() extends Bundle{
        val cmd = AggregatorCmd(p)
        val index = cloneOf(inputIndexes)
        val last = Bool()
      }
      val outputPayload = S0Output()
      outputPayload.cmd := io.input.payload
      outputPayload.index := inputIndexes
      outputPayload.last := offsetNext.msb
      val output = io.input.translateWith(outputPayload)
    }

    val s1 = new Area{
      val input = s0.output.m2sPipe(flush = io.flush)
      input.ready := !io.output.enough || io.output.consume
      when(s0.byteCounter > io.burstLength){
        input.ready := False
      }

      io.output.consumed := input.fire
      io.output.context := input.cmd.context

      val inputDataBytes = input.cmd.data.subdivideIn(8 bits)
      val byteLogic = for(byteId <- 0 until p.byteCount) yield new Area{
        val buffer = new Area{
          val valid = Reg(Bool)
          val data = Reg(Bits(8 bits))
        }
        val selOh = (0 until p.byteCount).map(inputId => input.cmd.mask(inputId) && input.index(inputId) === byteId)
        val sel = OHToUInt(selOh)
        val lastUsed = byteId === io.output.lastByteUsed
        val inputMask = (B(selOh) & input.cmd.mask).orR
        val inputData = MuxOH(selOh, inputDataBytes)
        val outputMask = buffer.valid || (input.valid && inputMask)
        val outputData = buffer.valid ? buffer.data | inputData

        io.output.mask(byteId) := outputMask
        io.output.data(byteId*8, 8 bits) := outputData
        when(io.output.consume){
          buffer.valid := False
        }
        when(input.fire){
          when(input.last){
            buffer.valid := False
          }
          when(inputMask && (!io.output.consume || buffer.valid)) {
            buffer.valid := True
            buffer.data := inputData
          }
        }
        when(io.flush){
          buffer.valid := byteId < io.offset //might be not necessary
        }
      }

      io.output.usedUntil := MuxOH(byteLogic.map(_.lastUsed), byteLogic.map(_.sel))
    }
  }
}


abstract class DmaSgTester(p : DmaSg.Parameter,
                           clockDomain : ClockDomain,
                           inputsIo : Seq[Bsb],
                           outputsIo : Seq[Bsb],
                           interruptsIo : Bits,
                           memory : SparseMemory) {

  import spinal.core.sim._

  val writesAllowed = mutable.HashMap[Long, (Byte, Int)]()

  def writeNotification(address: Long, value: Byte): Unit = {
    val e = writesAllowed(address)
    assert(writesAllowed(address)._1 == value)
    writesAllowed(address) = ((e._1, e._2 + 1))
  }

  val memoryReserved = MemoryRegionAllocator(base = 0, size = 1l << p.writeAddressWidth)

  val outputs = for(outputId <- 0 until p.outputs.size) yield new {
    val readyDriver = StreamReadyRandomizer(outputsIo(outputId), clockDomain)
    val ref = Array.fill(1 << p.outputs(outputId).sinkWidth)(mutable.Queue[(Int, Int, Boolean)]())
    val monitor = new BsbMonitor(outputsIo(outputId), clockDomain) {
      override def onByte(value: Byte, source: Int, sink: Int): Unit = {
        val e = ref(sink).dequeue()
        assert(value == e._1)
        assert(source == e._2)
        assert(!e._3)
      }

      override def onLast(source: Int, sink: Int): Unit = {
        val e = ref(sink).dequeue()
        assert(source == e._2)
        assert(e._3)
      }
    }
    val reservedSink = mutable.HashSet[Int]()
  }
  case class Packet(source : Int, sink : Int, last : Boolean){
    val data = mutable.Queue[Int]()
    var done = false
  }
  val inputs = for(inputId <- 0 until p.inputs.size) yield new {
    val ip = p.inputs(inputId)

    val packets = ArrayBuffer[Packet]()
    val driver = StreamDriver(inputsIo(inputId), clockDomain) { p =>
      if (packets.isEmpty) {
        false
      } else {
        val packet = packets.randomPick()
        var data = BigInt(0)
        var mask = BigInt(0)
        for (byteId <- 0 until ip.byteCount) if(packet.data.nonEmpty && Random.nextBoolean()){
          data |= BigInt(packet.data.dequeue()) << byteId * 8
          mask |= 1 << byteId
        }
        p.data #= data
        p.mask #= mask
        p.source #= packet.source
        p.sink #= packet.sink
        if(packet.data.isEmpty && Random.nextBoolean()) {
          p.last #= packet.last
          packet.done = true
          packets.remove(packets.indexOf(packet))
        } else {
          p.last #= false
        }
        true
      }
    }
    val reservedSink = mutable.HashSet[Int]()
  }

  periodicaly(10*1000){
    outputs.foreach(_.readyDriver.factor = Random.nextFloat)
  }

  val mutex = SimMutex()

  def ctrlWriteHal(data : BigInt, address : BigInt): Unit
  def ctrlReadHal(address : BigInt): BigInt

  def ctrlWrite(data : BigInt, address : BigInt): Unit ={
    mutex.lock()
    ctrlWriteHal(data,address)
    mutex.unlock()
  }

  def ctrlRead(address : BigInt): BigInt ={
    mutex.lock()
    val ret = ctrlReadHal(address)
    mutex.unlock()
    ret
  }


  def channelToAddress(channel : Int) = 0x800 + channel*0x40
  def channelPushMemory(channel : Int, address : BigInt, bytePerBurst : Int): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(address, channelAddress + 0x00)
    ctrlWrite(bytePerBurst-1 | 1 << 12, channelAddress + 0x0C)
  }
  def channelPopMemory(channel : Int, address : BigInt, bytePerBurst : Int): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(address, channelAddress + 0x10)
    ctrlWrite(bytePerBurst-1 | 1 << 12, channelAddress + 0x1C)
  }
  def channelPushStream(channel : Int, portId : Int, sourceId : Int, sinkId : Int): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(portId << 0 | sourceId << 8 | sinkId << 16, channelAddress + 0x08)
    ctrlWrite(0, channelAddress + 0x0C)
  }
  def channelPopStream(channel : Int, portId : Int, sourceId : Int, sinkId : Int, withLast : Boolean): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(portId << 0 | sourceId << 8 | sinkId << 16, channelAddress + 0x18)
    ctrlWrite(if(withLast) 1 << 13 else 0, channelAddress + 0x1C)
  }
  def channelStart(channel : Int, bytes : BigInt, selfRestart : Boolean): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(bytes-1, channelAddress + 0x20)
    ctrlWrite(1 + (if(selfRestart) 2 else 0), channelAddress+0x2C)
  }
  def channelConfig(channel : Int,
                    fifoBase : Int,
                    fifoBytes : Int,
                    priority : Int): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(fifoBase << 0 | fifoBytes-1 << 16,  channelAddress+0x30)
    ctrlWrite(priority,  channelAddress+0x34)
  }
  def channelInterruptConfigure(channel : Int, mask : Int): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(mask, channelAddress+0x3C)
    ctrlWrite(mask, channelAddress+0x38)
  }
  def channelStartAndWait(channel : Int, bytes : BigInt, doCount : Int): Unit ={
    val channelAddress = channelToAddress(channel)
    if(Random.nextBoolean() && doCount == 1){
      //By pulling
      channelStart(channel, bytes, doCount != 1)
      while ((ctrlRead(channelAddress + 0x2C) & 1) != 0) {
        clockDomain.waitSampling(Random.nextInt(50))
      }
    } else {
      //By interrupt
      channelInterruptConfigure(channel, 1)
      fork{
        clockDomain.waitSampling(Random.nextInt(10))
        channelStart(channel, bytes, doCount != 1)
      }
      doCount match {
        case 1 => {
          waitUntil((interruptsIo.toInt & 1 << channel) != 0)
          assert((ctrlRead(channelAddress + 0x2C) & 1) == 0)
        }
        case _ => {
          for(i <- 0 until doCount) {
            waitUntil((interruptsIo.toInt & 1 << channel) != 0)
            channelInterruptConfigure(channel, 1)
          }

          ctrlWrite(4, channelAddress + 0x2c)
          while ((ctrlRead(channelAddress + 0x2C) & 1) != 0) {
            clockDomain.waitSampling(Random.nextInt(50))
          }
        }
      }
    }
  }
  def channelStarted(channel : Int) ={
    val channelAddress = channelToAddress(channel)
    (ctrlRead(channelAddress + 0x2C) & 1) != 0
  }

  val inputsTrasher = if(inputsIo.nonEmpty) for(_ <- 0 until 3) yield fork{
    clockDomain.waitSampling(Random.nextInt(4000))
    val inputId = Random.nextInt(p.inputs.size)
    val ip = p.inputs(inputId)
    val sink = Random.nextInt(1 << ip.sinkWidth)
    val source = Random.nextInt(1 << ip.sourceWidth)

    while(inputs(inputId).reservedSink.contains(sink)) clockDomain.waitSampling(Random.nextInt(100))
    inputs(inputId).reservedSink.add(sink)
    val packet = Packet(source = source, sink = sink, last = true)
    val datas = ArrayBuffer[Int]()
    for (byteId <- 0 until Random.nextInt(100)) {
      val value = Random.nextInt & 0xFF
      packet.data += value
    }
    inputs(inputId).packets += packet

    waitUntil(packet.done)
    inputs(inputId).reservedSink.remove(sink)
  }
  val channelAgent = for((channel, channelId) <- p.channels.zipWithIndex) yield fork {
    val cp = p.channels(channelId)
    val M2M, M2S, S2M = new Object
    var tests = ArrayBuffer[Object]()
    if(cp.memoryToMemory)        tests += M2M
    if(cp.outputsPorts.nonEmpty) tests += M2S
    if(cp.inputsPorts.nonEmpty)  tests += S2M
    for (r <- 0 until 1000) {
      clockDomain.waitSampling(Random.nextInt(100))
      tests.randomPick() match {
        case S2M => if (p.inputs.nonEmpty) {
          val doCount = Random.nextInt(3) + 1
          val bytes = (Random.nextInt(0x100) + 1)
          val to = memoryReserved.allocate(size = bytes)
//          val doCount = 1
//          val bytes = 0x40
//          val to = SizeMapping(0x1000, bytes)
          val inputId = cp.inputsPorts.randomPick()
          val ip = p.inputs(inputId)
          val source = Random.nextInt(1 << ip.sourceWidth)
          val sink = Random.nextInt(1 << ip.sinkWidth)

          while(inputs(inputId).reservedSink.contains(sink)) clockDomain.waitSampling(Random.nextInt(100))
          inputs(inputId).reservedSink.add(sink)

          channelPushStream(channelId, inputId, source, sink)
          channelPopMemory(channelId, to.base.toInt, 16)
          channelConfig(channelId, 0x100 + 0x40*channelId, 0x40, 2)
          val packet = Packet(source = source, sink = sink, last = false)
          fork{
            while(!channelStarted(channelId)){
              clockDomain.waitSampling(Random.nextInt(5))
            }
            val datas = ArrayBuffer[Int]()
            for (byteId <- 0 until bytes) {
              val value = Random.nextInt & 0xFF
              writesAllowed(to.base.toInt + byteId) = ((value.toByte, 0))
              datas += value
            }
            for(_ <- 0 until doCount + 20) {
              packet.data ++= datas
            }
            inputs(inputId).packets += packet
          }
          channelStartAndWait(channelId, bytes, doCount)
          for (byteId <- 0 until bytes) {
            assert(writesAllowed.remove(to.base.toInt + byteId).get._2 >= doCount)
          }
          if(!packet.done) inputs(inputId).packets.remove(inputs(inputId).packets.indexOf(packet))

          val packetFlush = Packet(source = source, sink = sink, last = true)
          for (byteId <- 0 until Random.nextInt(10)) {
            packetFlush.data += Random.nextInt & 0xFF
          }
          inputs(inputId).packets += packetFlush

          waitUntil(packetFlush.done)
          inputs(inputId).reservedSink.remove(sink)
        }
        case M2M => {
          val bytes = (Random.nextInt(0x100) + 1)
          val from = memoryReserved.allocate(size = bytes)
          val to = memoryReserved.allocate(size = bytes)
          val doCount = Random.nextInt(3) + 1
          //            val from = 0x100 + Random.nextInt(4)
          //            val to = 0x400 + Random.nextInt(4)
          //            val bytes = 0x40 + Random.nextInt(4)
          //            val bytes = 0x40
          //            val from = SizeMapping(0x1000 + 0x100*channelId, bytes)
          //            val to = SizeMapping(0x2000 + 0x100*channelId, bytes)
          //            val doCount = 1
          //            println(s"$from $to $bytes")
          for (byteId <- 0 until bytes) {
            writesAllowed(to.base.toInt + byteId) = Tuple2(memory.read(from.base.toInt + byteId),0)
          }

          channelPushMemory(channelId, from.base.toInt, 16)
          channelPopMemory(channelId, to.base.toInt, 16)
          channelConfig(channelId, 0x100 + 0x40*channelId, 0x40, 2)
          channelStartAndWait(channelId, bytes, doCount)

          for (byteId <- 0 until bytes) {
            assert(writesAllowed.remove(to.base.toInt + byteId).get._2 >= doCount)
          }
        }
        case M2S => if (p.outputs.nonEmpty) {
          val doCount = Random.nextInt(3) + 1
          val bytes = (Random.nextInt(0x100) + 1)
          val from = memoryReserved.allocate(size = bytes)
          val outputId = cp.outputsPorts.randomPick()

//          val doCount = 1
//          val bytes = 0x40
//          val from =  SizeMapping(0x1000, bytes)
//          val outputId = 0

          val op = p.outputs(outputId)
          val source = Random.nextInt(1 << op.sourceWidth)
          val sink = Random.nextInt(1 << op.sinkWidth)
          val withLast = Random.nextBoolean()

          while(outputs(outputId).reservedSink.contains(sink)) clockDomain.waitSampling(Random.nextInt(100))
          outputs(outputId).reservedSink.add(sink)

          for(_ <- 0 until doCount + 4) {
            for (byteId <- 0 until bytes) {
              outputs(outputId).ref(sink).enqueue((memory.read(from.base.toInt + byteId), source, false))
            }
            if (withLast) outputs(outputId).ref(sink).enqueue((0, source, true))
          }

          channelPushMemory(channelId, from.base.toInt, 16)
          channelPopStream(channelId, outputId, source, sink, withLast)
          channelConfig(channelId, 0x100 + 0x40*channelId, 0x40, 2)
          channelStartAndWait(channelId, bytes = bytes, doCount)
          clockDomain.waitSampling(2)
          waitUntil(!outputsIo(outputId).valid.toBoolean)
          assert(outputs(outputId).ref(sink).size <= 4*(bytes + (if(withLast) 1 else 0)))
          outputs(outputId).ref(sink).clear()
          outputs(outputId).reservedSink.remove(sink)
        }
      }
    }
  }

  def waitCompletion() {
    channelAgent.foreach(_.join())
    clockDomain.waitSampling(1000)
  }
}


object SgDmaTestsParameter{
  def apply() : Seq[(String, DmaSg.Parameter)] = {
    val parameters = ArrayBuffer[(String, DmaSg.Parameter)]()

    for(withMemoryToMemory <- List(true, false);
        withOutputs <- List(true, false);
        withInputs <- List(true, false);
        if withMemoryToMemory || withOutputs || withInputs){

      var name = ""
      if(withMemoryToMemory) name = name + "M2m"
      if(withOutputs) name = name + "M2s"
      if(withInputs) name = name + "S2m"

      val outputs = if(!withOutputs) Nil else Seq(
        BsbParameter(
          byteCount   = 4,
          sourceWidth = 0,
          sinkWidth   = 4
        ),
        BsbParameter(
          byteCount   = 4,
          sourceWidth = 0,
          sinkWidth   = 4
        ),
        BsbParameter(
          byteCount   = 2,
          sourceWidth = 0,
          sinkWidth   = 4
        ),
        BsbParameter(
          byteCount   = 2,
          sourceWidth = 0,
          sinkWidth   = 4
        )
      )

      val inputs = if(!withInputs) Nil else Seq(
        BsbParameter(
          byteCount   = 4,
          sourceWidth = 0,
          sinkWidth   = 4
        ),
        BsbParameter(
          byteCount   = 4,
          sourceWidth = 0,
          sinkWidth   = 4
        ),
        BsbParameter(
          byteCount   = 2,
          sourceWidth = 0,
          sinkWidth   = 4
        ),
        BsbParameter(
          byteCount   = 2,
          sourceWidth = 0,
          sinkWidth   = 4
        )
      )

      val channels = ArrayBuffer[Channel]()
      channels += DmaSg.Channel(
        memoryToMemory = withMemoryToMemory,
        inputsPorts    = inputs.zipWithIndex.map(_._2),
        outputsPorts   = outputs.zipWithIndex.map(_._2)
      )
      if(withOutputs) channels += DmaSg.Channel(
        memoryToMemory = false,
        inputsPorts    = Nil,
        outputsPorts   = outputs.zipWithIndex.map(_._2)
      )
      if(withInputs) channels += DmaSg.Channel(
        memoryToMemory = false,
        inputsPorts    = inputs.zipWithIndex.map(_._2),
        outputsPorts   = Nil
      )
      if(withMemoryToMemory) channels += DmaSg.Channel(
        memoryToMemory = true,
        inputsPorts    = Nil,
        outputsPorts   = Nil
      )
      channels += DmaSg.Channel(
        memoryToMemory = withMemoryToMemory,
        inputsPorts    = inputs.zipWithIndex.map(_._2),
        outputsPorts   = outputs.zipWithIndex.map(_._2)
      )

      parameters += name -> Parameter(
        readAddressWidth  = 30,
        readDataWidth     = 32,
        readLengthWidth   = 6,
        writeAddressWidth = 30,
        writeDataWidth    = 32,
        writeLengthWidth  = 6,
        memory = DmaMemoryLayout(
          //      bankCount            = 1,
          //      bankWidth            = 32,
          bankCount            = 2,
          bankWidth            = 16,
          //      bankCount            = 4,
          //      bankWidth            = 8,
          //      bankCount            = 2,
          //      bankWidth            = 32,
          bankWords            = 256,
          priorityWidth        = 2
        ),
        outputs = outputs,
        inputs = inputs,
        channels = channels,
        bytePerTransferWidth = 16
      )
    }
    parameters
  }
}



//
//
//object SgDmaSynt extends App{
//  val p = Parameter(
//    readAddressWidth  = 30,
//    readDataWidth     = 32,
//    readLengthWidth   = 6,
//    writeAddressWidth = 30,
//    writeDataWidth    = 32,
//    writeLengthWidth  = 6,
//    memory = DmaMemoryLayout(
//      //      bankCount            = 1,
//      //      bankWidth            = 32,
//      bankCount            = 1,
//      bankWidth            = 32,
//      //      bankCount            = 4,
//      //      bankWidth            = 8,
//      //      bankCount            = 2,
//      //      bankWidth            = 32,
//      bankWords            = 256,
//      priorityWidth        = 2
//    ),
//    outputs = Seq(
//      BsbParameter(
//        byteCount   = 4,
//        sourceWidth = 0,
//        sinkWidth   = 4
//      )/*,
//      BsbParameter(
//        byteCount   = 4,
//        sourceWidth = 0,
//        sinkWidth   = 4
//      ),
//      BsbParameter(
//        byteCount   = 2,
//        sourceWidth = 0,
//        sinkWidth   = 4
//      ),
//      BsbParameter(
//        byteCount   = 2,
//        sourceWidth = 0,
//        sinkWidth   = 4
//      )*/
//    ),
//    inputs = Seq(
//      BsbParameter(
//        byteCount   = 4,
//        sourceWidth = 0,
//        sinkWidth   = 4
//      )/*,
//      BsbParameter(
//        byteCount   = 4,
//        sourceWidth = 0,
//        sinkWidth   = 4
//      ),
//      BsbParameter(
//        byteCount   = 2,
//        sourceWidth = 0,
//        sinkWidth   = 4
//      ),
//      BsbParameter(
//        byteCount   = 2,
//        sourceWidth = 0,
//        sinkWidth   = 4
//      )*/
//    ),
//    channels = Seq(
//      DmaSg.Channel(
//        memoryToMemory = true,
//        inputsPorts    = inputs.zipWithIndex.map(_._2),
//        outputsPorts   = outputs.zipWithIndex.map(_._2)
//      )/*,
//      DmaSg.Channel(
//
//      ),
//      DmaSg.Channel(
//
//      )*/
//    ),
//    bytePerTransferWidth = 24
//  )
//  val pCtrl = BmbParameter(
//    addressWidth = 12,
//    dataWidth    = 32,
//    sourceWidth  = 0,
//    contextWidth = 4,
//    lengthWidth  = 2
//  )
// SpinalVerilog(new DmaSg.Core[Bmb](p, ctrlType = HardType(Bmb(pCtrl)), BmbSlaveFactory(_)))
//}


