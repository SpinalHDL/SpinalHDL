package spinal.lib.system.dma.sg

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.bus.bmb.sim.{BmbDriver, BmbMemoryAgent}
import spinal.lib.bus.bsb.{BsbParameter, _}
import spinal.lib.bus.bsb.sim.BsbMonitor
import spinal.lib.sim.{MemoryRegionAllocator, SparseMemory, StreamDriver, StreamReadyRandomizer}
import spinal.lib.system.dma.sg.DmaSg.Parameter
import spinal.lib.system.dma.sg.DmaSgGen.{Aggregator, AggregatorParameter}

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
  ).addSources(p.channels.size, BmbSourceParameter(
    contextWidth = widthOf(p.FetchContext()),
    lengthWidth = p.readLengthWidth,
    canWrite = false
  ))

  def getWriteRequirements(p : Parameter) = BmbAccessParameter(
    addressWidth = p.writeAddressWidth,
    dataWidth    = p.writeDataWidth
  ).addSources(p.channels.size, BmbSourceParameter(
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

    case class FetchContext() extends Bundle{
      val channel = UInt(log2Up(channels.size) bits)
      val start, stop = UInt(log2Up(readDataWidth/8) bits)
      val last = Bool()
    }

    case class WriteContext() extends Bundle{
      val channel = UInt(log2Up(channels.size) bits)
    }
  }



  case class Channel()

  case class ChannelIo(p : Channel) extends Bundle{
    val interrupt = out Bool()
  }

  case class Core(p : Parameter, ctrlParameter : BmbParameter) extends Component{
    val io = new Bundle {
      val read = master(Bmb(getReadRequirements(p)))
      val write = master(Bmb(getWriteRequirements(p)))
      val outputs = Vec(p.outputs.map(s => master(Bsb(s))))
      val inputs = Vec(p.inputs.map(s => slave(Bsb(s))))
      val interrupts = out Bits(p.channels.size bits)

      val ctrl = slave(Bmb(ctrlParameter))
    }

    val ctrl = BmbSlaveFactory(io.ctrl)

    val ptrWidth = log2Up(p.memory.bankWords*p.memory.bankCount) + 1
    val ptrType = HardType(UInt(ptrWidth bits))

    case class InputContext(ip : BsbParameter) extends Bundle {
      val channel = Bits(p.channels.size bits)
      val bytes = UInt(log2Up(ip.byteCount + 1) bits)
      val flush = Bool()
      val veryLast = Bool()
    }
    case class M2sWriteContext() extends Bundle{
      val last = Bool()
      val channel = UInt(log2Up(p.channels.size) bits)
      val loadByteInNextBeat = UInt(log2Up(p.readDataWidth/8 + 1) bits)
    }

    val memory = new Area{
      val core = DmaMemoryCore(DmaMemoryCoreParameter(
        layout = p.memory,
        writes = p.inputs.map(p => DmaMemoryCoreWriteParameter(p.byteCount, widthOf(InputContext(p))))   :+ DmaMemoryCoreWriteParameter(io.read.p.access.byteCount, widthOf(M2sWriteContext())),
        reads  = p.outputs.map(p => DmaMemoryCoreReadParameter(p.byteCount, Core.this.p.channels.size + 1))  :+ DmaMemoryCoreReadParameter(io.write.p.access.byteCount, ptrWidth + 1)
      ))
      val ports = new Area{
        val m2b = core.io.writes.last
        val b2m = core.io.reads.last
        val s2b = core.io.writes.dropRight(1)
        val b2s = core.io.reads.dropRight(1)
      }
    }

    class Interrupt(fire : Bool) extends Area{
      val enable = Reg(Bool) init(False)
      val valid = Reg(Bool) init(False) setWhen(enable && fire)
    }

    class ChannelLogic(val id : Int) extends Area{
      val start = False
      val done = False
      val valid = RegInit(False) setWhen(start) clearWhen(done)
      val bytes = Reg(UInt(p.bytePerTransferWidth bits)) //minus one
      val priority = Reg(UInt(p.memory.priorityWidth bits))


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
//          val usedBytes = Reg(Bits(p.write.byteCount bits))

          val bytesIncr = DataOr(bytesType)
          val bytesDecr = DataOr(bytesType)
          bytes := bytes + bytesIncr.value - bytesDecr.value


          val commit = new Area{
            val valid = RegInit(False)
            val ptr = Reg(ptrType)
//            when(flush.value){
//              valid := True
//              ptr :=
//            }
          }

          val empty = ptr === push.ptr


          val ptrIncr = DataOr(ptrType)
          ptr := ptr + ptrIncr.value



        }

        val empty = push.ptr === pop.ptr

      }


      val push = new Area{
        val memory  = Reg(Bool)
        val address = Reg(UInt(io.read.p.access.addressWidth bits))
        val bytePerBurst = Reg(UInt(io.read.p.access.lengthWidth bits)) //minus one
        val bytesLeft = Reg(UInt(p.bytePerTransferWidth bits)) //minus one

        val portId = Reg(UInt(log2Up(p.inputs.size) bits))
        val sinkId = Reg(UInt(p.inputsSinkWidth bits))

        val loadDone = RegInit(True)
        val loadRequest = !loadDone && memory && fifo.push.available > (bytePerBurst >> log2Up(p.memory.bankWidth/8))
        when(start){
          bytesLeft := bytes
          loadDone := False
        }

      }

      val pop = new Area{
        val memory  = Reg(Bool)
        val last  = Reg(Bool)
        val address = Reg(UInt(io.write.p.access.addressWidth bits))
        val bytePerBurst = Reg(UInt(io.write.p.access.lengthWidth bits))
        val pushDone = Reg(Bool) clearWhen(start)
        val popDone = Reg(Bool) clearWhen(start)
        val veryLastTrigger = False
        val veryLastValid = Reg(Bool) setWhen(veryLastTrigger) clearWhen(start)
        val veryLastPtr = Reg(ptrType)
        when(veryLastTrigger){
          veryLastPtr := fifo.push.ptrWithBase
        }

        when(valid && popDone){
          done := True
        }

        val portId = Reg(UInt(log2Up(p.outputs.size) bits))
        val sinkId = Reg(UInt(p.outputsSinkWidth bits))
//        val commitValid = RegInit(False)
//        val commitBytes = Reg(UInt(io.write.p.access.lengthWidth bits)) // minus one
//        when(valid) {
//          when(bytes >= bytePerBurst) {
//            commitBytes := bytePerBurst
//          }
//        }

        val flush = Reg(Bool) setWhen(pushDone.rise())

        val arbiter = new Area{
          val fire = False
          val memRsp = False
          val memPending = Reg(UInt(log2Up(p.pendingWritePerChannel) bits)) init(0)

          val addressBurstOffset = (address.resized & bytePerBurst)
          val commitFromBytePerBurst = addressBurstOffset + fifo.pop.bytes > bytePerBurst
          val byteToCommit = commitFromBytePerBurst ? (bytePerBurst-addressBurstOffset) | (fifo.pop.bytes-1).resized
          def bytesInBurst = byteToCommit //minus one
          val bytesInBurstP1 = byteToCommit + 1
          val request = valid && memory && (commitFromBytePerBurst || flush && fifo.pop.bytes =/= 0 ) && memPending =/= p.pendingWritePerChannel
          val bytesToSkip = Reg(UInt(log2Up(p.writeByteCount) bits))

          val decrBytes = fifo.pop.bytesDecr.newPort()


          memPending := memPending + U(fire) - U(memRsp)

          decrBytes := 0
          when(fire){
            decrBytes := bytesInBurstP1.resized

            address := address + bytesInBurstP1
            when(!commitFromBytePerBurst){
              flush := False
            }
          }

          when(valid && memory && pushDone && memPending === 0 && fifo.pop.bytes === 0){
            popDone := True
          }

          when(start){
            bytesToSkip := 0
            flush := False
          }
        }
      }

      fifo.push.available := fifo.push.available - Mux(push.memory, fifo.push.availableDecr, fifo.push.ptrIncr.value) + fifo.pop.ptrIncr.value


      val interrupts = new Area{
        val completion = new Interrupt(done)
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

      def memoryPort = memory.ports.s2b(portId)

      def sink = io.inputs(portId)
      val bankPerBeat = sink.p.byteCount * 8 / p.memory.bankWidth

      val cmd = new Area {
        //todo drop
        val channelsOh = B(channels.map(c => c.valid && !c.push.memory && c.push.portId === portId && c.push.sinkId === sink.sink))
        val channelsFull = B(channels.map(c => c.fifo.push.available < bankPerBeat || c.push.loadDone))
        val sinkHalted = sink.haltWhen((channelsOh & channelsFull).orR)
        val used = Reg(Bits(ps.byteCount bits)) init(0)
        val maskNoSat = sinkHalted.mask & ~used
        val byteValidId = U(0) +: CountOneOnEach(maskNoSat.dropHigh(1))
        val byteLeft = MuxOH(channelsOh, channels.map(_.push.bytesLeft))
        val mask = B((0 until ps.byteCount).map(byteId => maskNoSat(byteId) && byteValidId(byteId) <= byteLeft))
        val byteInUse = CountOne(maskNoSat).min(byteLeft)
        val context = InputContext(ps)
        val byteCount = CountOne(memoryPort.cmd.mask)
        val veryLast = byteLeft < byteCount
        context.channel := channelsOh
        context.bytes := CountOne(mask)
        context.flush := sink.last || veryLast
        context.veryLast := veryLast
        sinkHalted.ready     := memoryPort.cmd.ready && mask === maskNoSat // Do not consume transactions when they are partialy used
        memoryPort.cmd.valid := sinkHalted.valid
        memoryPort.cmd.address := MuxOH(channelsOh, channels.map(_.fifo.push.ptrWithBase)).resized
        memoryPort.cmd.data := sinkHalted.data
        memoryPort.cmd.mask := mask
        memoryPort.cmd.priority := MuxOH(channelsOh, channels.map(_.priority))
        memoryPort.cmd.context := B(context)
        for (channelId <- 0 until channels.size) {
          val hit = channelsOh(channelId) && sinkHalted.fire
          channels(channelId).fifo.push.ptrIncr.newPort() := ((hit && memoryPort.cmd.mask.orR) ? U(bankPerBeat) | U(0)).resized
          when(hit) {
            channels(channelId).push.bytesLeft := byteLeft - byteCount
            channels(channelId).push.loadDone setWhen(veryLast)
          }
        }
        when(memoryPort.cmd.fire){
          used := used | memoryPort.cmd.mask
        }
        when(sink.ready){
          used := 0
        }
      }

      val rsp = new Area{
        val context = memoryPort.rsp.context.as(InputContext(ps))
        for (channelId <- 0 until channels.size) {
          val hit = memoryPort.rsp.fire && context.channel(channelId)
          channels(channelId).fifo.pop.bytesIncr.newPort := (hit ? context.bytes | U(0)).resized
          channels(channelId).pop.flush setWhen(hit && context.flush)
          channels(channelId).pop.pushDone setWhen(hit && context.veryLast)
        }
      }
    }

    val b2s = for(portId <- 0 until p.outputs.size) yield new Area{
      def source = io.outputs(portId)
      val bankPerBeat = source.p.byteCount * 8 / p.memory.bankWidth
      def memoryPort = memory.ports.b2s(portId)
      case class Context() extends Bundle {
        val channel = Bits(p.channels.size bits)
        val veryLast = Bool()
      }
      val cmd = new Area{
        //TODO better arbitration
        val channelsOh = B(OHMasking.first(channels.map(c => c.valid &&  !c.pop.memory && c.pop.portId === portId && !c.fifo.pop.empty)))
        val context = Context()
        val groupRange = log2Up(p.readDataWidth/p.memory.bankWidth) -1 downto log2Up(bankPerBeat)
        val addressRange = ptrWidth-1 downto log2Up(p.readDataWidth/p.memory.bankWidth)
        val veryLastPtr = MuxOH(channelsOh, channels.map(_.pop.veryLastPtr))
        val address = MuxOH(channelsOh, channels.map(_.fifo.pop.ptrWithBase))
        context.channel := channelsOh
        context.veryLast :=  MuxOH(channelsOh, channels.map(_.pop.veryLastValid)) && address(addressRange) === veryLastPtr(addressRange) && address(groupRange) === (1 << groupRange.size)-1

        memoryPort.cmd.valid := channelsOh.orR
        memoryPort.cmd.address := address.resized
        memoryPort.cmd.context := B(context)
        memoryPort.cmd.priority := MuxOH(channelsOh, channels.map(_.priority))

        for (channelId <- 0 until channels.size) {
          channels(channelId).fifo.pop.ptrIncr.newPort() := ((channelsOh(channelId) && memoryPort.cmd.ready) ? U(bankPerBeat) | U(0)).resized
        }
      }

      val rsp = new Area{
        val context = memoryPort.rsp.context.as(Context())
        source.arbitrationFrom(memoryPort.rsp)
        source.data   := memoryPort.rsp.data
        source.mask   := memoryPort.rsp.mask
        source.sink   := MuxOH(context.channel, channels.map(_.pop.sinkId))
        source.last   := context.veryLast && MuxOH(context.channel, channels.map(_.pop.last))

        when(source.fire) {
          for (channelId <- 0 until channels.size) when(context.channel(channelId) && context.veryLast) {
            channels(channelId).pop.popDone := True
          }
        }
      }
    }


    val m2b = new Area {
      val cmd = new Area {
        val arbiter = StreamArbiterFactory.roundRobin.transactionLock.build(NoData, p.channels.size)
        (arbiter.io.inputs, channels.map(_.push.loadRequest)).zipped.foreach(_.valid := _)

        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(arbiter.io.chosen)

        val requestValid = arbiter.io.inputs.map(_.valid).orR


        val address = channel(_.push.address)
        val bytesLeft = channel(_.push.bytesLeft)
        val readAddressBurstRange = address(io.read.p.access.lengthWidth-1 downto 0) //address(log2Up(io.read.p.access.byteCount) downto log2Up(io.read.p.access.byteCount))
        val lengthHead = ~readAddressBurstRange & channel(_.push.bytePerBurst)
        val length = lengthHead.min(bytesLeft).resize(io.read.p.access.lengthWidth)
        val lastBurst = bytesLeft === length

        val context = p.FetchContext()
        context.channel := arbiter.io.chosen
        context.start := address.resized
        context.stop := (address + length).resized
        context.last := lastBurst

        io.read.cmd.valid := False
        io.read.cmd.last := True
        io.read.cmd.source := arbiter.io.chosen
        io.read.cmd.opcode := Bmb.Cmd.Opcode.READ
        io.read.cmd.address := address & (address.maxValue-p.readDataWidth/8+1)
        io.read.cmd.length  := address(log2Up(io.read.p.access.byteCount)-1 downto 0) + length | p.readDataWidth/8-1
        io.read.cmd.context := B(context)


        arbiter.io.output.ready := False

        val beatPerBurst = (length >> log2Up(p.readDataWidth/8)) + 1
        val bankPerBurst = beatPerBurst << log2Up(p.readDataWidth/p.memory.bankWidth)

        when(requestValid) {
          io.read.cmd.valid := True
          when(io.read.cmd.ready) {
            address := address + length + 1
            bytesLeft := bytesLeft - length - 1
            arbiter.io.output.ready := True
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
          channel(_.pop.veryLastTrigger) := True
        }

        val first = io.read.rsp.first
        val last = io.read.rsp.last
        for (byteId <- memory.ports.m2b.cmd.mask.range) {
          val toLow = first && byteId < context.start
          val toHigh = last && byteId > context.stop
          memory.ports.m2b.cmd.mask(byteId) := !toLow && !toHigh
        }

        val writeContext = M2sWriteContext()
        writeContext.last := veryLast
        writeContext.channel := context.channel
        writeContext.loadByteInNextBeat := ((last ? context.stop | context.stop.maxValue) -^ (first ? context.start | 0))
        memory.ports.m2b.cmd.address := channel(_.fifo.push.ptrWithBase).resized
        memory.ports.m2b.cmd.arbitrationFrom(io.read.rsp)
        memory.ports.m2b.cmd.data := io.read.rsp.data
        memory.ports.m2b.cmd.priority := channel(_.priority)
        memory.ports.m2b.cmd.context := B(writeContext)


        for (channelId <- 0 until p.channels.size) {
          val fire = memory.ports.m2b.cmd.fire && context.channel === channelId
          channels(channelId).fifo.push.ptrIncr.newPort := (fire ? U(io.read.p.access.dataWidth / p.memory.bankWidth) | U(0)).resized
        }

      }

      val writeRsp = new Area{
        val context = memory.ports.m2b.rsp.context.as(M2sWriteContext())
        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(context.channel)
        when(memory.ports.m2b.rsp.fire && context.last){
          channel(_.pop.pushDone) := True
          channel(_.pop.flush) := True
        }
        for (channelId <- 0 until p.channels.size) {
          val fire = memory.ports.m2b.rsp.fire && context.channel === channelId
          channels(channelId).fifo.pop.bytesIncr.newPort := (fire ? (context.loadByteInNextBeat + 1) | U(0)).resized
        }
      }
    }


    val b2m = new Area {
      case class FsmCmd() extends Bundle{
        val channel = UInt(log2Up(p.channels.size) bits)
        val bytesInBurst = UInt(io.write.p.access.lengthWidth bits)
        val address = UInt(p.writeAddressWidth bits)
        val ptr = ptrType()
        val ptrMask = ptrType()
      }
      val arbiter = new Area{
        val core = StreamArbiterFactory.roundRobin.noLock.build(NoData, p.channels.size)
        (core.io.inputs, channels.map(_.pop.arbiter.request)).zipped.foreach(_.valid := _)

        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(core.io.chosen)

        val payload = FsmCmd()
        payload.channel := core.io.chosen
        payload.bytesInBurst := channel(_.pop.arbiter.bytesInBurst)
        payload.address := channel(_.pop.address)
        payload.ptr     := channel(_.fifo.pop.ptrWithBase)
        payload.ptrMask := channel(_.fifo.words)

        val sel = core.io.output.translateWith(payload)
        when(sel.fire){
          channel(_.pop.arbiter.fire) := True
        }
      }


      val fsm = new Area {
        val sel = arbiter.sel.halfPipe()
        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(sel.channel)

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
          memory.ports.b2m.cmd.priority := channel(_.priority)

          for (channelId <- 0 until channels.size) {
            when(sel.valid && sel.channel === channelId && memory.ports.b2m.cmd.ready) {
              sel.ptr.getDrivingReg := (sel.ptr & ~sel.ptrMask) | ((sel.ptr + U(io.read.p.access.dataWidth / p.memory.bankWidth) & sel.ptrMask))
            }
          }
        }

          //        for(channelId <- 0 until p.channels.size){
          //          channels(channelId).fifo.pop.ptrIncr.newPort() := ((memory.ports.b2m.cmd.fire && sel.payload === channelId) ? U(io.read.p.access.dataWidth/p.memory.bankWidth) | U(0))
          //        }

        val aggregate = new Area {
          val context = memory.ports.b2m.rsp.context.as(FetchContext())
          val memoryPort = memory.ports.b2m.rsp.throwWhen(context.toggle =/= toggle)


          val engine = Aggregator(AggregatorParameter(
            byteCount = p.writeByteCount,
            burstLength = p.writeLengthWidth,
            context = NoData
          ))

//          val byteCounter = Reg(UInt(p.writeLengthWidth + 1 bits))
//          when(engine.io.input.fire) {
//            byteCounter := byteCounter + CountOne(engine.io.input.mask)
//          }
//          val lastFiredDetected = byteCounter > sel.bytesInBurst
//          val lastFiredReg = Reg(Bool) setWhen(lastFiredDetected)

//          for(channelId <- 0 until p.channels.size){
//            channels(channelId).fifo.pop.ptrIncr.newPort() := ((sel.valid && memoryPort.fire && !lastFiredDetected && !lastFiredReg) ? U(io.write.p.access.dataWidth/p.memory.bankWidth) | U(0)).resized
//          }


//          when(!sel.isStall){
//            lastFiredReg := False
//            byteCounter := 0
//          }

          val first = Reg(Bool) clearWhen(memoryPort.fire) setWhen(!sel.isStall)
          val bytesToSkip = channel(_.pop.arbiter.bytesToSkip)
//          val bytesToSkipMask = B((0 until p.writeByteCount).map(byteId => !first || bytesToSkip === p.writeByteCount-1 || byteId > bytesToSkip))
          val bytesToSkipMask = B((0 until p.writeByteCount).map(byteId => !first || byteId >= bytesToSkip))
          engine.io.input.arbitrationFrom(memoryPort)
          engine.io.input.data := memoryPort.data
          engine.io.input.mask := memoryPort.mask & bytesToSkipMask
          engine.io.offset := sel.address.resized
          engine.io.burstLength := sel.bytesInBurst
          engine.io.flush := ! (RegNext(sel.isStall) init(False))
        }

//        when(arbiter.sel.fire) {
//          aggregate.usedBytes := arbiter.channel(_.fifo.pop.usedBytes)
//        }
        val cmd = new Area {

          def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(sel.channel)

          val beatCounter = Reg(UInt(io.read.p.access.beatCounterWidth bits))

          val maskFirstTrigger = address.resize(log2Up(p.writeByteCount) bits)
          val maskLastTrigger = maskFirstTrigger + sel.bytesInBurst.resized
          val maskLast = B((0 until p.writeByteCount).map(byteId => byteId <= maskLastTrigger))
          val maskFirst = B((0 until p.writeByteCount).map(byteId => byteId >= maskFirstTrigger))
          val enoughAggregation = sel.valid && !aggregate.engine.io.flush && (io.write.cmd.last ? ((aggregate.engine.io.output.mask & maskLast) === maskLast) | aggregate.engine.io.output.mask.andR)

          aggregate.engine.io.output.enough := enoughAggregation
          aggregate.engine.io.output.consume := io.write.cmd.fire
          aggregate.engine.io.output.lastByteUsed := maskLastTrigger
          io.write.cmd.valid := enoughAggregation
          io.write.cmd.last := beatCounter === sel.bytesInBurst >> log2Up(io.read.p.access.byteCount)
          io.write.cmd.address := address & (address.maxValue-p.readDataWidth/8+1)
          io.write.cmd.opcode := Bmb.Cmd.Opcode.WRITE
          io.write.cmd.data := aggregate.engine.io.output.data
          io.write.cmd.mask := ~((io.write.cmd.first ? ~maskFirst | B(0)) | (io.write.cmd.last ? ~maskLast| B(0)))
          io.write.cmd.length := address(log2Up(io.read.p.access.byteCount)-1 downto 0) + sel.bytesInBurst | p.readDataWidth/8-1
          io.write.cmd.source := sel.channel

          for(channelId <- 0 until p.channels.size){
            channels(channelId).fifo.pop.ptrIncr.newPort() := ((sel.valid  && sel.channel === channelId && (aggregate.engine.io.output.consumed || io.write.cmd.lastFire && aggregate.engine.io.output.usedUntil === aggregate.engine.io.output.usedUntil.maxValue)) ? U(io.write.p.access.dataWidth/p.memory.bankWidth) | U(0)).resized
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
            channel(_.pop.arbiter.bytesToSkip) := aggregate.engine.io.output.usedUntil + 1
          }
        }
      }



      val rsp = new Area{
        io.write.rsp.ready := True
        val context = io.write.rsp.context.as(p.WriteContext())
        when(io.write.rsp.fire){
          channels.map(_.pop.arbiter.memRsp).write(context.channel, True)
        }
      }
    }

    io.interrupts := 0
    val mapping = new Area{
      for(channel <- channels){
        val a = 0x800+channel.id*0x40

        ctrl.writeMultiWord(channel.push.address, a+0x00)
        ctrl.write(channel.push.portId,           a+0x08, 0)
        ctrl.write(channel.push.sinkId,           a+0x08, 16)
        ctrl.write(channel.push.bytePerBurst,     a+0x0C, 0)
        ctrl.write(channel.push.memory,           a+0x0C, 12)

        ctrl.writeMultiWord(channel.pop.address, a+0x10)
        ctrl.write(channel.pop.portId,           a+0x18, 0)
        ctrl.write(channel.pop.sinkId,           a+0x18, 16)
        ctrl.write(channel.pop.bytePerBurst,     a+0x1C, 0)
        ctrl.write(channel.pop.memory,           a+0x1C, 12)
        ctrl.write(channel.pop.last,             a+0x1C, 13)

        ctrl.write(channel.bytes, a+0x20, 0)
        ctrl.setOnSet(channel.start, a+0x2C, 0)
        ctrl.read(channel.valid, a+0x2C, 0)

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
}


object DmaSgGen extends App{
  import spinal.core.sim._
  val p = Parameter(
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
    outputs = Seq(
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
    ),
    inputs = Seq(
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
    ),
    channels = Seq(
      DmaSg.Channel(

      ),
      DmaSg.Channel(

      ),
      DmaSg.Channel(

      )
    ),
    bytePerTransferWidth = 16
  )
  val pCtrl = BmbParameter(
    addressWidth = 12,
    dataWidth    = 32,
    sourceWidth  = 0,
    contextWidth = 4,
    lengthWidth  = 2
  )
  SimConfig.allOptimisation.compile(new DmaSg.Core(p, pCtrl)).doSim(seed=42){ dut =>
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.forkSimSpeedPrinter(2.0)

    val writesAllowed = mutable.HashMap[Long, Byte]()
    val memory = new BmbMemoryAgent{
      override def writeNotification(address: Long, value: Byte): Unit = {
        assert(writesAllowed.remove(address).get == value)
        super.setByte(address, value)
      }
    }
    memory.addPort(dut.io.read, 0, dut.clockDomain, true)
    memory.addPort(dut.io.write, 0, dut.clockDomain, true)
    val memoryReserved = MemoryRegionAllocator(base = 0, size = 1l << p.writeAddressWidth)

    val outputs = for(outputId <- 0 until p.outputs.size) yield new {
      val readyDriver = StreamReadyRandomizer(dut.io.outputs(outputId), dut.clockDomain)
      val ref = Array.fill(1 << p.outputs(outputId).sinkWidth)(mutable.Queue[(Int, Int, Boolean)]())
      val monitor = new BsbMonitor(dut.io.outputs(outputId), dut.clockDomain) {
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
    }
    val inputs = for(inputId <- 0 until p.inputs.size) yield new {
      val ip = p.inputs(inputId)

      val packets = ArrayBuffer[Packet]()
      val driver = StreamDriver(dut.io.inputs(inputId), dut.clockDomain) { p =>
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
          p.last #= false //TODO

          if(packet.data.isEmpty) packets.remove(packets.indexOf(packet))
          true
        }
      }
      val reservedSink = mutable.HashSet[Int]()
    }

    periodicaly(10*1000){
      outputs.foreach(_.readyDriver.factor = Random.nextFloat)
    }

    val ctrl = BmbDriver(dut.io.ctrl, dut.clockDomain)

    def channelToAddress(channel : Int) = 0x800 + channel*0x40
    def channelPushMemory(channel : Int, address : BigInt, bytePerBurst : Int): Unit ={
      val channelAddress = channelToAddress(channel)
      ctrl.write(address, channelAddress + 0x00)
      ctrl.write(bytePerBurst-1 | 1 << 12, channelAddress + 0x0C)
    }
    def channelPopMemory(channel : Int, address : BigInt, bytePerBurst : Int): Unit ={
      val channelAddress = channelToAddress(channel)
      ctrl.write(address, channelAddress + 0x10)
      ctrl.write(bytePerBurst-1 | 1 << 12, channelAddress + 0x1C)
    }
    def channelPushStream(channel : Int, portId : Int, sourceId : Int, sinkId : Int): Unit ={
      val channelAddress = channelToAddress(channel)
      ctrl.write(portId << 0 | sourceId << 8 | sinkId << 16, channelAddress + 0x08)
      ctrl.write(0, channelAddress + 0x0C)
    }
    def channelPopStream(channel : Int, portId : Int, sourceId : Int, sinkId : Int, withLast : Boolean): Unit ={
      val channelAddress = channelToAddress(channel)
      ctrl.write(portId << 0 | sourceId << 8 | sinkId << 16, channelAddress + 0x18)
      ctrl.write(if(withLast) 1 << 13 else 0, channelAddress + 0x1C)
    }
    def channelStart(channel : Int, bytes : BigInt): Unit ={
      val channelAddress = channelToAddress(channel)
      ctrl.write(bytes-1, channelAddress + 0x20)
      ctrl.write(1, channelAddress+0x2C)
    }
    def channelConfig(channel : Int,
                      fifoBase : Int,
                      fifoBytes : Int,
                      priority : Int): Unit ={
      val channelAddress = channelToAddress(channel)
      ctrl.write(fifoBase << 0 | fifoBytes-1 << 16,  channelAddress+0x30)
      ctrl.write(priority,  channelAddress+0x34)
    }
    def channelInterruptConfigure(channel : Int, mask : Int): Unit ={
      val channelAddress = channelToAddress(channel)
      ctrl.write(mask, channelAddress+0x3C)
      ctrl.write(mask, channelAddress+0x38)
    }
    def channelStartAndWait(channel : Int, bytes : BigInt): Unit ={
      val channelAddress = channelToAddress(channel)
      if(Random.nextBoolean()){
        //By pulling
        channelStart(channel, bytes)
        while ((ctrl.read(channelAddress + 0x2C) & 1) != 0) {
          dut.clockDomain.waitSampling(Random.nextInt(50))
        }
      } else {
        //By interrupt
        channelInterruptConfigure(channel, 1)
        fork{
          dut.clockDomain.waitSampling(Random.nextInt(10))
          channelStart(channel, bytes)
        }
        waitUntil((dut.io.interrupts.toInt & 1 << channel) != 0)
        assert((ctrl.read(channelAddress + 0x2C) & 1) == 0)
      }
    }
    def channelStarted(channel : Int) ={
      val channelAddress = channelToAddress(channel)
      (ctrl.read(channelAddress + 0x2C) & 1) != 0
    }

    val channelAgent = for((channel, channelId) <- dut.p.channels.zipWithIndex) yield fork {
      val cp = dut.p.channels(channelId)
      val M2M, M2S, S2M = new Object
      var tests = ArrayBuffer[Object]()
      tests += M2M
      tests += M2S
      tests += S2M
      for (r <- 0 until 1000) {
        dut.clockDomain.waitSampling(Random.nextInt(100))
        tests.randomPick() match {
          case S2M => if (p.inputs.nonEmpty) {
            val bytes = (Random.nextInt(0x100) + 1)
            val to = memoryReserved.allocate(size = bytes)
//            val to = 0x100
//            val bytes = 0x100
            val inputId = Random.nextInt(dut.p.inputs.size)
            val ip = dut.p.inputs(inputId)
            val source = Random.nextInt(1 << ip.sourceWidth)
            val sink = Random.nextInt(1 << ip.sinkWidth)

            while(inputs(inputId).reservedSink.contains(sink)) dut.clockDomain.waitSampling(Random.nextInt(100))
            inputs(inputId).reservedSink.add(sink)

            val packet = Packet(source = source, sink = sink, last = false)
            for (byteId <- 0 until bytes) {
              val value = Random.nextInt & 0xFF
              writesAllowed(to.base.toInt + byteId) = value.toByte
              packet.data.enqueue(value)
            }

            channelPushStream(channelId, inputId, source, sink)
            channelPopMemory(channelId, to.base.toInt, 16)
            channelConfig(channelId, 0x100 + 0x40*channelId, 0x40, 2)
            fork{
              while(!channelStarted(channelId)){
                dut.clockDomain.waitSampling(Random.nextInt(5))
              }
              inputs(inputId).packets += packet
            }
            channelStartAndWait(channelId, bytes)
            inputs(inputId).reservedSink.remove(sink)
          }
          case M2M => {
            val bytes = (Random.nextInt(0x100) + 1)
            val from = memoryReserved.allocate(size = bytes)
            val to = memoryReserved.allocate(size = bytes)
//            val from = 0x100 + Random.nextInt(4)
//            val to = 0x400 + Random.nextInt(4)
//            val bytes = 0x40 + Random.nextInt(4)
//            val from = 0x1000 + 0x100*channelId
//            val to = 0x2000 + 0x100*channelId
//            val bytes = 0x40
//            println(s"$from $to $bytes")
            for (byteId <- 0 until bytes) {
              writesAllowed(to.base.toInt + byteId) = memory.memory.read(from.base.toInt + byteId)
            }
            channelPushMemory(channelId, from.base.toInt, 16)
            channelPopMemory(channelId, to.base.toInt, 16)
            channelConfig(channelId, 0x100 + 0x40*channelId, 0x40, 2)
            channelStartAndWait(channelId, bytes)
          }
          case M2S => if (p.outputs.nonEmpty) {
            val bytes = (Random.nextInt(0x100) + 1)
            val from = memoryReserved.allocate(size = bytes)
            val outputId = Random.nextInt(dut.p.outputs.size)
            val op = dut.p.outputs(outputId)
            val source = Random.nextInt(1 << op.sourceWidth)
            val sink = Random.nextInt(1 << op.sinkWidth)
            val withLast = Random.nextBoolean()

            while(outputs(outputId).reservedSink.contains(sink)) dut.clockDomain.waitSampling(Random.nextInt(100))
            outputs(outputId).reservedSink.add(sink)

            for (byteId <- 0 until bytes) {
              outputs(outputId).ref(sink).enqueue((memory.memory.read(from.base.toInt + byteId), source, false))
            }
            if(withLast) outputs(outputId).ref(sink).enqueue((0, source, true))

            channelPushMemory(channelId, from.base.toInt, 16)
            channelPopStream(channelId, outputId, source, sink, withLast)
            channelConfig(channelId, 0x100 + 0x40*channelId, 0x40, 2)
            channelStartAndWait(channelId, bytes = bytes)

            outputs(outputId).reservedSink.remove(sink)
          }
        }
      }
    }


    channelAgent.foreach(_.join())
    dut.clockDomain.waitSampling(1000)
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