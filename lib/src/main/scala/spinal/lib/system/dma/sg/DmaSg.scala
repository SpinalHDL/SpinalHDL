package spinal.lib.system.dma.sg

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import spinal.lib.bus.bmb.sim.{BmbDriver, BmbMemoryAgent}
import spinal.lib.bus.bsb._
import spinal.lib.bus.bsb.sim.BsbMonitor
import spinal.lib.sim.{SparseMemory, StreamDriver, StreamReadyRandomizer}
import spinal.lib.system.dma.sg.DmaSg.Parameter

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
  ).addSources(1, BmbSourceParameter(
    contextWidth = widthOf(p.FetchContext()),
    lengthWidth = p.readLengthWidth,
    canWrite = false
  ))

  def getWriteRequirements(p : Parameter) = BmbAccessParameter(
    addressWidth = p.writeAddressWidth,
    dataWidth    = p.writeDataWidth
  ).addSources(1, BmbSourceParameter(
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
                       bytePerTransferWidth : Int){
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
      val last = Bool()
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
    }


    val memory = new Area{
      val core = DmaMemoryCore(DmaMemoryCoreParameter(
        layout = p.memory,
        writes = p.inputs.map(p => DmaMemoryCoreWriteParameter(p.byteCount, widthOf(InputContext(p))))   :+ DmaMemoryCoreWriteParameter(io.read.p.access.byteCount, 1),
        reads  = p.outputs.map(p => DmaMemoryCoreReadParameter(p.byteCount, Core.this.p.channels.size + 1))  :+ DmaMemoryCoreReadParameter(io.write.p.access.byteCount, ptrWidth + 1)
      ))
      val ports = new Area{
        val m2b = core.io.writes.last
        val b2m = core.io.reads.last
        val s2b = core.io.writes.dropRight(1)
        val b2s = core.io.reads.dropRight(1)
      }
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
          val availableDecr = DataOr(ptrType)

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

          val flush = DataOr(Bool)
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
        push.available := push.available - push.availableDecr.value + pop.ptrIncr.value
        when(start){
          push.ptr := 0
          push.available := words + 1
          pop.ptr := 0
          pop.bytes := 0
        }
      }


      val push = new Area{
        val memory  = Reg(Bool)
        val address = Reg(UInt(io.read.p.access.addressWidth bits))
        val bytePerBurst = Reg(UInt(io.read.p.access.lengthWidth bits)) //minus one
        val bytesLeft = Reg(UInt(p.bytePerTransferWidth bits)) //minus one

        val portId = Reg(UInt(log2Up(p.inputs.size) bits))
        val sourceId = Reg(UInt(p.inputsSourceWidth bits))
        val sinkId = Reg(UInt(p.inputsSinkWidth bits))

        val loadDone = RegInit(True) clearWhen(memory && start)
        val loadRequest = !loadDone && memory && fifo.push.available > (bytePerBurst >> log2Up(io.read.p.access.byteCount))
        when(start){
          bytesLeft := bytes
        }

      }

      val pop = new Area{
        val memory  = Reg(Bool)
        val address = Reg(UInt(io.write.p.access.addressWidth bits))
        val bytePerBurst = Reg(UInt(io.write.p.access.lengthWidth bits))
        val pushDone = Reg(Bool) clearWhen(start)
        val popDone = Reg(Bool) clearWhen(start)
        val veryLastTrigger = False
        val veryLastValid = Reg(Bool) setWhen(veryLastTrigger) clearWhen(start)
        val veryLastPtr = Reg(ptrType)
        val veryLast = veryLastValid && veryLastPtr === fifo.pop.ptr
        when(veryLastTrigger){
          veryLastPtr := fifo.push.ptr
        }

        when(valid && pushDone && popDone){
          done := True
        }

        val portId = Reg(UInt(log2Up(p.outputs.size) bits))
        val sourceId = Reg(UInt(p.outputsSourceWidth bits))
        val sinkId = Reg(UInt(p.outputsSinkWidth bits))
//        val commitValid = RegInit(False)
//        val commitBytes = Reg(UInt(io.write.p.access.lengthWidth bits)) // minus one
//        when(valid) {
//          when(bytes >= bytePerBurst) {
//            commitBytes := bytePerBurst
//          }
//        }


        val arbiter = new Area{

          val first = Reg(Bool)
          val busy = RegInit(False)
          val fire = False
          val bytesLeft = Reg(UInt(p.bytePerTransferWidth bits)) //minus one
          val commitFromBytePerBurst = fifo.pop.bytes > bytePerBurst
          val byteToCommit = commitFromBytePerBurst ? bytePerBurst | U(0)
          val last = bytesLeft === byteToCommit
          def bytesInBurst = byteToCommit //minus one
          val bytesInBurstP1 = byteToCommit + 1
//          val bytesInBurst = first ? (address & bytePerBurst) | (last ? bytesLeft | bytePerBurst) //minus one
          val request = valid && memory && commitFromBytePerBurst && busy

          val decrBytes = fifo.pop.bytesDecr.newPort()

          decrBytes := 0
          when(fire){
            bytesLeft := bytesLeft - bytesInBurstP1
            decrBytes := bytesInBurstP1.resized
            when(last) {
              busy := False
            }
          }

          when(start){
            busy := True
            bytesLeft := bytes
          }
        }
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
        val channelsOh = B(channels.map(c => c.valid && c.push.portId === portId && c.push.sinkId === sink.sink))
        val channelsFull = B(channels.map(c => c.fifo.push.available < bankPerBeat))
        val sinkHalted = sink.haltWhen((channelsOh & channelsFull).orR)
        val used = Reg(Bits(ps.byteCount bits)) init(0)
        val maskNoSat = sinkHalted.mask & ~used
        val byteValidId = U(0) +: CountOneOnEach(maskNoSat.dropHigh(1))
        val byteLeft = MuxOH(channelsOh, channels.map(_.push.bytesLeft))
        val mask = B((0 until ps.byteCount).map(byteId => maskNoSat(byteId) && byteValidId(byteId) <= byteLeft))
        val byteInUse = CountOne(maskNoSat).min(byteLeft)
        val context = InputContext(ps)
        context.channel := channelsOh
        context.bytes := CountOne(mask)
        context.flush := False //TODO
        sinkHalted.ready     := memoryPort.cmd.ready && mask === maskNoSat // Do not consume transactions when they are partialy used
        memoryPort.cmd.valid := sinkHalted.valid
        memoryPort.cmd.address := MuxOH(channelsOh, channels.map(_.fifo.push.ptrWithBase)).resized
        memoryPort.cmd.data := sinkHalted.data
        memoryPort.cmd.mask := mask
        memoryPort.cmd.priority := MuxOH(channelsOh, channels.map(_.priority))
        memoryPort.cmd.context := B(context)
        for (channelId <- 0 until channels.size) {
          val hit = channelsOh(channelId) && sinkHalted.fire
          channels(channelId).fifo.push.ptrIncr.newPort() := (hit ? U(bankPerBeat) | U(0)).resized
          channels(channelId).fifo.pop.bytesIncr.newPort() := (hit ? byteInUse | U(0)).resized
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
          val hit = memoryPort.rsp.fire && context.channel === channelId
          channels(channelId).fifo.pop.bytesIncr.newPort := (hit ? context.bytes | U(0)).resized
          channels(channelId).fifo.pop.flush.newPort := RegNext(hit && context.flush).init(False)
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
        val channelsOh = B(channels.map(c => c.valid &&  !c.pop.memory && c.push.portId === portId))
        val channelsNotEmpty = B(channels.map(c => !c.fifo.pop.empty))
        val context = Context()
        context.channel := channelsOh
        context.veryLast :=  MuxOH(channelsOh, channels.map(_.pop.veryLast))

        memoryPort.cmd.valid := (channelsOh & channelsNotEmpty).orR
        memoryPort.cmd.address := MuxOH(channelsOh, channels.map(_.fifo.pop.ptrWithBase)).resized
        memoryPort.cmd.context := B(context)
        memoryPort.cmd.priority := MuxOH(channelsOh, channels.map(_.priority))

        for (channelId <- 0 until channels.size) {
          channels(channelId).fifo.pop.ptrIncr.newPort() := ((channelsOh(channelId) && channelsNotEmpty(channelId) && memoryPort.cmd.ready) ? U(bankPerBeat) | U(0)).resized
        }
      }

      val rsp = new Area{
        val context = memoryPort.rsp.context.as(Context())
        source.arbitrationFrom(memoryPort.rsp)
        source.data   := memoryPort.rsp.data
        source.mask   := memoryPort.rsp.mask
        source.source := MuxOH(context.channel, channels.map(_.pop.sourceId))
        source.sink   := MuxOH(context.channel, channels.map(_.pop.sinkId))
        source.last   := False

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
        val availabilityDecr = channels.map(_.fifo.push.availableDecr.newPort())
        availabilityDecr.foreach(_ := 0)
        when(requestValid) {
          io.read.cmd.valid := True
          when(io.read.cmd.ready) {
            address := address + length + 1
            bytesLeft := bytesLeft - length - 1
            arbiter.io.output.ready := True
            for(channelId <- 0 until channels.size) when(channelId === arbiter.io.chosen){ availabilityDecr(channelId) := bankPerBurst.resized}
            when(lastBurst) {
              channel(_.push.loadDone) := True
            }
          }
        }
      }

      val rsp = new Area{
        val context = io.read.rsp.context.as(p.FetchContext())

        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(context.channel)
        val loadByteInNextBeat = context.stop -^ context.start + 1

        val veryLast = context.last && io.read.rsp.last
        when(io.read.rsp.fire && veryLast){
          channel(_.pop.veryLastTrigger) := True
        }

        memory.ports.m2b.cmd.address := channel(_.fifo.push.ptrWithBase).resized
        memory.ports.m2b.cmd.arbitrationFrom(io.read.rsp)
        memory.ports.m2b.cmd.data := io.read.rsp.data
        memory.ports.m2b.cmd.priority := channel(_.priority)
        memory.ports.m2b.cmd.context(0) := veryLast

        val first = io.read.rsp.first
        val last = io.read.rsp.last
        for(byteId <- memory.ports.m2b.cmd.mask.range){
          val toLow = first && byteId < context.start
          val toHigh = last && byteId >  context.stop
          memory.ports.m2b.cmd.mask(byteId) := !toLow && !toHigh
        }

        for(channelId <- 0 until p.channels.size){
          val fire = memory.ports.m2b.cmd.fire && context.channel === channelId
          channels(channelId).fifo.push.ptrIncr.newPort := (fire ? U(io.read.p.access.dataWidth/p.memory.bankWidth) | U(0)).resized
          channels(channelId).fifo.pop.bytesIncr.newPort := (fire ? loadByteInNextBeat | U(0)).resized
        }

        when(memory.ports.m2b.rsp.fire && memory.ports.m2b.rsp.context(0)){
          channel(_.pop.pushDone) := True
        }
      }
    }


    val b2m = new Area {
      case class FsmCmd() extends Bundle{
        val channel = UInt(log2Up(p.channels.size) bits)
        val bytesInBurst = UInt(io.write.p.access.lengthWidth bits)
        val last = Bool()
      }
      val arbiter = new Area{
        val core = StreamArbiterFactory.roundRobin.noLock.build(NoData, p.channels.size)
        (core.io.inputs, channels.map(_.pop.arbiter.request)).zipped.foreach(_.valid := _)

        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(core.io.chosen)

        val payload = FsmCmd()
        payload.channel := core.io.chosen
        payload.last := channel(_.pop.arbiter.last)
        payload.bytesInBurst := channel(_.pop.arbiter.bytesInBurst)

        val sel = core.io.output.translateWith(payload)
        when(sel.fire){
          channel(_.pop.arbiter.fire) := True
        }
      }


      val fsm = new Area {
        val sel = arbiter.sel.stage()
        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(sel.channel)

        val toggle = RegInit(False)
        toggle := toggle ^ sel.fire


        val address = RegNext(channel(_.pop.address))
        val addressNext = address + sel.bytesInBurst + 1

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
          memory.ports.b2m.cmd.address := channel(_.fifo.pop.ptrWithBase).resized
          memory.ports.b2m.cmd.context := B(context)
          memory.ports.b2m.cmd.priority := channel(_.priority)



          for (channelId <- 0 until channels.size) {
            channels(channelId).fifo.pop.ptrIncr.newPort() := ((sel.valid && sel.channel === channelId && memory.ports.b2m.cmd.ready) ? U(io.read.p.access.dataWidth / p.memory.bankWidth) | U(0)).resized
          }
        }

          //        for(channelId <- 0 until p.channels.size){
          //          channels(channelId).fifo.pop.ptrIncr.newPort() := ((memory.ports.b2m.cmd.fire && sel.payload === channelId) ? U(io.read.p.access.dataWidth/p.memory.bankWidth) | U(0))
          //        }

        val aggregate = new Area {
          val context = memory.ports.b2m.rsp.context.as(FetchContext())
          val memoryPort = memory.ports.b2m.rsp.throwWhen(context.toggle =/= toggle)
          val usedBytes = Reg(Bits(p.writeDataWidth bits))
          val feedValids = memoryPort.mask //& ~usedBytes
          val feedData = memoryPort.data.subdivideIn(8 bits)
          val first = Reg(Bool)
          val last = Reg(Bool)
          val valids = Reg(Bits(p.writeByteCount bits)) init (0)
          val buffer = Reg(Vec(Bits(8 bits), p.writeByteCount))
          val byteAddress = address.resize(log2Up(p.writeByteCount) bits)
          //          val bufferFeed = Bits(p.write.byteCount bits)
          //          for(byteId <- bufferFeed.range){
          //            val toLow = first && byteId < byteAddress
          //            val toHigh = last && byteId > (byteAddress + channel(_.bytes).resized)
          //            bufferFeed(byteId) := !toLow && !toHigh
          //          }

          val dataInIndex = U(0) +: CountOneOnEach(valids.dropHigh(1))
          val dataInOffset = first ? byteAddress | U(0)
          val dataInIndexWithOffset = dataInIndex.map(_ + dataInOffset)
//          for (byteId <- 0 until p.writeByteCount) {
//            when(!valids(byteId)) {
//              val indexMatch = dataInIndexWithOffset.map(_ === byteId).asBits()
//              buffer(byteId) := MuxOH(memoryPort.mask & indexMatch, feedData)
//            }
//          }
//          when(memoryPort.valid){
//
//            usedBytes := usedBytes |
//          }


          val bufferWillBeCompleted = False
          val bufferStream = memoryPort.translateWith(NoData).throwWhen(!bufferWillBeCompleted).m2sPipe(flush = sel.ready)
          val bufferPtr = Reg(ptrType)
          bufferWillBeCompleted setWhen(memoryPort.valid)
          when(memoryPort.valid && !bufferStream.isStall){
            valids := feedValids
            buffer := feedData
            bufferPtr := context.ptr
          }


//          first := ???
//          when(sel.fire) {
//            //            first := True
//          }
        }

//        when(arbiter.sel.fire) {
//          aggregate.usedBytes := arbiter.channel(_.fifo.pop.usedBytes)
//        }
        val cmd = new Area {

          def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(sel.channel)

          val beatCounter = Reg(UInt(io.read.p.access.beatCounterWidth bits))
          io.write.cmd.arbitrationFrom(aggregate.bufferStream)
          io.write.cmd.last := beatCounter === io.write.cmd.length >> log2Up(io.read.p.access.byteCount)
          io.write.cmd.address := address
          io.write.cmd.opcode := Bmb.Cmd.Opcode.WRITE
          io.write.cmd.data := B(aggregate.buffer)
          io.write.cmd.mask := aggregate.valids
          io.write.cmd.length := sel.bytesInBurst

          val context = p.WriteContext()
          context.channel := sel.channel
          context.last := sel.last
          io.write.cmd.context := B(context)

          when(io.write.cmd.fire){
            beatCounter := beatCounter + 1
          }
          when(io.write.cmd.lastFire || !io.write.cmd.valid){
            beatCounter := 0
          }

          when(io.write.cmd.lastFire){
            sel.ready := True
            channel(_.pop.address) := addressNext
            channel(_.fifo.pop.ptr) := aggregate.bufferPtr + 1
          }
        }
      }



      val rsp = new Area{
        io.write.rsp.ready := True
        val context = io.write.rsp.context.as(p.WriteContext())
        when(io.write.rsp.fire && context.last){
          channels.map(_.done := True)
        }
      }
    }


    val mapping = new Area{
      for(channel <- channels){
        val a = 0x800+channel.id*0x40

        ctrl.writeMultiWord(channel.push.address, a+0x00)
        ctrl.write(channel.push.portId,           a+0x08, 0)
        ctrl.write(channel.push.sourceId,         a+0x08, 8)
        ctrl.write(channel.push.sinkId,           a+0x08, 16)
        ctrl.write(channel.push.bytePerBurst,     a+0x0C, 0)
        ctrl.write(channel.push.memory,           a+0x0C, 12)

        ctrl.writeMultiWord(channel.pop.address, a+0x10)
        ctrl.write(channel.pop.portId,           a+0x18, 0)
        ctrl.write(channel.pop.sourceId,         a+0x18, 8)
        ctrl.write(channel.pop.sinkId,           a+0x18, 16)
        ctrl.write(channel.pop.bytePerBurst,     a+0x1C, 0)
        ctrl.write(channel.pop.memory,           a+0x1C, 12)

        ctrl.write(channel.bytes, a+0x20, 0)
        ctrl.setOnSet(channel.start, a+0x2C, 0)
        ctrl.read(channel.valid, a+0x2C, 0)

        ctrl.write(channel.fifo.base, a+0x30, log2Up(p.memory.bankWidth/8))
        ctrl.write(channel.fifo.words, a+0x30, 16 + log2Up(p.memory.bankWidth/8))
        ctrl.write(channel.priority, a+0x34, 0)

      }
    }


    io.interrupts := 0
  }
}


object DmaSgGen extends App{
  import spinal.core.sim._
  val p = Parameter(
    readAddressWidth  = 32,
    readDataWidth     = 32,
    readLengthWidth   = 6,
    writeAddressWidth = 32,
    writeDataWidth    = 32,
    writeLengthWidth  = 6,
    memory = DmaMemoryLayout(
      bankCount            = 1,
      bankWords            = 256,
      bankWidth            = 32,
      priorityWidth        = 2
    ),
    outputs = Seq(
      BsbParameter(
        byteCount   = 4,
        sourceWidth = 4,
        sinkWidth   = 4
      )
    ),
    inputs = Seq(
      BsbParameter(
        byteCount   = 4,
        sourceWidth = 4,
        sinkWidth   = 4
      )
    ),
    channels = Seq(
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
  SimConfig.withWave.compile(new DmaSg.Core(p, pCtrl)).doSim(seed=42){ dut =>
    dut.clockDomain.forkStimulus(10)

    val writesAllowed = mutable.HashMap[Long, Byte]()
    val memory = new BmbMemoryAgent{
      override def writeNotification(address: Long, value: Byte): Unit = {
        assert(writesAllowed.remove(address).get == value)
        super.setByte(address, value)
      }
    }
    memory.addPort(dut.io.read, 0, dut.clockDomain, true)
    memory.addPort(dut.io.write, 0, dut.clockDomain, true)

    val outputs = for(outputId <- 0 until p.outputs.size) yield new {
      val readyDriver = StreamReadyRandomizer(dut.io.outputs(outputId), dut.clockDomain)
      val ref = mutable.Queue[(Int, Int, Int)]()
      val monitor = BsbMonitor(dut.io.outputs(outputId), dut.clockDomain){(value, source, sink) =>
        val e = ref.dequeue()
        assert(value == e._1)
        assert(source == e._2)
        assert(sink == e._3)
      }
    }
    case class Packet(source : Int, sink : Int){
      val data = mutable.Queue[Int]()
    }
    val inputs = for(inputId <- 0 until p.inputs.size) yield new {
      val ip = p.inputs(inputId)
      val readyDriver = StreamReadyRandomizer(dut.io.outputs(inputId), dut.clockDomain)

      val packets = ArrayBuffer[Packet]()
      val driver = StreamDriver(dut.io.inputs(inputId), dut.clockDomain) { p =>
        if (packets.isEmpty) {
          false
        } else {
          val packet = packets.randomPick()
          var data = BigInt(0)
          var mask = BigInt(0)
          for (byteId <- 0 until ip.byteCount) {
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
    def channelPopStream(channel : Int, portId : Int, sourceId : Int, sinkId : Int): Unit ={
      val channelAddress = channelToAddress(channel)
      ctrl.write(portId << 0 | sourceId << 8 | sinkId << 16, channelAddress + 0x18)
      ctrl.write(0, channelAddress + 0x1C)
    }
    def channelStart(channel : Int, bytes : BigInt): Unit ={
      val channelAddress = channelToAddress(channel)
      ctrl.write(bytes-1, channelAddress + 0x20)
      ctrl.write(1, channelAddress+0x2C)
    }
    def channelConfig( channel : Int,
                       fifoBase : Int,
                       fifoWords : Int,
                       priority : Int): Unit ={
      val channelAddress = channelToAddress(channel)
      ctrl.write(fifoBase << 0 | fifoWords-1 << 16,  channelAddress+0x30)
      ctrl.write(priority,  channelAddress+0x34)
    }
    def channelWaitDone( channel : Int): Unit ={
      val channelAddress = channelToAddress(channel)
      while((ctrl.read(channelAddress+0x2C) & 1) != 0){
        dut.clockDomain.waitSampling(Random.nextInt(50))
      }
    }

    val channelAgent = for((channel, channelId) <- dut.p.channels.zipWithIndex) yield fork {
      val cp = dut.p.channels(channelId)
      val M2M, M2S, S2M = new Object
      for (r <- 0 until 100) {
        List(M2M, M2S, S2M).randomPick() match {
          case S2M => {
            val to = 0x100
            val bytes = 0x40
            val inputId = Random.nextInt(dut.p.outputs.size)
            val ip = dut.p.inputs(inputId)
            val source = Random.nextInt(1 << ip.sourceWidth)
            val sink = Random.nextInt(1 << ip.sinkWidth)

            val packet = Packet(source = source, sink = sink)
            for (byteId <- 0 until bytes) {
              val value = Random.nextInt & 0xFF
              writesAllowed(to + byteId) = value.toByte
              packet.data.enqueue(value)
            }

            channelPushStream(channelId, inputId, source, sink)
            channelPopMemory(channelId, to, 16)
            channelConfig(channelId, 0x100, 0x40, 2)
            channelStart(channelId, bytes = bytes)
            inputs(inputId).packets += packet
            channelWaitDone(channelId)
          }
          case M2M => {
            val from = Random.nextInt(0x1000) & ~0x3
            val to = 0x1000 + Random.nextInt(0x1000) & ~0x3
            val bytes = (Random.nextInt(0x100) + 0x10) / 16 * 16
            for (byteId <- 0 until bytes) {
              writesAllowed(to + byteId) = memory.memory.read(from + byteId)
            }
            channelPushMemory(channelId, from, 16)
            channelPopMemory(channelId, to, 16)
            channelConfig(channelId, 0x100, 0x40, 2)
            channelStart(channelId, bytes = bytes)
            channelWaitDone(channelId)
          }
          case M2S => {
            val from = Random.nextInt(0x100)
            val bytes = Random.nextInt(0x100) + 1
            val outputId = Random.nextInt(dut.p.outputs.size)
            val op = dut.p.outputs(outputId)
            val source = Random.nextInt(1 << op.sourceWidth)
            val sink = Random.nextInt(1 << op.sinkWidth)

            for (byteId <- 0 until bytes) {
              outputs(outputId).ref.enqueue((memory.memory.read(from + byteId), source, sink))
            }
            channelPushMemory(channelId, from, 16)
            channelPopStream(channelId, outputId, source, sink)
            channelConfig(channelId, 0x100, 0x40, 2)
            channelStart(channelId, bytes = bytes)
            channelWaitDone(channelId)
          }
        }
      }
    }


    channelAgent.foreach(_.join())
    dut.clockDomain.waitSampling(1000)
  }
}