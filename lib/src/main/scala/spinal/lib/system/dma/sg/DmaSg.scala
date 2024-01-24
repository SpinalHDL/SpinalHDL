package spinal.lib.system.dma.sg

import spinal.core.{Bool, _}
import spinal.core.sim.{SimDataPimper, SimMutex, fork, waitUntil}
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
import scala.collection.Seq

object DmaSg{
  val ctrlAddressWidth = 16
  val descriptorSize = 32
  def getCtrlCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = ctrlAddressWidth,
    dataWidth = 32
  )

  def getReadRequirements(p : Parameter) = BmbAccessParameter(
    addressWidth = p.readAddressWidth,
    dataWidth    = p.readDataWidth
  ).addSources(p.channels.count(_.canRead), BmbSourceParameter(
    contextWidth = widthOf(p.ReadContext()),
    lengthWidth = p.readLengthWidth,
    canWrite = false,
    alignment = BmbParameter.BurstAlignement.BYTE
  ))

  def getWriteRequirements(p : Parameter) = BmbAccessParameter(
    addressWidth = p.writeAddressWidth,
    dataWidth    = p.writeDataWidth
  ).addSources(p.channels.count(_.canWrite), BmbSourceParameter(
    contextWidth = widthOf(p.WriteContext()),
    lengthWidth = p.writeLengthWidth,
    canRead = false,
    alignment = BmbParameter.BurstAlignement.BYTE
  ))

  def getSgReadRequirements(p : Parameter) = BmbAccessParameter(
    addressWidth = p.sgAddressWidth,
    dataWidth    = p.sgReadDataWidth
  ).addSources(1, BmbSourceParameter(
    contextWidth = widthOf(p.SgReadContext()),
    lengthWidth = log2Up(descriptorSize),
    canWrite = false,
    alignment = BmbParameter.BurstAlignement.LENGTH
  ))

  def getSgWriteRequirements(p : Parameter) = BmbAccessParameter(
    addressWidth = p.sgAddressWidth,
    dataWidth    = p.sgWriteDataWidth
  ).addSources(1, BmbSourceParameter(
    contextWidth = widthOf(p.SgWriteContext()),
    lengthWidth = 2,
    canRead = false,
    alignment = BmbParameter.BurstAlignement.LENGTH
  ))

  case class Parameter(readAddressWidth : Int,
                       readDataWidth : Int,
                       readLengthWidth : Int,
                       writeAddressWidth : Int,
                       writeDataWidth : Int,
                       writeLengthWidth : Int,
                       sgAddressWidth : Int,
                       sgReadDataWidth : Int,
                       sgWriteDataWidth : Int,
                       memory : DmaMemoryLayout,
                       outputs : Seq[BsbParameter],
                       inputs : Seq[BsbParameter],
                       channels : Seq[Channel],
                       bytePerTransferWidth : Int,
                       weightWidth : Int,
                       withSgBus : Boolean = false,
                       pendingWritePerChannel : Int = 15,
                       pendingReadPerChannel : Int = 15){

    def toSgBusParameter() = SgBusParameter(addressWidth, bytePerTransferWidth, channels.size)
    val readWriteMinDataWidth = Math.min(readDataWidth, writeDataWidth)
    val readWriteMaxDataWidth = Math.max(readDataWidth, writeDataWidth)
    val writeByteCount = writeDataWidth/8
    val addressWidth = Math.min(readAddressWidth, writeAddressWidth)
    assert(outputs.map(_.byteCount*8).fold(0)(Math.max) <= readWriteMinDataWidth)
    assert(inputs.map(_.byteCount*8).fold(0)(Math.max) <= readWriteMinDataWidth)
    assert(readWriteMaxDataWidth <= memory.bankCount*memory.bankWidth)

    def canRead = channels.exists(_.canRead)
    def canWrite = channels.exists(_.canWrite)
    def canSgRead = channels.exists(_.linkedListCapable)
    def canSgWrite = channels.exists(_.linkedListCapable)

    case class ReadContext() extends Bundle{
      val channel = UInt(log2Up(channels.count(_.canRead)) bits)
      val start, stop = UInt(log2Up(readDataWidth/8) bits)
      val length = UInt(readLengthWidth bits)
      val last = Bool()
    }

    case class WriteContext() extends Bundle{
      val channel = UInt(log2Up(channels.count(_.canWrite)) bits)
      val length = UInt(writeLengthWidth bits)
      val doPacketSync = Bool()
    }

    case class SgReadContext() extends Bundle{
      val channel =  UInt(log2Up(channels.count(_.linkedListCapable)) bits)
    }

    case class SgWriteContext() extends Bundle{
      val channel =  UInt(log2Up(channels.count(_.linkedListCapable)) bits)
    }
  }

  case class SgBusParameter(addressWidth : Int, bytePerTransferWidth : Int, channels : Int)

  case class SgReadCmd(p : SgBusParameter) extends Bundle {
    val channelId = UInt(log2Up(p.channels) bits)
  }

  case class SgReadRsp(p : SgBusParameter) extends Bundle{
    val channelId = UInt(log2Up(p.channels) bits)
    val srcAddress = UInt(p.addressWidth bits)
    val dstAddress = UInt(p.addressWidth bits)
    val bytes = UInt(p.bytePerTransferWidth bits)
    val last = Bool()
    val stall = Bool()
  }

  case class SgRead(p: SgBusParameter) extends Bundle with IMasterSlave{
    val cmd = Stream(SgReadCmd(p))
    val rsp = Flow(SgReadRsp(p))

    override def asMaster(): Unit = {
      master(cmd)
      slave(rsp)
    }
  }

  case class SgWriteCmd(p: SgBusParameter) extends Bundle {
    val channelId = UInt(log2Up(p.channels) bits)
    val bytesDone = UInt(p.bytePerTransferWidth+1 bits)
    val endOfPacket = Bool()
    val completed = Bool()
  }

  case class SgWrite(p: SgBusParameter) extends Bundle with IMasterSlave {
    val cmd = Stream(SgWriteCmd(p))
    override def asMaster(): Unit = {
      master(cmd)
    }
  }

  case class SgBus(p : SgBusParameter) extends Bundle with IMasterSlave{
    val write = SgWrite(p)
    val read = SgRead(p)

    override def asMaster(): Unit = {
      master(write, read)
    }
  }

  case class Channel(memoryToMemory : Boolean,
                     inputsPorts : Seq[Int],
                     outputsPorts : Seq[Int],
                     linkedListCapable : Boolean,
                     directCtrlCapable : Boolean,
                     selfRestartCapable : Boolean,
                     linkedListFromMemory: Boolean = true,
                     linkedListFromSg: Boolean = true,
                     progressProbes : Boolean,
                     halfCompletionInterrupt : Boolean,
                     bytePerBurst : Option[Int] = None,
                     fifoMapping : Option[(Int, Int)] = None,
                     name : Option[String] = None) extends OverridedEqualsHashCode {
    def canRead = memoryToMemory || outputsPorts.nonEmpty
    def canWrite = memoryToMemory || inputsPorts.nonEmpty
    def canInput = inputsPorts.nonEmpty
    def canOutput = outputsPorts.nonEmpty

    assert(linkedListCapable || directCtrlCapable, "A DMA channel should be at least controllable via a linked list or direct access")
    assert(!(!directCtrlCapable && selfRestartCapable), "Channel self restart is only available if direct controle is enabled")
    assert(memoryToMemory || inputsPorts.nonEmpty || outputsPorts.nonEmpty, "A DMA channel require at least one opperation (m->m, m->s, s->m)")
    val withProgressCounter = progressProbes || halfCompletionInterrupt || linkedListCapable && canInput
    val withProgressCounterM2s = progressProbes || halfCompletionInterrupt

    bytePerBurst match {
      case Some(x) => assert(isPow2(x), "Channel byte per burst should be power of 2")
      case None =>
    }
    fifoMapping match {
      case Some((base, size)) => assert(isPow2(size), "Channel buffer size should be power of two"); assert(base % size == 0, "Channel buffer address should be aligned to its size (address % size == 0)")
      case None =>
    }
  }

  case class ChannelIo(p : Channel) extends Bundle{
    val interrupt = out Bool()
  }

  case class Core[CTRL <: Data with IMasterSlave](p : Parameter, ctrlType : HardType[CTRL], slaveFactory : CTRL => BusSlaveFactory) extends Component{
    val io = new Bundle {
      val sgRead = p.canSgRead generate master(Bmb(getSgReadRequirements(p)))
      val sgWrite = p.canSgWrite generate master(Bmb(getSgWriteRequirements(p)))
      val read = p.canRead generate master(Bmb(getReadRequirements(p)))
      val write = p.canWrite generate master(Bmb(getWriteRequirements(p)))
      val outputs = Vec(p.outputs.map(s => master(Bsb(s))))
      val inputs = Vec(p.inputs.map(s => slave(Bsb(s))))
      val interrupts = out Bits(p.channels.size bits)
      val ctrl = slave(ctrlType())
      val sg = p.withSgBus generate master(SgBus(p.toSgBusParameter()))
    }

    val ctrl = slaveFactory(io.ctrl)

    val internalMemoryBytes = p.memory.bankWidth/8*p.memory.bankWords*p.memory.bankCount
    val ptrWidth = log2Up(p.memory.bankWords*p.memory.bankCount) + 1
    val ptrType = HardType(UInt(ptrWidth bits))

    case class InputContext(ip : BsbParameter, portId : Int) extends Bundle {
      val channel = Bits(p.channels.count(_.inputsPorts.contains(portId)) bits)
      val bytes = UInt(log2Up(ip.byteCount + 1) bits)
      val flush = Bool()
      val packet = Bool()
    }
    case class M2bWriteContext() extends Bundle{
      val last = Bool()
      val lastOfBurst = Bool()
      val channel = UInt(log2Up(p.channels.count(_.canRead)) bits)
      val loadByteInNextBeat = UInt(log2Up(p.readDataWidth/8 + 1) bits)
    }
    case class B2sReadContext(portId : Int) extends Bundle {
      val channel = Bits(p.channels.count(_.outputsPorts.contains(portId)) bits)
      val veryLast = Bool()
      val endPacket = Bool()
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
      val enable = Reg(Bool()) init(False)
      val valid = Reg(Bool()) init(False) setWhen(fire) clearWhen(!enable)
    }

    def bytesType = UInt(log2Up(p.memory.bankWords*p.memory.bankCount*p.memory.bankWidth/8+1) bits)

    class ChannelLogic(val id : Int) extends Area{
      val cp = p.channels(id)

      val channelStart = False
      val channelStop = Reg(Bool())
      val channelCompletion = False
      val channelValid = RegInit(False) setWhen(channelStart) clearWhen(channelCompletion)

      val descriptorStart = False
      val descriptorCompletion = False
      val descriptorValid = RegInit(False) setWhen(descriptorStart) clearWhen(descriptorCompletion)

      val bytes = Reg(UInt(p.bytePerTransferWidth bits)) //minus one
      val priority = Reg(UInt(p.memory.priorityWidth bits)) init(0)
      val weight = Reg(UInt(p.weightWidth bits)) init(0)
      val selfRestart = cp.selfRestartCapable generate Reg(Bool())
      val readyToStop = True //todo Check s2b b2s transiants



      val bytesProbe = (cp.withProgressCounter) generate new Area{
        val value = Reg(UInt(p.bytePerTransferWidth + 1 bits)).simPublic()
        val incr = Flow(UInt(Math.max(p.writeLengthWidth, p.readLengthWidth) bits))
        incr.valid := False
        incr.payload.assignDontCare()
        when(incr.valid){
          value := value + incr.payload + 1
        }
      }

      val ctrl = cp.directCtrlCapable generate new Area{
        val kick = RegNext(False) init(False)
        when(kick){
          descriptorStart := True
        }
        when(channelCompletion){
          kick := False
        }
      }

      val ll = cp.linkedListCapable generate new Area{
        val sgStart = False
        val valid = RegInit(False)
        val onSgStream = RegInit(False)
        val head = Reg(Bool())
        val justASync = Reg(Bool())
        val waitDone = Reg(Bool())
        val readDone = Reg(Bool())
        val writeDone = Reg(Bool())
        val gotDescriptorStall = Reg(Bool())
        val controlNoCompletion = Reg(Bool())
        val packet = Reg(Bool()) clearWhen(descriptorStart)
        val requireSync = Reg(Bool()) clearWhen(descriptorStart)
        val ptr, ptrNext = Reg(UInt(p.addressWidth bits))

        val requestLl = channelValid && valid && !channelStop && !waitDone && (!descriptorValid || requireSync)

        val descriptorUpdated = False
        when(valid && waitDone && writeDone && readDone){
          waitDone := False
          when(!justASync) {
            head := False
            when(!gotDescriptorStall) {
              descriptorStart := True
            } otherwise {
              valid := False
            }
          }
          when(!head){
            descriptorUpdated := True
          }
        }

        when(sgStart){
          valid := True
        }
        when(channelStart){
          waitDone := False
          head := True
        }

        when(waitDone){
          readyToStop := False
        }
        when(channelCompletion){
          valid := False
        }
      }



      val fifo = new Area{
        val (base, words) = cp.fifoMapping match {
          case None => (Reg(ptrType),Reg(ptrType))
          case Some((x,y)) => {
            val n = cp.name.getOrElse("")
            assert(x < internalMemoryBytes && x+y <= internalMemoryBytes, f"The channel $n buffer range, 0x$x%x:0x${x+y-1}%x isn't contained in the internal memory space (0x0:0x${internalMemoryBytes-1}%x)")
            (U(x*8/p.memory.bankWidth, ptrWidth bits), U(y*8/p.memory.bankWidth-1, ptrWidth bits))
          }
        }

        val push = new Area {
          val available = Reg(ptrType)
          val availableDecr = ptrType()
          availableDecr := 0

          val ptr = Reg(ptrType)
          val ptrWithBase = (base & ~words) | (ptr & words)
          val ptrIncr = DataOr(ptrType)
          ptr := ptr + ptrIncr.value

          when(channelStart){
            ptr := 0
          }
        }

        val pop = new Area {
          val ptr = Reg(ptrType)
          val bytes = bytesType
          val ptrWithBase = (base & ~words) | (ptr.resized & words)

          val bytesIncr = DataOr(bytesType)
          val bytesDecr = DataOr(bytesType)




          val empty = ptr === push.ptr


          val ptrIncr = DataOr(ptrType)
          ptr := ptr + ptrIncr.value

          val withoutOverride = !cp.canInput generate new Area{
            val exposed = Reg(bytesType)
            exposed := exposed + bytesIncr.value - bytesDecr.value
            bytes := exposed
            when(channelStart){
              exposed := 0
            }
          }

          val withOverride = cp.canInput generate new Area{
            val backup = Reg(bytesType)
            val backupNext = backup + bytesIncr.value - bytesDecr.value
            backup := backupNext

            val load, unload = False
            val exposed = Reg(bytesType)
            val valid = Reg(Bool()) clearWhen(channelStart || unload) setWhen(load)
            exposed := (!valid ? backupNext | (exposed - bytesDecr.value))

            bytes := exposed
            when(channelStart){
              backup := 0
              valid := False
            }
          }

        }

        val empty = push.ptr === pop.ptr

      }


      val push = new Area{
        val memory  = Reg(Bool())


        val m2b = cp.canRead generate new Area {
          val address = Reg(UInt(io.read.p.access.addressWidth bits))
          val bytePerBurst = cp.bytePerBurst match {
            case None => Reg(UInt(io.read.p.access.lengthWidth bits))  //minus one
            case Some(x) => U(x-1, io.read.p.access.lengthWidth bits)
          }

          val loadDone = RegInit(True)
          val bytesLeft = Reg(UInt(p.bytePerTransferWidth bits)) //minus one
          val memPending = Reg(UInt(log2Up(p.pendingReadPerChannel + 1) bits)) init(0)
          val memPendingIncr, memPendingDecr = False
          memPending := memPending + U(memPendingIncr) - U(memPendingDecr)

          val loadRequest = descriptorValid && !channelStop && !loadDone && memory && fifo.push.available > (bytePerBurst >> log2Up(p.memory.bankWidth/8)) && memPending =/= p.pendingReadPerChannel

          when(descriptorStart){
            bytesLeft := bytes
            loadDone := False
          }
        }

        val s2b = cp.inputsPorts.nonEmpty generate new Area {
          val portId = Reg(UInt(log2Up(cp.inputsPorts.size) bits))
          val completionOnLast = Reg(Bool())
          val packetEvent = False
          val packetLock = Reg(Bool()) //Maybe this should be disable if not necessary
          val waitFirst = Reg(Bool())
          fifo.pop.withOverride.load setWhen( packetEvent && completionOnLast)

          when(channelStart){
            packetLock := False
          }
        }
      }

      val pop = new Area{
        val memory  = Reg(Bool())
        val b2s = cp.canOutput generate new Area{
          val last  = Reg(Bool())
          val portId = Reg(UInt(log2Up(p.outputs.size) bits))
          val sourceId = Reg(UInt(cp.outputsPorts.map(p.outputs(_).sourceWidth).max bits))
          val sinkId = Reg(UInt(cp.outputsPorts.map(p.outputs(_).sinkWidth).max bits))

          val veryLastTrigger = False
          val veryLastValid = Reg(Bool()) setWhen(veryLastTrigger)
          val veryLastPtr = Reg(ptrType)
          val veryLastEndPacket = Reg(Bool())

          when(veryLastTrigger){
            veryLastPtr := fifo.push.ptrWithBase
            veryLastEndPacket := last
          }
          //Memory to stream => descriptor completion when memory is done
          when(descriptorValid && !memory && push.memory && push.m2b.loadDone && push.m2b.memPending === 0){
            descriptorCompletion := True
          }
          when(!memory && veryLastValid && push.m2b.bytesLeft <= push.m2b.bytePerBurst){
            push.m2b.loadRequest := False
          }
          when(channelStart){
            veryLastValid := False
          }
        }


        val b2m = cp.canWrite generate new Area{
          val bytePerBurst = cp.bytePerBurst match {
            case None => Reg(UInt(io.write.p.access.lengthWidth bits))  //minus one
            case Some(x) => U(x-1, io.write.p.access.lengthWidth bits)
          }

          val fire = False
          val waitFinalRsp = Reg(Bool())
          val flush = Reg(Bool()) clearWhen(fire)  //Check flush
          val packetSync = False
          val packet = Reg(Bool()) clearWhen(channelStart || fire)
          val memRsp = False
          val memPending = Reg(UInt(log2Up(p.pendingWritePerChannel + 1) bits)) init(0)
          val address = Reg(UInt(io.write.p.access.addressWidth bits))
          val bytesLeft = Reg(UInt(p.bytePerTransferWidth+1 bits)) //minus one

          // Trigger request when there is enough to do a burst, fifo occupancy > 50 %, flush
          val request = descriptorValid && !channelStop && !waitFinalRsp && memory && (fifo.pop.bytes > bytePerBurst || (fifo.push.available < (fifo.words >> 1) || flush)) && fifo.pop.bytes =/= 0 && memPending =/= p.pendingWritePerChannel
          val bytesToSkip = Reg(UInt(log2Up(p.writeByteCount) bits))

          val decrBytes = fifo.pop.bytesDecr.newPort()

          val memPendingInc = False
          memPending := memPending + U(memPendingInc) - U(memRsp)

          decrBytes := 0


          when(bytesLeft < fifo.pop.bytes){
            flush := True
          }

          when(memPending === 0 && fifo.pop.bytes === 0){ //TODO bouarf
            flush := False
            packet := False
            packetSync setWhen(packet)
          }

          if(cp.canInput) when(packetSync){
            fifo.pop.withOverride.unload := True
            push.s2b.packetLock := False
            when(descriptorValid && !push.memory){
              when(push.s2b.completionOnLast){
                descriptorCompletion := True
              } otherwise{
                if(cp.linkedListCapable) when(!waitFinalRsp){
                  ll.requireSync := True
                }
              }

              if(cp.linkedListCapable) {
                ll.packet := True
              }
            }
          }

          when(descriptorValid && memPending === 0 && waitFinalRsp){
            descriptorCompletion := True
          }
          when(channelStart){
            bytesToSkip := 0
            flush := False
          }
          when(descriptorStart){
            bytesLeft := bytes.resized
            waitFinalRsp := False
          }
        }
      }


      if(cp.canRead) readyToStop.clearWhen(push.m2b.memPending =/= 0)
      if(cp.canWrite) readyToStop.clearWhen(pop.b2m.memPending =/= 0)

      val readyForChannelCompletion = True
      if(cp.canOutput) readyForChannelCompletion.clearWhen(!pop.memory && !fifo.pop.empty)

      when(channelValid) {
        when(channelStop) {
          when(readyToStop){
            channelCompletion := True
            descriptorCompletion := True
          }
        } otherwise {
          when(!descriptorValid) {
            val noMoreDescriptor = True
            if(cp.selfRestartCapable) {
              when(selfRestart && !ctrl.kick) {
                noMoreDescriptor := False
                descriptorStart := True
                if (cp.canWrite) pop.b2m.address := pop.b2m.address - bytes - 1
                if (cp.canRead) push.m2b.address := push.m2b.address - bytes - 1
              }
            }
            if(cp.directCtrlCapable){
              when(ctrl.kick){
                noMoreDescriptor := False
              }
            }
            if(cp.linkedListCapable) when(ll.valid){
              noMoreDescriptor := False
            }
            when(noMoreDescriptor && readyForChannelCompletion){
              channelStop := True
            }
          }
        }
      }

      val s2b = new Area{
        val full = fifo.push.available < p.memory.bankCount
      }

      fifo.push.available := fifo.push.available + RegNext(fifo.pop.ptrIncr.value) - Mux(push.memory, fifo.push.availableDecr, fifo.push.ptrIncr.value)


      val interrupts = new Area{
        val completion = new Interrupt(descriptorValid && descriptorCompletion)
        val halfCompletion = cp.halfCompletionInterrupt generate new Area{
          val trigger = bytesProbe.value > (bytes >> 1)
          val interrupt = new Interrupt(channelValid && trigger)
        }
        val onChannelCompletion = new Interrupt(channelValid && channelCompletion)
        val onLinkedListUpdate = cp.linkedListCapable generate new Interrupt(ll.descriptorUpdated)

        val s2mPacket = cp.canInput generate new Interrupt(pop.b2m.packetSync)
      }


      when(channelStart){
        fifo.push.ptr := 0
        fifo.push.available := fifo.words + 1
        fifo.pop.ptr := 0
      }
      if(cp.withProgressCounter) when(channelStart || descriptorStart){
         bytesProbe.value := 0
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
        val firsts = sink.firstSink
        val first = firsts(sink.sink)
        val channelsOh = B(channels.map(c => c.channelValid && (first || !c.push.s2b.waitFirst) && !c.push.memory && c.push.s2b.portId === c.cp.inputsPorts.indexOf(portId) && sink.sink === channels.indexOf(c)))
        val noHit = !channelsOh.orR
        val channelsFull = B(channels.map(c => c.s2b.full || c.push.s2b.packetLock && sink.last))
        val sinkHalted = sink.throwWhen(noHit).haltWhen((channelsOh & channelsFull).orR)
        val byteCount = CountOne(sinkHalted.mask)
        val context = InputContext(ps, portId)
        context.channel := channelsOh
        context.bytes := byteCount
        context.flush := sink.last
        context.packet := sink.last
        sinkHalted.ready     := memoryPort.cmd.ready
        memoryPort.cmd.valid := sinkHalted.valid
        memoryPort.cmd.address := MuxOH(channelsOh, channels.map(_.fifo.push.ptrWithBase)).resized
        memoryPort.cmd.data := sinkHalted.data
        memoryPort.cmd.mask := sinkHalted.mask
        memoryPort.cmd.priority := MuxOH(channelsOh, channels.map(_.priority))
        memoryPort.cmd.context := B(context)
        for ((channel, ohId) <- channels.zipWithIndex) {
          val hit = channelsOh(ohId) && memoryPort.cmd.fire
          channel.fifo.push.ptrIncr.newPort() := ((hit && memoryPort.cmd.mask.orR) ? U(bankPerBeat) | U(0)).resized
          when(hit){
            channel.push.s2b.waitFirst := False
            when(sink.last) {
              channel.push.s2b.packetLock := True
            }
          }
        }
      }

      val rsp = new Area{
        val context = memoryPort.rsp.context.as(InputContext(ps, portId))
        for ((channel, ohId) <- channels.zipWithIndex) {
          val hit = memoryPort.rsp.fire && context.channel(ohId)
          channel.fifo.pop.bytesIncr.newPort := (hit ? context.bytes | U(0)).resized
          channel.push.s2b.packetEvent setWhen(hit && context.packet)
          if(channel.cp.canWrite) {
            channel.pop.b2m.flush setWhen(hit && context.flush)
            channel.pop.b2m.packet setWhen(hit && context.packet)
          }
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
        val channelsOh = B(OHMasking.first(channels.map(c => c.channelValid && !c.pop.memory && c.pop.b2s.portId === c.cp.outputsPorts.indexOf(portId) && !c.fifo.pop.empty)))
        val context = B2sReadContext(portId)
        val groupRange = log2Up(p.readDataWidth/p.memory.bankWidth) -1 downto log2Up(bankPerBeat)
        val addressRange = ptrWidth-1 downto log2Up(p.readDataWidth/p.memory.bankWidth)
        val veryLastPtr = MuxOH(channelsOh, channels.map(_.pop.b2s.veryLastPtr))
        val address = MuxOH(channelsOh, channels.map(_.fifo.pop.ptrWithBase))
        context.channel := channelsOh
        context.veryLast :=  MuxOH(channelsOh, channels.map(_.pop.b2s.veryLastValid)) && address(addressRange) === veryLastPtr(addressRange) && address(groupRange) === (1 << groupRange.size)-1
        context.endPacket :=  MuxOH(channelsOh, channels.map(_.pop.b2s.veryLastEndPacket))

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
        source.source := MuxOH(context.channel, channels.map(_.pop.b2s.sourceId)).resized
        source.last   := context.veryLast && context.endPacket
        when(source.fire && context.veryLast){
          for((channel, ohId) <- channels.zipWithIndex) when(context.channel(ohId)){
            channel.pop.b2s.veryLastValid := False
          }
        }
      }
    }


    abstract class ArbiterLogic(channels : Seq[ChannelLogic]) extends Area {
      def requestFrom(c : ChannelLogic) : Bool
      def acceptFrom(c : ChannelLogic) : Unit

      val valid = RegInit(False)
      val chosen = Reg(UInt(log2Up(channels.size) bits))

      val priority = new Area{
        def TB2[T <: Data, T2 <: Data](a: T, b : T2) = {
          val ret = TupleBundle2(a,b)
          ret._1 := a
          ret._2 := b
          ret
        }
        val highest = channels.map(c => TB2(requestFrom(c), c.priority)).reduceBalancedTree((a,b) => (!b._1 || (a._1 && a._2 > b._2)) ? a | b)._2
        val masked = B(channels.map(c => requestFrom(c) && c.priority === highest))
        val roundRobins = Vec(Reg(Bits(channels.size bits)) init(1), 1 << p.memory.priorityWidth)
        val counter = Reg(UInt(p.weightWidth bits)) init(0)
        val chosenOh = OHMasking.roundRobin(masked, roundRobins(highest))
        val chosen = OHToUInt(chosenOh)
        val weightLast = channels.map(_.weight === counter).read(chosen)
        val contextNext = weightLast ? chosenOh.rotateLeft(1) | chosenOh
      }

      when(!valid) {
        chosen := priority.chosen
        when(channels.map(requestFrom(_)).orR) {
          valid := True

          for(roundRobinId <- 0 until 1 << p.memory.priorityWidth) when(roundRobinId === priority.highest){
            priority.roundRobins(roundRobinId) := priority.contextNext
          }

          priority.counter := (priority.counter + 1).resized
          when(priority.weightLast){
            priority.counter := 0
          }
        }

        for(id <- 0 until channels.length) when(requestFrom(channels(id)) && priority.chosenOh(id)){
          acceptFrom(channels(id))
        }
      }
    }

    val m2b = p.canRead generate new Area {
      val channels = Core.this.channels.filter(_.cp.canRead)
      val cmd = new Area {
        val s0 = new ArbiterLogic(channels) {
          def requestFrom(c : ChannelLogic) : Bool = c.push.m2b.loadRequest
          def acceptFrom(c : ChannelLogic) : Unit = c.push.m2b.memPendingIncr := True

          def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(chosen)
          val address = channel(_.push.m2b.address)
          val bytesLeft = channel(_.push.m2b.bytesLeft)
          val readAddressBurstRange = address(io.read.p.access.lengthWidth-1 downto 0) //address(log2Up(io.read.p.access.byteCount) downto log2Up(io.read.p.access.byteCount))
          val lengthHead = ~readAddressBurstRange & channel(_.push.m2b.bytePerBurst)
          val length = lengthHead.min(bytesLeft).resize(io.read.p.access.lengthWidth)
          val lastBurst = bytesLeft === length
        }

        val s1 = new Area{
          def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(s0.chosen)
          val valid = RegInit(False) setWhen(s0.valid)

          val address = RegNext(s0.address)
          val length = RegNext(s0.length)
          val lastBurst = RegNext(s0.lastBurst)
          val bytesLeft = RegNext(s0.bytesLeft)
          val context = p.ReadContext()
          context.channel := s0.chosen
          context.start := address.resized
          context.stop := (address + length).resized
          context.last := lastBurst
          context.length := length

          io.read.cmd.valid := False
          io.read.cmd.last := True
          io.read.cmd.source := s0.chosen
          io.read.cmd.opcode := Bmb.Cmd.Opcode.READ
          io.read.cmd.address := address
          io.read.cmd.length  := length
          io.read.cmd.context := B(context)


          val addressNext = address + length + 1
          val byteLeftNext = bytesLeft - length - 1
          val fifoPushDecr = ((address(log2Up(io.read.p.access.byteCount)-1 downto 0) + io.read.cmd.length | (io.read.p.access.byteCount-1))+^1 >> log2Up(p.memory.bankWidth/8)).resized

          when(valid) {
            io.read.cmd.valid := True
            when(io.read.cmd.ready) {
              s0.valid := False
              valid := False
              for((channel, ohId) <- channels.zipWithIndex) when(ohId === s0.chosen){
                channel.push.m2b.address := addressNext
                channel.push.m2b.bytesLeft :=   byteLeftNext
                channel.fifo.push.availableDecr := fifoPushDecr
                when(lastBurst) {
                  channel.push.m2b.loadDone := True
                }
              }
            }
          }
        }
      }

      val rsp = new Area {
        val context = io.read.rsp.context.as(p.ReadContext())

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
        writeContext.lastOfBurst := io.read.rsp.last
        writeContext.channel := context.channel
        writeContext.loadByteInNextBeat := ((last ? context.stop | context.stop.maxValue) -^ (first ? context.start | 0))
        memory.ports.m2b.cmd.address := channel(_.fifo.push.ptrWithBase).resized
        memory.ports.m2b.cmd.arbitrationFrom(io.read.rsp)
        memory.ports.m2b.cmd.data := io.read.rsp.data
        memory.ports.m2b.cmd.context := B(writeContext)

        for ((channel, ohId) <- channels.zipWithIndex) {
          val fire = memory.ports.m2b.cmd.fire && context.channel === ohId
          channel.fifo.push.ptrIncr.newPort := (fire ? U(io.read.p.access.dataWidth / p.memory.bankWidth) | U(0)).resized
          if(channel.cp.withProgressCounterM2s) when(fire && io.read.rsp.last){
            channel.bytesProbe.incr.valid := (if(channel.cp.canWrite) !channel.pop.memory else True)
            channel.bytesProbe.incr.payload := context.length
          }
        }
      }

      val writeRsp = new Area{
        val context = memory.ports.m2b.rsp.context.as(M2bWriteContext())
        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(context.channel)
        when(memory.ports.m2b.rsp.fire && context.last) {
          for((channel, ohId) <- channels.zipWithIndex) when(context.channel === ohId){
            if(channel.cp.canWrite) channel.pop.b2m.flush := True
          }
        }
        for ((channel, ohId) <- channels.zipWithIndex) {
          val fire = memory.ports.m2b.rsp.fire && context.channel === ohId
          channel.fifo.pop.bytesIncr.newPort := (fire ? (context.loadByteInNextBeat + 1) | U(0)).resized

          when(fire && context.lastOfBurst){
            channel.push.m2b.memPendingDecr := True
          }
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
          val bytePerBurst = Reg(UInt(io.write.p.access.lengthWidth bits))
          val bytesInBurst = Reg(UInt(io.write.p.access.lengthWidth bits))
          val bytesInFifo = Reg(bytesType)
          val address = Reg(UInt(p.writeAddressWidth bits))
          val ptr = Reg(ptrType())
          val ptrMask = Reg(ptrType())
          val flush = Reg(Bool())
          val packet = Reg(Bool())
          val bytesLeft = Reg(UInt(p.bytePerTransferWidth bits)) //minus one
//          val commitFromBytePerBurst = Reg(Bool())
          def fire = valid && ready
          def isStall = valid && !ready
        }

        val arbiter = new Area{

          val logic = new ArbiterLogic(channels) {
            def requestFrom(c: ChannelLogic): Bool = c.pop.b2m.request
            def acceptFrom(c: ChannelLogic): Unit = {c.pop.b2m.memPendingInc := True}
          }

          def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(logic.chosen)
          when(sel.ready){
            sel.valid := False
            when(sel.valid) {logic.valid := False}
          }
          when(!sel.valid && logic.valid) {
            sel.valid := True
            sel.channel := logic.chosen
            sel.address := channel(_.pop.b2m.address)
            sel.ptr := channel(_.fifo.pop.ptrWithBase)
            sel.ptrMask := channel(_.fifo.words)
            sel.bytePerBurst := channel(_.pop.b2m.bytePerBurst)
            sel.bytesInFifo :=  channel(_.fifo.pop.bytes)
            sel.flush := channel(_.pop.b2m.flush)
            sel.packet := channel(_.pop.b2m.packet)
            sel.bytesLeft := channel(_.pop.b2m.bytesLeft).resized
            channel(_.pop.b2m.fire) := True
          }
        }

//        val arbiter = new Area {
//          val core = StreamArbiterFactory.roundRobin.noLock.build(NoData, channels.size)
//          (core.io.inputs, channels.map(_.pop.b2m.request)).zipped.foreach(_.valid := _)
//          core.io.output.ready := False
//          def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(core.io.chosen)
//          when(sel.ready){
//            sel.valid := False
//          }
//          when(!sel.valid && core.io.output.valid) {
//            core.io.output.ready := True
//            sel.valid := True
//            sel.channel := core.io.chosen
//            sel.address := channel(_.pop.b2m.address)
//            sel.ptr := channel(_.fifo.pop.ptrWithBase)
//            sel.ptrMask := channel(_.fifo.words)
//            sel.bytePerBurst := channel(_.pop.b2m.bytePerBurst)
//            sel.bytesInFifo :=  channel(_.fifo.pop.bytes)
//            sel.flush := channel(_.pop.b2m.flush)
//            sel.packet := channel(_.pop.b2m.packet)
//            sel.bytesLeft := channel(_.pop.b2m.bytesLeft).resized
//            channel(_.pop.b2m.fire) := True
//          }
//        }

//        val sel = arbiter.sel.halfPipe()
        def channel[T <: Data](f: ChannelLogic => T) = Vec(channels.map(f))(sel.channel)

        val bytesInBurstP1 = sel.bytesInBurst +^ 1
        val addressNext = sel.address + bytesInBurstP1
        val bytesLeftNext = sel.bytesLeft -^ bytesInBurstP1
        val isFinalCmd = bytesLeftNext.msb
        val beatCounter = Reg(UInt(io.write.p.access.lengthWidth - io.write.p.access.wordRangeLength  bits))

        //Assume that the delay of reading the memory and going throug the aggregator is at least two cycles
        val s0 = sel.valid.rise(False)
        val s1 = RegNext(s0) init(False)
        val s2 = RegInit(False) setWhen(s1) clearWhen(!sel.valid)

        when(s0){
          val addressBurstOffset = (sel.address.resized & sel.bytePerBurst)
          sel.bytesInBurst := ((sel.bytesInFifo-1).min(sel.bytesLeft).min(sel.bytePerBurst-addressBurstOffset)).resized
        }

        val fifoCompletion = sel.bytesInBurst === sel.bytesInFifo-1
        when(s1){
          beatCounter := sel.address(log2Up(io.write.p.access.byteCount)-1 downto 0) + sel.bytesInBurst >> log2Up(io.write.p.access.byteCount)
          for((channel, ohId) <- channels.zipWithIndex) when(sel.channel === ohId){
            channel.pop.b2m.decrBytes := bytesInBurstP1.resized
            channel.pop.b2m.address := addressNext
            channel.pop.b2m.bytesLeft := bytesLeftNext.resized
            channel.pop.b2m.waitFinalRsp setWhen(isFinalCmd)
            when(!fifoCompletion) {
              when(sel.flush) {
                channel.pop.b2m.flush := True
              }
              when(sel.packet) {
                channel.pop.b2m.packet := True
              }
            }
          }
        }


        val toggle = RegInit(False) toggleWhen(sel.fire)

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

          when(sel.valid && memory.ports.b2m.cmd.ready) {
            sel.ptr.getDrivingReg() := (sel.ptr & ~sel.ptrMask) | ((sel.ptr + U(io.write.p.access.dataWidth / p.memory.bankWidth) & sel.ptrMask))
          }
        }


        val aggregate = new Area {
          val context = memory.ports.b2m.rsp.context.as(FetchContext())
          val memoryPort = memory.ports.b2m.rsp.s2mPipe().throwWhen(context.toggle =/= toggle)


          val engine = Aggregator(AggregatorParameter(
            byteCount = p.writeByteCount,
            burstLength = p.writeLengthWidth,
            context = NoData()
          ))

          val first = Reg(Bool()) clearWhen(memoryPort.fire) setWhen(!sel.isStall)
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

          val maskFirstTrigger = address.resize(log2Up(p.writeByteCount) bits)
          val maskLastTriggerComb = maskFirstTrigger + sel.bytesInBurst.resized
          val maskLastTriggerReg = RegNext(maskLastTriggerComb)
          val maskLast = RegNext(B((0 until p.writeByteCount).map(byteId => byteId <= maskLastTriggerComb)))
          val maskFirst = B((0 until p.writeByteCount).map(byteId => byteId >= maskFirstTrigger))
          val enoughAggregation = s2 && sel.valid && !aggregate.engine.io.flush && (io.write.cmd.last ? ((aggregate.engine.io.output.mask & maskLast) === maskLast) | aggregate.engine.io.output.mask.andR)


          aggregate.engine.io.output.enough := enoughAggregation
          aggregate.engine.io.output.consume := io.write.cmd.fire
          aggregate.engine.io.output.lastByteUsed := maskLastTriggerReg

          io.write.cmd.valid := enoughAggregation
          io.write.cmd.last := beatCounter === 0
          io.write.cmd.address := address
          io.write.cmd.opcode := Bmb.Cmd.Opcode.WRITE
          io.write.cmd.data := aggregate.engine.io.output.data
          io.write.cmd.mask := ~((io.write.cmd.first ? ~maskFirst | B(0)) | (io.write.cmd.last ? ~maskLast| B(0)))
          io.write.cmd.length := sel.bytesInBurst
          io.write.cmd.source := sel.channel

          val doPtrIncr = sel.valid && (aggregate.engine.io.output.consumed || io.write.cmd.lastFire && aggregate.engine.io.output.usedUntil === aggregate.engine.io.output.usedUntil.maxValue)
          for((channel, ohId) <- channels.zipWithIndex){
            channel.fifo.pop.ptrIncr.newPort() := ((doPtrIncr && sel.channel === ohId) ? U(io.write.p.access.dataWidth/p.memory.bankWidth) | U(0)).resized
          }

          val context = p.WriteContext()
          context.channel := sel.channel
          context.length := sel.bytesInBurst
          context.doPacketSync := sel.packet && fifoCompletion
          io.write.cmd.context := B(context)

          when(io.write.cmd.fire){
            beatCounter := beatCounter - 1
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
          for((channel, ohId) <- channels.zipWithIndex) when(context.channel === ohId){
            if(channel.cp.withProgressCounter) {
              channel.bytesProbe.incr.valid := True
              channel.bytesProbe.incr.payload := context.length
            }
            when(context.doPacketSync) {
              channel.pop.b2m.packetSync := True
            }
          }
        }
      }
    }


    val ll = p.canSgRead generate new Area{
      val (channels, channelsId) = Core.this.channels.zipWithIndex.filter(_._1.cp.linkedListCapable).unzip
      val arbiter = new Area{
        val requests = channels.map(c => c.ll.requestLl)
        val oh = OHMasking.first(requests)
        val sel = OHMasking.first(requests)
        def channel[T <: Data](f: ChannelLogic => T, keep : ChannelLogic => Boolean = _ => true) = {
          val (ohFiltred, channelsFiltred) = (oh.toSeq, channels).zipped.filter((a,b) => keep(b))
          MuxOH(B(ohFiltred), channelsFiltred.map(f))
        }
        val head = channel(_.ll.head)
        val isJustASink = channel(_.descriptorValid)
        val doDescriptorStall = channel(c => !c.ll.controlNoCompletion || c.ll.gotDescriptorStall)
        val onSgStream = channel(_.ll.onSgStream)
      }

      val cmd = new Area{
        val valid = RegInit(False)
        def fromArbiter[T <: Data](f : ChannelLogic => T, keep : ChannelLogic => Boolean = _ => true) = RegNextWhen(arbiter.channel(f, keep), cond = !valid)
        val oh = RegNextWhen(arbiter.oh, !valid)
        val ptr = fromArbiter(_.ll.ptr)
        val ptrNext = fromArbiter(_.ll.ptrNext)
        val bytesDone = if(channels.exists(_.cp.canInput)) fromArbiter(_.bytesProbe.value, _.cp.canInput) else U(0)
        val endOfPacket = fromArbiter(_.ll.packet)
        val isJustASink = RegNextWhen(arbiter.isJustASink, !valid)
        val doDescriptorStall = RegNextWhen(arbiter.doDescriptorStall, !valid)
        val onSgStream = RegNextWhen(arbiter.onSgStream, !valid)

        val readFired, writeFired = Reg(Bool())

        when(!valid){
          valid setWhen(arbiter.oh.orR)
          oh := arbiter.oh
          for((channel, e) <- (channels, arbiter.oh).zipped) when(e){
            channel.ll.waitDone := True
            channel.ll.writeDone := arbiter.head
            channel.ll.justASync := arbiter.isJustASink
            channel.ll.packet := False
            channel.ll.requireSync := False
            when(!arbiter.isJustASink) {
              channel.ll.ptr := channel.ll.ptrNext
            }
            channel.ll.readDone := arbiter.isJustASink
          }
          readFired := arbiter.isJustASink
          writeFired := arbiter.head
        } otherwise{
          valid.clearWhen(writeFired && readFired)
        }

        val context = p.SgReadContext()
        context.channel := OHToUInt(oh)

        io.sgRead.cmd.valid := valid && !readFired && !onSgStream
        io.sgRead.cmd.last := True
        io.sgRead.cmd.address := ptrNext(ptrNext.high downto 5) @@ U"00000"
        io.sgRead.cmd.length := descriptorSize-1
        io.sgRead.cmd.opcode := Bmb.Cmd.Opcode.READ
        io.sgRead.cmd.context := B(context)

        io.sgWrite.cmd.valid := valid && !writeFired && !onSgStream
        io.sgWrite.cmd.last := True
        io.sgWrite.cmd.address := ptr(ptrNext.high downto 5) @@ U"00000"
        io.sgWrite.cmd.length := 3
        io.sgWrite.cmd.opcode := Bmb.Cmd.Opcode.WRITE
        io.sgWrite.cmd.context := B(context)

        val writeMaskSplit = io.sgWrite.cmd.mask.subdivideIn(4 bits)
        val writeDataSplit = io.sgWrite.cmd.data.subdivideIn(32 bits)
        writeMaskSplit.tail.foreach(_ := 0)
        writeDataSplit.tail.foreach(_.assignDontCare())

        writeMaskSplit.head := 0xF
        writeDataSplit.head := 0
        writeDataSplit.head(0, 27 bits) := B(bytesDone).resized
        writeDataSplit.head(30) := endOfPacket
        writeDataSplit.head(31) := !isJustASink && doDescriptorStall

        readFired setWhen(io.sgRead.cmd.fire)
        writeFired setWhen(io.sgWrite.cmd.fire)

        val sgStreamLogic = p.withSgBus generate new Area{
          val channelId = channelsId.map(U(_, log2Up(p.channels.size) bits)).reader(oh).apply(e => e)

          readFired setWhen (io.sg.read.cmd.fire)
          io.sg.read.cmd.valid := valid && !readFired && onSgStream
          io.sg.read.cmd.channelId := channelId

          writeFired setWhen (io.sg.write.cmd.fire)
          io.sg.write.cmd.valid := valid && !writeFired && onSgStream
          io.sg.write.cmd.channelId := channelId
          io.sg.write.cmd.bytesDone := bytesDone.resized
          io.sg.write.cmd.endOfPacket := endOfPacket
          io.sg.write.cmd.completed := !isJustASink && doDescriptorStall
        }
      }

      val readRsp = new Area{
        val context = io.sgRead.rsp.context.as(p.SgReadContext())
        val oh = UIntToOh(context.channel, width = channels.size).asBools

        val beatBytes = io.sgRead.p.access.byteCount
        val beatCount = descriptorSize/beatBytes
        val beatCounter = Reg(UInt(log2Up(beatCount) bits)) init(0)


        val statusOffset  = 0
        val controlOffset  = 4
        val pushOffset = 8
        val popOffset  = 16
        val nextOffset  = 24

        val completed = Reg(Bool())
        io.sgRead.rsp.ready := True
        when(io.sgRead.rsp.fire){
          beatCounter := (beatCounter + 1).resized

          def beatHit(offset : Int) = offset/beatBytes === beatCounter
          def mapChannel[T <: Data](f : ChannelLogic => T, gen : Channel => Boolean, byte : Int, bit : Int){
            val bitOffset = (byte % beatBytes)*8 + bit
            when(beatHit(byte)){
              for((channel, e) <- (channels, oh).zipped) if(gen(channel.cp) && channel.cp.linkedListFromMemory) when(e){
                val target = f(channel)
                target.assignFromBits(io.sgRead.rsp.data(bitOffset, widthOf(target) bits))
              }
            }
          }

          def map[T <: Data](that : T, byte : Int, bit : Int){
            val bitOffset = byte % beatBytes + bit
            when(beatHit(byte)){
              that.assignFromBits(io.sgRead.rsp.data(bitOffset, widthOf(that) bits))
            }
          }

          mapChannel(_.push.m2b.address, _.canRead, pushOffset, 0)
          mapChannel(_.pop.b2m.address, _.canWrite, popOffset, 0)
          mapChannel(_.ll.ptrNext, _ => true, nextOffset, 0)
          mapChannel(_.bytes, _ => true, controlOffset, 0)
          mapChannel(_.ll.controlNoCompletion, _ => true, controlOffset, 31)
          mapChannel(_.pop.b2s.last, _.canOutput, controlOffset, 30)
          mapChannel(_.ll.gotDescriptorStall, _ => true, statusOffset, 31)

          when(io.sgRead.rsp.fire && io.sgRead.rsp.last){
            for ((channel, e) <- (channels, oh).zipped) when(e) {
              channel.ll.readDone := True
            }
          }
        }
      }

      val sgRsp = p.withSgBus generate new Area{
        def rsp = io.sg.read.rsp
        val oh = UIntToOh(rsp.channelId)

        when(rsp.fire) {
          for ((channel, e) <- (channels, oh.asBools).zipped; if channel.cp.linkedListFromSg) when(e) {
            channel.ll.readDone := True
            channel.ll.writeDone := True
            if (channel.cp.canRead) channel.push.m2b.address := rsp.srcAddress
            if (channel.cp.canWrite) channel.pop.b2m.address := rsp.dstAddress
            channel.bytes := rsp.bytes
            channel.ll.controlNoCompletion := False
            if(channel.cp.canOutput) channel.pop.b2s.last := rsp.last
            channel.ll.gotDescriptorStall := rsp.stall
          }
        }
      }

      val writeRsp = new Area {
        val context = io.sgWrite.rsp.context.as(p.SgWriteContext())
        val oh = UIntToOh(context.channel, width = channels.size).asBools

        io.sgWrite.rsp.ready := True
        when(io.sgWrite.rsp.fire){
          for ((channel, e) <- (channels, oh).zipped) when(e) {
            channel.ll.writeDone := True
          }
        }
      }
    }

    io.interrupts := 0
    val mapping = new Area{
      for(channel <- channels){
        val a = 0x000+channel.id*0x80


        if (channel.cp.canRead && channel.cp.directCtrlCapable) ctrl.writeMultiWord(channel.push.m2b.address, a + 0x00)
        if (channel.cp.canInput) ctrl.write(channel.push.s2b.portId, a + 0x08, 0)
        //        if(channel.cp.canInput) ctrl.write(channel.push.s2b.sourceId,       a+0x08, 16)
//        if (channel.cp.canInput) ctrl.write(channel.push.s2b.sinkId, a + 0x08, 16)
        if (channel.cp.canRead && channel.cp.bytePerBurst.isEmpty) ctrl.write(channel.push.m2b.bytePerBurst, a + 0x0C, 0)
        ctrl.write(channel.push.memory, a + 0x0C, 12)
        if (channel.cp.canInput) ctrl.write(channel.push.s2b.completionOnLast, a + 0x0C, 13)
        if (channel.cp.canInput) ctrl.write(channel.push.s2b.waitFirst, a + 0x0C, 14)

        if (channel.cp.canWrite && channel.cp.directCtrlCapable) ctrl.writeMultiWord(channel.pop.b2m.address, a + 0x10)
        if (channel.cp.canOutput) ctrl.write(channel.pop.b2s.portId, a + 0x18, 0)
        if (channel.cp.canOutput) ctrl.write(channel.pop.b2s.sourceId, a + 0x18, 8)
        if (channel.cp.canOutput) ctrl.write(channel.pop.b2s.sinkId, a + 0x18, 16)
        if (channel.cp.canWrite && channel.cp.bytePerBurst.isEmpty) ctrl.write(channel.pop.b2m.bytePerBurst, a + 0x1C, 0)
        ctrl.write(channel.pop.memory, a + 0x1C, 12)
        if (channel.cp.canOutput) ctrl.write(channel.pop.b2s.last, a + 0x1C, 13)

        ctrl.read(channel.channelValid, a + 0x2C, 0)

        ctrl.write(channel.channelStop, a + 0x2C, 2)

        if(channel.cp.directCtrlCapable) {
          ctrl.setOnSet(channel.channelStart, a + 0x2C, 0)
          ctrl.setOnSet(channel.ctrl.kick, a + 0x2C, 0)
          ctrl.write(channel.bytes, a + 0x20, 0)
          if (channel.cp.selfRestartCapable) ctrl.write(channel.selfRestart, a + 0x2C, 1)
        }

        if(channel.cp.linkedListCapable){
          ctrl.setOnSet(channel.channelStart, a + 0x2C, 4)
          ctrl.setOnSet(channel.ll.sgStart, a + 0x2C, 4)

          ctrl.write(channel.ll.ptrNext, a + 0x70)
          ctrl.read(channel.ll.ptr, a + 0x70)
          ctrl.write(channel.ll.onSgStream, a + 0x78)
        }

        if(channel.cp.fifoMapping.isEmpty) {
          ctrl.write(channel.fifo.base, a + 0x40, log2Up(p.memory.bankWidth / 8))
          ctrl.write(channel.fifo.words, a + 0x40, 16 + log2Up(p.memory.bankWidth / 8))
        }
        ctrl.write(channel.priority, a+0x44, 0)
        ctrl.write(channel.weight, a+0x44, 8)

        def map(interrupt : Interrupt, id : Int): Unit ={
          ctrl.write(interrupt.enable, a+0x50, id)
          ctrl.clearOnSet(interrupt.valid, a+0x54, id)
          ctrl.read(interrupt.valid, a+0x54, id)
          io.interrupts(channel.id) setWhen(interrupt.valid)
        }

        map(channel.interrupts.completion, 0)
        if(channel.cp.halfCompletionInterrupt) map(channel.interrupts.halfCompletion.interrupt, 1)
        map(channel.interrupts.onChannelCompletion, 2)
        if(channel.cp.linkedListCapable) map(channel.interrupts.onLinkedListUpdate, 3)
        if(channel.cp.canInput) map(channel.interrupts.s2mPacket, 4)
        if(channel.cp.progressProbes)ctrl.read(channel.bytesProbe.value, a+0x60)
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
      val input = io.input.m2sPipe(flush = io.flush)

      val countOnesLogic = Vec(CountOneOnEach(input.mask))

      case class S0Output() extends Bundle{
        val cmd = AggregatorCmd(p)
        val countOnes = cloneOf(countOnesLogic)
      }

      val outputPayload = S0Output()
      outputPayload.cmd := input.payload
      outputPayload.countOnes := countOnesLogic
      val output = input.translateWith(outputPayload)
    }

    val s1 = new Area{
      val input = s0.output.m2sPipe(flush = io.flush)

      val offset = Reg(UInt(log2Up(p.byteCount) bits))
      val offsetNext = offset + input.countOnes.last
      when(input.fire){
        offset := offsetNext.resized
      }
      when(io.flush){
        offset := io.offset
      }

      val byteCounter = Reg(UInt(p.burstLength + 1 bits))
      when(input.fire){
        byteCounter := byteCounter + input.countOnes.last
      }
      when(io.flush){
        byteCounter := 0
      }

      val inputIndexes = Vec((U(0) +: input.countOnes.dropRight(1)).map(_ + offset))
      case class S1Output() extends Bundle{
        val cmd = AggregatorCmd(p)
        val index = cloneOf(inputIndexes)
        val last = Bool()
        val sel = Vec(UInt(log2Up(p.byteCount) bits), p.byteCount)
        val selValid = Bits(p.byteCount bits)
      }
      val outputPayload = S1Output()
      outputPayload.cmd := input.cmd
      outputPayload.index := inputIndexes
      outputPayload.last := offsetNext.msb
      for(i <- 0 until p.byteCount) {
        val selOh = (0 until p.byteCount).map(inputId => input.cmd.mask(inputId) && inputIndexes(inputId) === i)
        outputPayload.sel(i) := OHToUInt(selOh)
        outputPayload.selValid(i) := selOh.orR && outputPayload.cmd.mask(outputPayload.sel(i))
      }
      val output = input.translateWith(outputPayload)
    }

    val s2 = new Area{
      val input = s1.output.m2sPipe(flush = io.flush)
      input.ready := !io.output.enough || io.output.consume
      when(s1.byteCounter > io.burstLength){
        input.ready := False
      }

      io.output.consumed := input.fire
      io.output.context := input.cmd.context

      val inputDataBytes = input.cmd.data.subdivideIn(8 bits)
      val byteLogic = for(byteId <- 0 until p.byteCount) yield new Area{
        val buffer = new Area{
          val valid = Reg(Bool())
          val data = Reg(Bits(8 bits))
        }
        def sel = input.sel(byteId)
        val lastUsed = byteId === io.output.lastByteUsed
        val inputMask = input.selValid(byteId)
        val inputData = inputDataBytes.read(sel)
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

abstract class DmaSgTesterCtrl(clockDomain: ClockDomain){
  import spinal.core.sim._
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


  def channelToAddress(channel : Int) = 0x000 + channel*0x80
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
  def channelPushStream(channel : Int, portId : Int, sourceId : Int, sinkId : Int, completionOnPacket : Boolean = false): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(portId << 0 | sourceId << 8 | sinkId << 16, channelAddress + 0x08)
    ctrlWrite((if(completionOnPacket) 1 << 13 else 0) | (1 << 14), channelAddress + 0x0C)
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
  def channelStartSg(channel : Int, head : Long): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(head, channelAddress+0x70)
    ctrlWrite(0x10, channelAddress+0x2C)
  }
  def channelProgress(channel : Int): Int ={
    val channelAddress = channelToAddress(channel)
    ctrlRead(channelAddress + 0x60).toInt
  }
  def channelConfig(channel : Int,
                    fifoBase : Int,
                    fifoBytes : Int,
                    priority : Int,
                    weight : Int): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(fifoBase << 0 | fifoBytes-1 << 16,  channelAddress+0x40)
    ctrlWrite(priority | weight << 8,  channelAddress+0x44)
  }
  def channelInterruptConfigure(channel : Int, mask : Int): Unit = {
    val channelAddress = channelToAddress(channel)
    ctrlWrite(0xFFFFFFFFl, channelAddress+0x54)
    ctrlWrite(mask, channelAddress+0x50)
  }
  def channelStop(channel : Int): Unit ={
    val channelAddress = channelToAddress(channel)
    clockDomain.waitSampling(Random.nextInt(10))
    ctrlWrite(4, channelAddress + 0x2c)
  }

  def channelStartAndWait(channel : Int, bytes : BigInt): Unit = {
    val channelAddress = channelToAddress(channel)
    channelStart(channel, bytes, false)
    channelWaitCompletion(channel)

  }
  def channelBusy(channel : Int) ={
    val channelAddress = channelToAddress(channel)
    (ctrlRead(channelAddress + 0x2C) & 1) != 0
  }

  def channelSgBusy(channel : Int) ={
    val channelAddress = channelToAddress(channel)
    (ctrlRead(channelAddress + 0x2C) & 0x10) != 0
  }
  def channelWaitSgDone(channel : Int) ={
    val channelAddress = channelToAddress(channel)
    while (channelSgBusy(channel)) {
      clockDomain.waitSampling(Random.nextInt(50))
    }
  }
  def channelWaitCompletion(channel : Int) ={
    do{
      clockDomain.waitSampling(Random.nextInt(50))
    } while(channelBusy(channel))
  }

}

abstract class DmaSgTester(p : DmaSg.Parameter,
                           clockDomain : ClockDomain,
                           inputsIo : Seq[Bsb],
                           outputsIo : Seq[Bsb],
                           interruptsIo : Bits,
                           memory : SparseMemory,
                           dut : DmaSg.Core[_]) {

  import spinal.core.sim._

  val writesAllowed = mutable.HashMap[Long, (Byte, Int, Boolean)]()
  def allowWrite(address : Long, value : Byte) = writesAllowed(address) = (value, 0, true)
  def allowWrite(address : Long) = writesAllowed(address) = (0, 0, false)

  def writeNotification(address: Long, value: Byte): Unit = {
    val e = writesAllowed(address)
    assert(!e._3 || e._1 == value)
    writesAllowed(address) = ((e._1, e._2 + 1, e._3))
  }

  val memoryReserved = MemoryRegionAllocator(base = 0, size = 1l << p.writeAddressWidth)

  val outputs = for(outputId <- 0 until p.outputs.size) yield new {
    val readyDriver = StreamReadyRandomizer(outputsIo(outputId), clockDomain)
    val ref = Array.fill(1 << p.outputs(outputId).sinkWidth)(mutable.Queue[(Int, Int, Boolean)]())
    val monitor = new BsbMonitor(outputsIo(outputId), clockDomain) {
      override def onByte(value: Byte, source: Int, sink: Int): Unit = {
        assert(ref(sink).nonEmpty, s"Error output $outputId")
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
    var allowSplitLast = true
  }
  val inputs = for(inputId <- 0 until p.inputs.size) yield new {
    val ip = p.inputs(inputId)


    val sinkToPackets = ArrayBuffer.fill(1 << ip.sinkWidth)(mutable.Queue[Packet]())
    def enqueue(p : Packet) = sinkToPackets(p.sink).enqueue(p)

    val driver = StreamDriver(inputsIo(inputId), clockDomain) { p =>
      val nonEmptySinks = sinkToPackets.filter(_.nonEmpty)
      if (nonEmptySinks.isEmpty) {
        false
      } else {
        val packets = nonEmptySinks.randomPick()
        val packet = packets.head
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
        if(packet.data.isEmpty && (packet.allowSplitLast || Random.nextBoolean())) {
          p.last #= packet.last
          packet.done = true
          packets.dequeue()
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


  def channelToAddress(channel : Int) = 0x000 + channel*0x80
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
  def channelPushStream(channel : Int, portId : Int, sourceId : Int, sinkId : Int, completionOnPacket : Boolean = false): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(portId << 0 | sourceId << 8 | sinkId << 16, channelAddress + 0x08)
    ctrlWrite((if(completionOnPacket) 1 << 13 else 0) | (1 << 14), channelAddress + 0x0C)
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
  def channelStartSg(channel : Int, head : Long): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(head, channelAddress+0x70)
    ctrlWrite(0x10, channelAddress+0x2C)
  }
  def channelProgress(channel : Int): Int ={
    val channelAddress = channelToAddress(channel)
    ctrlRead(channelAddress + 0x60).toInt
  }
  def channelConfig(channel : Int,
                    fifoBase : Int,
                    fifoBytes : Int,
                    priority : Int,
                    weight : Int): Unit ={
    val channelAddress = channelToAddress(channel)
    ctrlWrite(fifoBase << 0 | fifoBytes-1 << 16,  channelAddress+0x40)
    ctrlWrite(priority | weight << 8,  channelAddress+0x44)
  }
  def channelInterruptConfigure(channel : Int, mask : Int): Unit = {
    val channelAddress = channelToAddress(channel)
    ctrlWrite(0xFFFFFFFFl, channelAddress+0x54)
    ctrlWrite(mask, channelAddress+0x50)
  }
  def channelStop(channel : Int): Unit ={
    val channelAddress = channelToAddress(channel)
    clockDomain.waitSampling(Random.nextInt(10))
    ctrlWrite(4, channelAddress + 0x2c)
  }

  def channelStartAndWait(channel : Int, bytes : BigInt, doCount : Int): Unit = {
    val channelAddress = channelToAddress(channel)
    if(Random.nextBoolean() && doCount == 1){
      //By pulling
      channelStart(channel, bytes, doCount != 1)
      channelWaitCompletion(channel)
    } else {
      //By interrupt
      fork{
        clockDomain.waitSampling(Random.nextInt(10))
        channelStart(channel, bytes, doCount != 1)
      }
      doCount match {
        case 1 => {
          channelInterruptConfigure(channel, 0x4)
          waitUntil((interruptsIo.toInt & 1 << channel) != 0)
          assert((ctrlRead(channelAddress + 0x2C) & 1) == 0)
        }
        case _ => {
          for(i <- 0 until doCount) {
            channelInterruptConfigure(channel, 0x1)
            waitUntil((interruptsIo.toInt & 1 << channel) != 0)
          }

          ctrlWrite(4, channelAddress + 0x2c)
          channelInterruptConfigure(channel, 0x4)
          channelWaitCompletion(channel)
        }
      }
    }
//    if(p.channels(channel).withProgressCounter) p.channels(channel).progressProbes match {
//      case true =>   assert(channelProgress(channel) == bytes)
//      case false =>  assert(dut.channels(channel).bytesProbe.value.toLong == bytes)
//    }

  }
  def channelBusy(channel : Int) ={
    val channelAddress = channelToAddress(channel)
    (ctrlRead(channelAddress + 0x2C) & 1) != 0
  }

  def channelSgBusy(channel : Int) ={
    val channelAddress = channelToAddress(channel)
    (ctrlRead(channelAddress + 0x2C) & 0x10) != 0
  }
  def channelWaitSgDone(channel : Int) ={
    val channelAddress = channelToAddress(channel)
    while (channelSgBusy(channel)) {
      clockDomain.waitSampling(Random.nextInt(50))
    }
  }
  def channelWaitCompletion(channel : Int) ={
    do{
      clockDomain.waitSampling(Random.nextInt(50))
    } while(channelBusy(channel))
  }

  def writeDescriptor(address : Long, push : Long, pop : Long, size : Long, next : Long, completed : Boolean, m2sLast : Boolean = false) = {
    memory.write(address+0, 0)
    memory.write(address+4, size-1 | (if(m2sLast) 1 << 30 else 0))
    memory.write(address+8, push)
    memory.write(address+16, pop)
    memory.write(address+24, next)
  }
  def writeTail(address : Long) = {
    memory.write(address+0, 0x80000000)
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
    inputs(inputId).enqueue(packet)

    waitUntil(packet.done)
    inputs(inputId).reservedSink.remove(sink)
  }

  def log(that : String) = { }
  val channelAgent = for((channel, channelId) <- p.channels.zipWithIndex) yield fork {
    Thread.currentThread().setName(s"CH $channelId")
    val cp = p.channels(channelId)
    val M2M, M2S, S2M = new Object
    var tests = ArrayBuffer[Object]()
    if(cp.memoryToMemory)        tests += M2M
    if(cp.outputsPorts.nonEmpty) tests += M2S
    if(cp.inputsPorts.nonEmpty)  tests += S2M
    for (r <- 0 until 100) {
//      println(f"Channel $channelId")
      clockDomain.waitSampling(Random.nextInt(100))
      tests.randomPick() match {
        case S2M => if (p.inputs.nonEmpty) {
          val TRANSFER, PACKETS_SELF, PACKETS_STOP, LINKED_LIST = new Object
          val test = ArrayBuffer[Object]()
          if(cp.directCtrlCapable) {
            test += TRANSFER
            test += PACKETS_SELF
            test += PACKETS_STOP
          }
          if(cp.linkedListCapable) test += LINKED_LIST
          test.randomPick() match {
            case LINKED_LIST => {
              val descriptorCount = Random.nextInt(5) + 1
              val inputId = cp.inputsPorts.randomPick()
              val packetBased = Random.nextBoolean()
              val innerStop = Random.nextFloat() < 0.3

//              val inputId = 0
//              val packetBased = false

              val ip = p.inputs(inputId)
              val connectedChannels = p.channels.filter(_.inputsPorts.contains(inputId))
              val sink = connectedChannels.indexOf(cp)
              val source = Random.nextInt(1 << ip.sourceWidth)
              while(inputs(inputId).reservedSink.contains(sink)) clockDomain.waitSampling(Random.nextInt(100))
              inputs(inputId).reservedSink.add(sink)

//              println("MIAOU")
              val tail = memoryReserved.allocateAligned(size = DmaSg.descriptorSize)
              val descriptors = List.fill(descriptorCount)( new {
                val address = memoryReserved.allocateAligned(size = DmaSg.descriptorSize)
                val bytes = (Random.nextInt(0x100) + 1)
                val to = memoryReserved.allocate(size = bytes)

                memory.writeInt(address.base.toLong, Random.nextInt() | (1 << 31))
                for(i <- 0 until bytes){
                  allowWrite(to.base.toInt + i)
                }
                for(i <- 0 until 4){
                  allowWrite(address.base.toInt + 0 + i)
                }

                def free(): Unit ={
                  def assertMasked(that : Boolean) = if(!innerStop) assert(that)
                  memoryReserved.free(address)
                  memoryReserved.free(to)
                  for(i <- 0 until bytes){
                    writesAllowed.remove(to.base.toInt + i)
                  }
                  for(i <- 0 until 4){
                    val count = writesAllowed.remove(address.base.toInt + 0 + i).get._2
                    assertMasked(if(packetBased)count == 1 else count >= 1)
                  }
                }

                log(f"Desc : ${address.base}%08x ${to.base}%08x ${to.base + bytes-1}%08x $bytes")
              })


              for(i <- 0 until descriptorCount) {
                val d = descriptors(i)
                writeDescriptor(
                  address = d.address.base.toLong,
                  push = 0,
                  pop = d.to.base.toLong,
                  size = d.bytes,
                  next = if(i == descriptorCount-1) tail.base.toLong else descriptors(i+1).address.base.toLong,
                  completed = false
                )
              }
              writeTail(tail.base.toInt)

              channelPushStream(channelId, cp.inputsPorts.indexOf(inputId), source, sink, completionOnPacket = packetBased)
              channelPopMemory (channelId, 0, 16)
              channelConfig (channelId, 0x40 * channelId, 0x40, Random.nextInt(2), Random.nextInt(4))
              channelStartSg(channelId, descriptors.head.address.base.toLong)
              dut.clockDomain.waitSampling(2)

              val checks = ArrayBuffer[() => Unit]()
              val packets = mutable.Queue[mutable.Queue[Byte]]()
              packetBased match {
                case false => {
                  var descriptorPtr = descriptors.head
                  var descriptorByteId = 0
                  while(descriptorPtr != null){
                    val packet = Packet(source = source, sink = sink, last = true)
                    val bytes = Random.nextInt(0x100)+1
                    for (byteId <- 0 until bytes) {
                      val value = Random.nextInt & 0xFF
                      packet.data += value

                      if(descriptorPtr != null){
                        val to = descriptorPtr.to.base.toInt + descriptorByteId
                        allowWrite(to, value.toByte)
                        checks += (() => assert (writesAllowed.remove (to).get._2 == 1))
                        descriptorByteId += 1
                        if(descriptorByteId == descriptorPtr.bytes) {
                          descriptorByteId = 0
                          val descriptorId = descriptors.indexOf(descriptorPtr)
                          descriptorPtr = if (descriptorId == descriptorCount - 1) null else descriptors(descriptorId + 1)
                        }
                      }
                    }
                    inputs(inputId).enqueue(packet)
                    packets.enqueue(mutable.Queue.empty ++= (packet.data.map(_.toByte)))
//                    println(f"Packet : ${packet.data.size}")
                  }
                }
                case true => {
                  val descriptorsBytes = descriptors.map(_.bytes).sum
                  var packetsBytes = 0
                  while(packetsBytes < descriptorsBytes){
                    val packet = Packet(source = source, sink = sink, last = true)
                    val bytes = Random.nextInt(0x100)+1
                    packetsBytes += bytes
                    for (byteId <- 0 until bytes) {
                      val value = Random.nextInt & 0xFF
                      packet.data += value
                    }
                    inputs(inputId).enqueue(packet)
                    packets.enqueue(mutable.Queue.empty ++= (packet.data.map(_.toByte)))
                    log(f"Packet : ${packet.data.size}")
                  }
                }
              }

              def decodeDescriptor(address : Long) = new {
                val status = memory.readInt(address + 0)
                log(f"Status : $status%08x")
                val bytes = (status & 0x7FFFFFF)
                val isLast = (status & 0x40000000) != 0
                val completed = (status & 0x80000000) != 0
//                println(f"$bytes")
              }
              var stopVerification = false
              val verificationThread = fork{
                packetBased match {
                  case true => {
                    for(descriptor <- descriptors){
//                      println(s"wait ${descriptor.address.base.toLong + 31}")
                      waitUntil( memory.read(descriptor.address.base.toLong + 3) < 0 || stopVerification)
                      if(stopVerification) simThread.terminate()
                      val decoded = decodeDescriptor(descriptor.address.base.toLong)
                      val packet = packets.head
                      decoded.isLast match {
                        case true => assert(decoded.bytes == packet.size)
                        case false => assert(decoded.bytes == descriptor.bytes)
                      }
                      assert(packet.size >= decoded.bytes)
                      for(byteId <- 0 until decoded.bytes){
                        assert(packet.dequeue() == memory.read(descriptor.to.base.toLong + byteId))
                      }
                      if(decoded.isLast){
                        assert(packet.isEmpty)
                        packets.dequeue()
                      }
                    }
                  }
                  case false => {
                    var descriptorId = 0
                    var descriptorByteId = 0
                    channelInterruptConfigure(channelId, 0x8)
                    while(descriptorId != descriptors.size){
                      waitUntil((dut.io.interrupts.toInt & (1 << channelId)) != 0 || stopVerification)
                      if(stopVerification) simThread.terminate()
                      channelInterruptConfigure(channelId, 0x8)
                      var continue = true
                      while(descriptorId != descriptors.size && continue){
                        continue = false
                        val descriptor = descriptors(descriptorId)
                        val decoded = decodeDescriptor(descriptor.address.base.toLong)
                        while(descriptorByteId < decoded.bytes){
                          val packet = packets.head
                          assert(memory.read(descriptor.to.base.toLong + descriptorByteId) == packet.dequeue(), f"Error at ${descriptor.to.base.toLong + descriptorByteId}%08x")
                          if(packet.isEmpty) packets.dequeue()
                          descriptorByteId += 1
                        }
                        decoded.completed match {
                          case true => {
                            assert(descriptorByteId == descriptor.bytes)
                            descriptorId += 1
                            descriptorByteId = 0
                            continue = true
                          }
                          case false => {
//                            assert(descriptorByteId != descriptor.bytes)
                          }
                        }
                      }
                    }
                  }
                }
              }

              val stopThread = fork{if(innerStop) {
                dut.clockDomain.waitSampling(Random.nextInt(600))
                channelStop(channelId)
                channelInterruptConfigure(channelId, 0)
                stopVerification = true
              }}
              channelWaitCompletion(channelId)
              stopThread.join()
              verificationThread.join()
              if(!innerStop) checks.foreach(_())
              descriptors.foreach(_.free())
              memoryReserved.free(tail)
              waitUntil(inputs(inputId).sinkToPackets(sink).isEmpty)
              inputs(inputId).reservedSink.remove(sink)
            }
            case PACKETS_STOP => {
              val bufferSize = 0x100

              val to = memoryReserved.allocate(size = bufferSize)
              def rBytes = Random.nextInt(bufferSize) + 1
              val inputId = cp.inputsPorts.randomPick()
              val packetCount = Random.nextInt(5) + 1

//              val to = SizeMapping(0x1000, bufferSize)
//              def rBytes = 0x20
//              val inputId = 0
//              val packetCount = 1

              val ip = p.inputs(inputId)
              val connectedChannels = p.channels.filter(_.inputsPorts.contains(inputId))
              val sink = connectedChannels.indexOf(cp)
              val source = Random.nextInt(1 << ip.sourceWidth)
              while(inputs(inputId).reservedSink.contains(sink)) clockDomain.waitSampling(Random.nextInt(100))
              inputs(inputId).reservedSink.add(sink)


              for(_ <- 0 until packetCount){
                val bytes = rBytes
                val packet = Packet(source = source, sink = sink, last = true)
                for (byteId <- 0 until bytes) {
                  val value = Random.nextInt & 0xFF
                  allowWrite(to.base.toInt + byteId, value.toByte)
                  packet.data += value
                }

                channelPushStream(channelId, cp.inputsPorts.indexOf(inputId), source, sink, completionOnPacket = true)
                channelPopMemory(channelId, to.base.toInt, 16)
                channelConfig (channelId, 0x40 * channelId, 0x40, Random.nextInt(2), Random.nextInt(4))
                channelInterruptConfigure(channelId, 0x4)
                channelStart(channelId, bufferSize, selfRestart = false)
                dut.clockDomain.waitSampling(2)

                inputs(inputId).enqueue(packet)
                if(Random.nextBoolean()){ //Send trash afterward
                  val trash = Packet(source = source, sink = sink, last = true)
                  for(i <- 0 until Random.nextInt(20)){
                    trash.data += Random.nextInt(256)
                  }
                  inputs(inputId).enqueue(trash)
                }
                waitUntil((interruptsIo.toInt & 0x1 << channelId) != 0)

                for (byteId <- 0 until bytes) {
                  assert(writesAllowed.remove(to.base.toInt + byteId).get._2 == 1)
                }
                assert(!channelBusy(channelId))
                waitUntil(inputs(inputId).sinkToPackets(sink).isEmpty)
              }
              inputs(inputId).reservedSink.remove(sink)
            }
            case TRANSFER => {
              val doCount = if(cp.selfRestartCapable) Random.nextInt(3) + 1 else 1
              val bytes = (Random.nextInt(0x100) + 1)
              val to = memoryReserved.allocate(size = bytes)
              val inputId = cp.inputsPorts.randomPick()
//              val doCount = 3
//              val bytes = 0x40+2
//              val to = SizeMapping(0x1000, bytes)
//              val inputId = 0

              val ip = p.inputs(inputId)
              val source = Random.nextInt(1 << ip.sourceWidth)
              val connectedChannels = p.channels.filter(_.inputsPorts.contains(inputId))
              val sink = connectedChannels.indexOf(cp)

              while(inputs(inputId).reservedSink.contains(sink)) clockDomain.waitSampling(Random.nextInt(100))
              inputs(inputId).reservedSink.add(sink)

              channelPushStream(channelId, cp.inputsPorts.indexOf(inputId), source, sink)
              channelPopMemory(channelId, to.base.toInt, 16)
              channelConfig (channelId, 0x40 * channelId, 0x40, Random.nextInt(2), Random.nextInt(4))
              val packet = Packet(source = source, sink = sink, last = false)
              fork{
                while(!channelBusy(channelId)){
                  clockDomain.waitSampling(Random.nextInt(5))
                }
                val datas = ArrayBuffer[Int]()
                for (byteId <- 0 until bytes) {
                  val value = Random.nextInt & 0xFF
                  allowWrite(to.base.toInt + byteId, value.toByte)
                  datas += value
                }
                for(_ <- 0 until doCount + 20) {
                  packet.data ++= datas
                }
                inputs(inputId).enqueue(packet)
              }
              channelStartAndWait(channelId, bytes, doCount)
              for (byteId <- 0 until bytes) {
                assert(writesAllowed.remove(to.base.toInt + byteId).get._2 >= doCount)
              }
              if(!packet.done) inputs(inputId).sinkToPackets(sink).clear()

              val packetFlush = Packet(source = source, sink = sink, last = true)
              for (byteId <- 0 until Random.nextInt(10)) {
                packetFlush.data += Random.nextInt & 0xFF
              }
              inputs(inputId).enqueue(packetFlush)

              waitUntil(packetFlush.done)
              inputs(inputId).reservedSink.remove(sink)
            }
            case PACKETS_SELF => {
              val bufferSize = 0x80
              val packetCount = if(cp.selfRestartCapable) Random.nextInt(5) + 1 else 1
              val to = memoryReserved.allocate(size = bufferSize)
              def rBytes = Random.nextInt(bufferSize) + 1
              val inputId = cp.inputsPorts.randomPick()

//              val packetCount = 10
//              val to = SizeMapping(0x1000, bufferSize)
//              def rBytes = 0x80
//              val inputId = 0

              val ip = p.inputs(inputId)
              val connectedChannels = p.channels.filter(_.inputsPorts.contains(inputId))
              val sink = connectedChannels.indexOf(cp)
              val source = Random.nextInt(1 << ip.sourceWidth)
              while(inputs(inputId).reservedSink.contains(sink)) clockDomain.waitSampling(Random.nextInt(100))
              inputs(inputId).reservedSink.add(sink)

              channelPushStream(channelId, cp.inputsPorts.indexOf(inputId), source, sink)
              channelPopMemory(channelId, to.base.toInt, 16)
              channelConfig (channelId, 0x40 * channelId, 0x40, Random.nextInt(2), Random.nextInt(4))
              channelInterruptConfigure(channelId, 0x10)
              channelStart(channelId, bufferSize, selfRestart = true)
              var cpuIdx, refIdx = 0

              for(_ <- 0 until packetCount){
                val bytes = rBytes
                val packet = Packet(source = source, sink = sink, last = true)
                val datas = ArrayBuffer[Byte]()
                for (byteId <- 0 until bytes) {
                  val value = Random.nextInt & 0xFF
                  allowWrite(to.base.toInt + refIdx, value.toByte)
                  packet.data += value
                  datas += value.toByte
                  refIdx += 1
                  refIdx &= bufferSize-1
                }

                inputs(inputId).enqueue(packet)
                waitUntil((interruptsIo.toInt & 0x1 << channelId) != 0)

                for (byteId <- 0 until bytes) {
                  assert(writesAllowed.remove(to.base.toInt + cpuIdx).get._2 == 1)
                  assert(memory.read(to.base.toInt + cpuIdx) == datas(byteId))
                  cpuIdx += 1
                  cpuIdx &= bufferSize-1
                }
                channelInterruptConfigure(channelId, 0x10)
                dut.clockDomain.waitSampling(3)

              }

              channelStop(channelId)
              while(channelBusy(channelId)){}
              inputs(inputId).reservedSink.remove(sink)
            }
          }

        }

        case M2M => {
          val TRANSFER, LINKED_LIST = new Object
          val test = ArrayBuffer[Object]()
          if(cp.directCtrlCapable) test += TRANSFER
          if(cp.linkedListCapable) test += LINKED_LIST
          test.randomPick() match {
            case TRANSFER => {
              val bytes = (Random.nextInt (0x100) + 1)
              val from = memoryReserved.allocate (size = bytes)
              val to = memoryReserved.allocate (size = bytes)
              val doCount = if (cp.selfRestartCapable) Random.nextInt (3) + 1 else 1
                //          val from = 0x100 + Random.nextInt(4)
                //          val to = 0x400 + Random.nextInt(4)
                //          val bytes = 0x40 + Random.nextInt(4)
//              val bytes = 0x40
//              val from = SizeMapping(0x1000 + 0x100*channelId, bytes)
//              val to = SizeMapping(0x2000 + 0x100*channelId, bytes)
//              val doCount = 3
              for (byteId <- 0 until bytes) {
                allowWrite (to.base.toInt + byteId, memory.read (from.base.toInt + byteId))
              }

              channelPushMemory (channelId, from.base.toInt, 16)
              channelPopMemory (channelId, to.base.toInt, 16)
              channelConfig (channelId, 0x40 * channelId, 0x40, Random.nextInt(2), Random.nextInt(4))
              channelStartAndWait (channelId, bytes, doCount)

              for (byteId <- 0 until bytes) {
                assert (writesAllowed.remove (to.base.toInt + byteId).get._2 >= doCount)
              }
            }
            case LINKED_LIST => {
              val descriptorCount = Random.nextInt(5) + 1
              val innerStop = Random.nextFloat() < 0.3

              val tail = memoryReserved.allocateAligned(size = DmaSg.descriptorSize)
              val descriptors = List.fill(descriptorCount)( new {
                val address = memoryReserved.allocateAligned(size = DmaSg.descriptorSize)
                val bytes = (Random.nextInt(0x100) + 1)
                val from = memoryReserved.allocate(size = bytes)
                val to = memoryReserved.allocate(size = bytes)
                for (byteId <- 0 until bytes) {
                  allowWrite (to.base.toInt + byteId, memory.read (from.base.toInt + byteId))
                }
                for(i <- 0 until 4){
                  allowWrite(address.base.toInt + 0 + i)
                }

                def free(): Unit ={
                  def assertMasked(that : Boolean) = if(!innerStop) assert(that)

                  for (byteId <- 0 until bytes) {
                    assertMasked(writesAllowed.remove(to.base.toInt + byteId).get._2 == 1)
                  }
                  for (i <- 0 until 4) {
                    assertMasked(writesAllowed.remove(address.base.toInt + 0 + i).get._2 == 1)
                  }
                  assertMasked((memory.readInt(address.base.toLong + 0) & 0x80000000) != 0)

                  memoryReserved.free(address)
                  memoryReserved.free(from)
                  memoryReserved.free(to)
                }

//                println(f"${address.base}%x ${from.base}%x ${to.base}%x")
              })

              for(i <- 0 until descriptorCount) {
                val d = descriptors(i)
                writeDescriptor(
                  address = d.address.base.toLong,
                  push = d.from.base.toLong,
                  pop = d.to.base.toLong,
                  size = d.bytes,
                  next = if(i == descriptorCount-1) tail.base.toLong else descriptors(i+1).address.base.toLong,
                  completed = false
                )
              }
              writeTail(tail.base.toInt)


              channelPushMemory (channelId, 0, 16)
              channelPopMemory (channelId, 0, 16)
              channelConfig (channelId, 0x40 * channelId, 0x40, Random.nextInt(2), Random.nextInt(4))
              channelStartSg(channelId, descriptors.head.address.base.toLong)
              val stopThread = fork{if(innerStop) {
                dut.clockDomain.waitSampling(Random.nextInt(600))
                channelStop(channelId)
                channelInterruptConfigure(channelId, 0)
              }}
              channelWaitCompletion(channelId)
              stopThread.join()
              memoryReserved.free(tail)
              descriptors.foreach(_.free())
            }
          }
        }

        case M2S => if (p.outputs.nonEmpty) {
          val TRANSFER, LINKED_LIST = new Object
          val test = ArrayBuffer[Object]()
          if(cp.directCtrlCapable) test += TRANSFER
          if(cp.linkedListCapable) test += LINKED_LIST
          test.randomPick() match {
            case LINKED_LIST => {
              val descriptorCount = Random.nextInt(5) + 1
              val outputId = cp.outputsPorts.randomPick()
              val innerStop = Random.nextFloat() < 0.3

//              val outputId = 0

              val op = p.outputs(outputId)
              val source = Random.nextInt(1 << op.sourceWidth)
              val sink = Random.nextInt(1 << op.sinkWidth)

              while(outputs(outputId).reservedSink.contains(sink)) clockDomain.waitSampling(Random.nextInt(100))
              outputs(outputId).reservedSink.add(sink)

              val tail = memoryReserved.allocateAligned(size = DmaSg.descriptorSize)
              val descriptors = List.fill(descriptorCount)( new {
                val address = memoryReserved.allocateAligned(size = DmaSg.descriptorSize)
                val bytes = (Random.nextInt(0x100) + 1)
                val from = memoryReserved.allocate(size = bytes)
                val withLast = Random.nextBoolean()

//                val bytes = 0x40
//                val from =  SizeMapping(0x1000, bytes)

                for(i <- 0 until 4){
                  allowWrite(address.base.toInt + 0 + i)
                }
                for (byteId <- 0 until bytes) {
                  outputs(outputId).ref(sink).enqueue((memory.read(from.base.toInt + byteId), source, false))
                }
                if (withLast) outputs(outputId).ref(sink).enqueue((0, source, true))

                def free(): Unit ={
                  def assertMasked(that : Boolean) = if(!innerStop) assert(that)
                  memoryReserved.free(address)
                  memoryReserved.free(from)
                  assertMasked((memory.readInt(address.base.toLong + 0) & 0x80000000) != 0)
                  for (i <- 0 until 4) {
                    assertMasked(writesAllowed.remove(address.base.toInt + 0 + i).get._2 == 1)
                  }
                }
              })

              for(i <- 0 until descriptorCount) {
                val d = descriptors(i)
                writeDescriptor(
                  address = d.address.base.toLong,
                  push = d.from.base.toLong,
                  pop = 0,
                  size = d.bytes,
                  next = if(i == descriptorCount-1) tail.base.toLong else descriptors(i+1).address.base.toLong,
                  completed = false,
                  m2sLast = d.withLast
                )
              }
              writeTail(tail.base.toInt)


              channelPushMemory (channelId, 0, 16)
              channelPopStream(channelId, cp.outputsPorts.indexOf(outputId), source, sink, false)
              channelConfig (channelId, 0x40 * channelId, 0x40, Random.nextInt(2), Random.nextInt(4))
              channelStartSg(channelId, descriptors.head.address.base.toLong)
              val stopThread = fork{if(innerStop) {
                dut.clockDomain.waitSampling(Random.nextInt(600))
                channelStop(channelId)
                channelInterruptConfigure(channelId, 0)
              }}
              channelWaitCompletion(channelId)
              stopThread.join()
              dut.clockDomain.waitSampling(20)
              waitUntil(!outputsIo(outputId).valid.toBoolean)
              dut.clockDomain.waitSampling(20)
              outputs(outputId).ref(sink).clear()
              descriptors.foreach(_.free())
              memoryReserved.free(tail)
              outputs(outputId).reservedSink.remove(sink)
            }
            case TRANSFER => {
              val doCount = if(cp.selfRestartCapable) Random.nextInt(3) + 1 else 1
              val bytes = (Random.nextInt(0x100) + 1)
              val from = memoryReserved.allocate(size = bytes)
              val outputId = cp.outputsPorts.randomPick()

              //          val doCount = 3
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
              channelPopStream(channelId, cp.outputsPorts.indexOf(outputId), source, sink, withLast)
              channelConfig (channelId, 0x40 * channelId, 0x40, Random.nextInt(2), Random.nextInt(4))
              channelStart(channelId, bytes = bytes, doCount != 1)
              waitUntil(outputs(outputId).ref(sink).size <= 4*(bytes + (if(withLast) 1 else 0)))
              channelStop(channelId)
              channelWaitCompletion(channelId)
              dut.clockDomain.waitSampling(20)
              waitUntil(!outputsIo(outputId).valid.toBoolean)
              dut.clockDomain.waitSampling(20)
              outputs(outputId).ref(sink).clear()
              outputs(outputId).reservedSink.remove(sink)
            }
          }
        }
      }
    }
  }

  def waitCompletion() {
    channelAgent.foreach(e => e.join())
    clockDomain.waitSampling(1000)
  }
}


object SgDmaTestsParameter{

  import spinal.core.sim._

  def test(p: Parameter) = {
    val pCtrl = BmbParameter(
      addressWidth = DmaSg.ctrlAddressWidth,
      dataWidth    = 32,
      sourceWidth  = 0,
      contextWidth = 4,
      lengthWidth  = 2
    )

    SimConfig.allOptimisation.compile(new DmaSg.Core[Bmb](p, ctrlType = HardType(Bmb(pCtrl)), BmbSlaveFactory(_))).doSim(seed=42){ dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.forkSimSpeedPrinter(1.0)

      var writeNotificationHandle : (Long, Byte) => Unit = null

      val memory = new BmbMemoryAgent{
        override def writeNotification(address: Long, value: Byte): Unit = {
          writeNotificationHandle(address,value)
          super.setByte(address, value)
        }
      }
      if(p.canSgRead) memory.addPort(dut.io.sgRead, 0, dut.clockDomain, true)
      if(p.canSgWrite) memory.addPort(dut.io.sgWrite, 0, dut.clockDomain, true)
      if(p.canRead) memory.addPort(dut.io.read, 0, dut.clockDomain, true)
      if(p.canWrite) memory.addPort(dut.io.write, 0, dut.clockDomain, true)

      val ctrl = BmbDriver(dut.io.ctrl, dut.clockDomain)

      val tester = new DmaSgTester(
        p            = p,
        clockDomain  = dut.clockDomain,
        inputsIo     = dut.io.inputs,
        outputsIo    = dut.io.outputs,
        interruptsIo = dut.io.interrupts,
        memory       = memory.memory,
        dut
      ) {
        override def ctrlWriteHal(data: BigInt, address: BigInt): Unit = ctrl.write(data, address)
        override def ctrlReadHal(address: BigInt): BigInt = ctrl.read(address)
        writeNotificationHandle = writeNotification
      }

      tester.waitCompletion()
    }
  }

  def random() : DmaSg.Parameter = {
    var layout : DmaMemoryLayout = null

    var layoutWidth = 0
    var layoutWidthByte = 0

    do{
      layout = DmaMemoryLayout(
        bankCount            = List(1,2,4)(Random.nextInt(3)),
        bankWidth            = List(8,16,32)(Random.nextInt(3)),
        bankWords            = List(512, 1024, 2048)(Random.nextInt(3)),
        priorityWidth        = Random.nextInt(3)
      )

//      layout = DmaMemoryLayout(
//        bankCount            = 1,
//        bankWidth            = 64,
//        bankWords            = 1024,
//        priorityWidth        = 2
//      )
      layoutWidth = layout.bankCount* layout.bankWidth
      layoutWidthByte = layoutWidth/8
    } while(layoutWidthByte < 4)

    val outputs = ArrayBuffer[BsbParameter]()
    for(i <- 0 until Random.nextInt(4)) outputs ++= Seq(
      BsbParameter(
        byteCount   = Math.max(layout.bankWidth/8, layoutWidthByte >> Random.nextInt(log2Up(layoutWidthByte) + 1)),
        sourceWidth = Random.nextInt(4) + 1,
        sinkWidth   = 4
      )
    )



    val inputs = ArrayBuffer[BsbParameter]()
    for(i <- 0 until Random.nextInt(4)) inputs ++= Seq(
      BsbParameter(
        Math.max(layout.bankWidth/8, layoutWidthByte >> Random.nextInt(log2Up(layoutWidthByte) + 1)),
        sourceWidth = Random.nextInt(4) + 1,
        sinkWidth   = 4
      )
    )

    val channels = ArrayBuffer[Channel]()
    for(i <- 0 to Random.nextInt(4)) {
      val direct = Random.nextBoolean()
      val ll = !direct || Random.nextBoolean()
      val i = inputs.zipWithIndex.map(_._2).filter(_ => Random.nextBoolean())
      val o = outputs.zipWithIndex.map(_._2).filter(_ => Random.nextBoolean())
      channels += DmaSg.Channel(
        memoryToMemory = Random.nextBoolean() || (i.isEmpty && o.isEmpty),
        inputsPorts    = i,
        outputsPorts   = o,
        selfRestartCapable = direct && Random.nextBoolean(),
        progressProbes = Random.nextBoolean(),
        halfCompletionInterrupt = true,
        linkedListCapable = ll,
        directCtrlCapable = direct
      )
    }

    //Ensure all I O have at least one channel
    {
      val direct = Random.nextBoolean()
      val ll = !direct || Random.nextBoolean()
      val i = inputs.zipWithIndex.map(_._2).filter(id => !channels.flatMap(_.inputsPorts).contains(id))
      val o = outputs.zipWithIndex.map(_._2).filter(id => !channels.flatMap(_.outputsPorts).contains(id))
      if(i.nonEmpty || o.nonEmpty)
      channels += DmaSg.Channel(
        memoryToMemory = Random.nextBoolean(),
        inputsPorts =  i,
        outputsPorts = o,
        selfRestartCapable = direct && Random.nextBoolean(),
        progressProbes = Random.nextBoolean(),
        halfCompletionInterrupt = true,
        linkedListCapable = ll,
        directCtrlCapable = direct
      )
    }

    DmaSg.Parameter(
      readAddressWidth  = 24,
      readDataWidth     = layoutWidth,
      readLengthWidth   = 6,
      writeAddressWidth = 24,
      writeDataWidth    = layoutWidth,
      writeLengthWidth  = 6,
      sgAddressWidth = 24,
      sgReadDataWidth = layoutWidth,
      sgWriteDataWidth = layoutWidth,
      memory = layout,
      outputs = outputs,
      inputs = inputs,
      channels = channels,
      bytePerTransferWidth = 16,
      weightWidth = Random.nextInt(3)
    )
  }

  def apply(allowSmallerStreams : Boolean) : Seq[(String, DmaSg.Parameter)] = {
    val parameters = ArrayBuffer[(String, DmaSg.Parameter)]()

    for(withMemoryToMemory <- List(true, false);
        withOutputs <- List(true, false);
        withInputs <- List(true, false);
        if withMemoryToMemory || withOutputs || withInputs){

      var name = ""
      if(withMemoryToMemory) name = name + "M2m"
      if(withOutputs) name = name + "M2s"
      if(withInputs) name = name + "S2m"

      val outputs = ArrayBuffer[BsbParameter]()
      if(withOutputs) {
        outputs ++= Seq(
          BsbParameter(
            byteCount   = 4,
            sourceWidth = 3,
            sinkWidth   = 4
          ),
          BsbParameter(
            byteCount   = 4,
            sourceWidth = 0,
            sinkWidth   = 4
          ),
          BsbParameter(
            byteCount   = 2,
            sourceWidth = 2,
            sinkWidth   = 4
          ),
          BsbParameter(
            byteCount   = 2,
            sourceWidth = 5,
            sinkWidth   = 4
          )
        )
        if(allowSmallerStreams) outputs += BsbParameter(
          byteCount   = 1,
          sourceWidth = 0,
          sinkWidth   = 4
        )
        if(allowSmallerStreams) outputs += BsbParameter(
          byteCount   = 1,
          sourceWidth = 0,
          sinkWidth   = 4
        )
      }

      val inputs = ArrayBuffer[BsbParameter]()
      if(withInputs) {
        inputs ++= Seq(
          BsbParameter(
            byteCount   = 4,
            sourceWidth = 1,
            sinkWidth   = 4
          ),
          BsbParameter(
            byteCount   = 4,
            sourceWidth = 0,
            sinkWidth   = 4
          ),
          BsbParameter(
            byteCount   = 2,
            sourceWidth = 4,
            sinkWidth   = 4
          ),
          BsbParameter(
            byteCount   = 2,
            sourceWidth = 2,
            sinkWidth   = 4
          )
        )
        if(allowSmallerStreams) inputs += BsbParameter(
          byteCount   = 1,
          sourceWidth = 0,
          sinkWidth   = 4
        )
        if(allowSmallerStreams) inputs += BsbParameter(
          byteCount   = 1,
          sourceWidth = 0,
          sinkWidth   = 4
        )
      }


      val channels = ArrayBuffer[Channel]()
      channels += DmaSg.Channel(
        memoryToMemory = withMemoryToMemory,
        inputsPorts    = inputs.zipWithIndex.map(_._2),
        outputsPorts   = outputs.zipWithIndex.map(_._2),
        selfRestartCapable = true,
        progressProbes = true,
        halfCompletionInterrupt = true,
        linkedListCapable = true,
        directCtrlCapable = true
      )
      if(withOutputs) channels += DmaSg.Channel(
        memoryToMemory = false,
        inputsPorts    = Nil,
        outputsPorts   = outputs.zipWithIndex.map(_._2),
        selfRestartCapable = true,
        progressProbes = false,
        halfCompletionInterrupt = true,
        linkedListCapable = true,
        directCtrlCapable = true
      )
      if(withInputs) channels += DmaSg.Channel(
        memoryToMemory = false,
        inputsPorts    = inputs.zipWithIndex.map(_._2),
        outputsPorts   = Nil,
        selfRestartCapable = true,
        progressProbes = true,
        halfCompletionInterrupt = false,
        linkedListCapable = true,
        directCtrlCapable = true
      )
      if(withMemoryToMemory) channels += DmaSg.Channel(
        memoryToMemory = true,
        inputsPorts    = Nil,
        outputsPorts   = Nil,
        selfRestartCapable = true,
        progressProbes = false,
        halfCompletionInterrupt = false,
        linkedListCapable = true,
        directCtrlCapable = true
      )
      channels += DmaSg.Channel(
        memoryToMemory = withMemoryToMemory,
        inputsPorts    = inputs.zipWithIndex.map(_._2),
        outputsPorts   = outputs.zipWithIndex.map(_._2),
        selfRestartCapable = true,
        progressProbes = true,
        halfCompletionInterrupt = true,
        linkedListCapable = true,
        directCtrlCapable = true
      )
      channels += DmaSg.Channel(
        memoryToMemory = withMemoryToMemory,
        inputsPorts    = inputs.zipWithIndex.map(_._2).grouped(2).map(_(0)).toSeq,
        outputsPorts   = outputs.zipWithIndex.map(_._2).grouped(2).map(_(1)).toSeq,
        selfRestartCapable = true,
        progressProbes = true,
        halfCompletionInterrupt = true,
        linkedListCapable = true,
        directCtrlCapable = true
      )
      channels += DmaSg.Channel(
        memoryToMemory = withMemoryToMemory,
        inputsPorts    = inputs.zipWithIndex.map(_._2).grouped(2).map(_(0)).toSeq,
        outputsPorts   = outputs.zipWithIndex.map(_._2).grouped(2).map(_(1)).toSeq,
        selfRestartCapable = true,
        progressProbes = false,
        halfCompletionInterrupt = true,
        linkedListCapable = false,
        directCtrlCapable = true
      )
      channels += DmaSg.Channel(
        memoryToMemory = withMemoryToMemory,
        inputsPorts    = inputs.zipWithIndex.map(_._2).grouped(2).map(_(0)).toSeq,
        outputsPorts   = outputs.zipWithIndex.map(_._2).grouped(2).map(_(1)).toSeq,
        selfRestartCapable = false,
        progressProbes = true,
        halfCompletionInterrupt = false,
        linkedListCapable = true,
        directCtrlCapable = false
      )
      channels += DmaSg.Channel(
        memoryToMemory = withMemoryToMemory,
        inputsPorts    = inputs.zipWithIndex.map(_._2),
        outputsPorts   = outputs.zipWithIndex.map(_._2),
        selfRestartCapable = false,
        progressProbes = true,
        halfCompletionInterrupt = true,
        linkedListCapable = true,
        directCtrlCapable = true,
        bytePerBurst = Some(0x20),
        fifoMapping = Some(0x600, 0x200)
      )

      parameters += name -> Parameter(
        readAddressWidth  = 24,
        readDataWidth     = 32,
        readLengthWidth   = 6,
        writeAddressWidth = 24,
        writeDataWidth    = 32,
        writeLengthWidth  = 6,
        sgAddressWidth = 24,
        sgReadDataWidth = 32,
        sgWriteDataWidth = 32,
        memory = DmaMemoryLayout(
          //      bankCount            = 1,
          //      bankWidth            = 32,
          bankCount            = 2,
          bankWidth            = 16,
          //      bankCount            = 4,
          //      bankWidth            = 8,
          //      bankCount            = 2,
          //      bankWidth            = 32,
          bankWords            = 1024,
          priorityWidth        = 2
        ),
        outputs = outputs,
        inputs = inputs,
        channels = channels,
        bytePerTransferWidth = 16,
        weightWidth = 2
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


