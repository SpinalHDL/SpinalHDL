package spinal.lib.system.dma.sg2

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bsb.{Bsb, BsbParameter}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.fsm._
import spinal.lib.misc.pipeline.StagePipeline
import spinal.lib.misc.slot.{Slot, SlotPool}

/*
Descriptor mapping :
- 32 status
- 32 control
- 64 from
- 64 to
- 64 next
 */

object DmaSgReadOnly{
  val statusAt = 0
  val statusCompletedAt = 31
  val controlAt = 4
  val controlLastAt = 30
  val nextAt = 8
  val fromAt = 16
}

case class DmaSgReadOnlyParam(addressWidth : Int,
                              dataWidth : Int,
                              blockSize : Int,
                              bufferBytes : Int,
                              pendingSlots : Int,
                              var descriptorQueueSize : Int = 4,
                              var descriptorBytes : Int = 32
                             ){
  def dataBytes = dataWidth/8
  def transferBytesWidth = 27
  def bufferWords = bufferBytes/dataBytes
  def blocks = bufferBytes / blockSize


  def getM2sSupport() = M2sSupport(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    transfers = M2sTransfers(
      get = SizeRange(descriptorBytes, blockSize),
      putFull = SizeRange(4)
    )
  )
  def getM2sParameter(name : Nameable) = M2sParameters(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    masters = List(
      M2sAgent(
        name = name,
        mapping = M2sSource(
          id = SizeMapping(0, pendingSlots),
          emits = M2sTransfers(
            get = SizeRange(descriptorBytes, blockSize),
            putFull = SizeRange(4)
          )
        )
      )
    ),
  )
  def getBsbParameter() = BsbParameter(
    byteCount = dataBytes,
    sourceWidth = 0,
    sinkWidth = 0,
    withMask = true
  )
  def getCtrlSupport(proposed: bus.tilelink.M2sSupport) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = ctrlAddressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed
  )
  def getCtrlParam() = bus.tilelink.SlaveFactory.getParameter(
    addressWidth = ctrlAddressWidth,
    dataWidth = 32,
    allowBurst = false
  )
  def ctrlAddressWidth = 12
}

case class PopDescriptor(p : DmaSgReadOnlyParam) extends Bundle{
  val startAt, endAt = Reg(UInt(log2Up(p.blockSize) bits))
  val blocks = Reg(UInt(p.transferBytesWidth+1-log2Up(p.blockSize) bits))
  val last = Reg(Bool())
}

class DmaSgReadOnly(val p : DmaSgReadOnlyParam,
                    val ctrlParam : BusParameter,
                    val pushCd : ClockDomain,
                    val popCd : ClockDomain) extends Component {
  import DmaSgReadOnly._
  assert(isPow2(p.descriptorBytes))
  assert(p.blockSize >= p.descriptorBytes)

  val burstRange = log2Up(p.blockSize)-1 downto 0
  val beatRange = log2Up(p.blockSize)-1 downto log2Up(p.dataBytes)
  val dataRange = log2Up(p.dataBytes)-1 downto 0

  val io = new Bundle{
    val ctrl = slave(Bus(ctrlParam))
    val mem = master(Bus(p.getM2sParameter(DmaSgReadOnly.this)))
    val bsb = master(Bsb(p.getBsbParameter()))
    val interrupt = out Bool()
  }

  assert(isPow2(p.bufferWords))
  val ram = Mem.fill(p.bufferWords)(io.mem.p.data)
  val ptrWidth = log2Up(p.blocks) + 1
  val PTR = HardType(UInt(ptrWidth bits))

  val cc = new Area{
    val pushPtr, popPtr = PTR()
  }

  val onPush = pushCd on new Area {
    val start = False
    val stop = RegInit(False)
    val descriptor = new Area {
      val self, next, from = Reg(UInt(p.addressWidth bits))
      val control = new Area {
        val bytes = Reg(UInt(p.transferBytesWidth bits))
        val last = Reg(Bool())
      }
      val status = new Area {
        val completed = RegInit(False)
      }

      //    val fromPlusOne = KeepAttribute(from.clearedLow(log2Up(p.blockSize)) + p.blockSize)
      val error = RegInit(False)
      val firstBlock = Reg(Bool())
      val fromLast = from(burstRange) +^ control.bytes
      val blocks = RegNext(fromLast >> log2Up(p.blockSize))
      val headOffset = from(burstRange)
      val lastOffset = RegNext(fromLast(burstRange))
      val blockCounter = Reg(cloneOf(blocks))
      val lastBlock = blockCounter === blocks

      val toPop = new Area {
        val ready, done = RegInit(False)
        val stream = Stream(PopDescriptor(p))
        done setWhen(stream.fire)
        stream.valid := ready && !done
        stream.startAt := headOffset
        stream.endAt := lastOffset
        stream.blocks := blocks
        stream.last := control.last
      }
    }

    val onRam = new Area {
      val write = ram.writePort()
      val cmdPtr = Reg(PTR) init (0)
      val popPtr = DataCc(cc.popPtr, popCd, pushCd)
      val full = (cmdPtr ^ popPtr ^ (p.blocks)) === 0
    }


    val pendings = new SlotPool(p.pendingSlots, true)(
      new Slot {
        val target = Reg(PTR)
        val first, last = Reg(Bool())
      }
    )

    io.mem.a.valid := False
    io.mem.a.payload.assignDontCare()
    io.mem.d.ready := True

    val onD = new Area {
      val toBuffer = RegInit(False)

      val reader = pendings.slots.reader(io.mem.d.source)
      onRam.write.valid := False
      onRam.write.address := reader(_.target).resize(log2Up(p.blocks)) @@ io.mem.d.beatCounter()
      onRam.write.data := io.mem.d.data
      when(toBuffer && io.mem.d.fire) {
        onRam.write.valid := True
        when(io.mem.d.isLast()) {
          pendings.free(io.mem.d.source)
        }
      }
    }

    val fsm = new StateMachine {
      val IDLE, NEXT_CMD, NEXT_RSP, DESCRIPTOR_CALC, READ_CMD, STATUS_CMD, STATUS_RSP, FINALIZE = new State()
      setEntry(IDLE)

      val isBusy = !isActive(IDLE)
      IDLE.whenIsActive(
        when(start) {
          descriptor.self := descriptor.next
          goto(NEXT_CMD)
        }
      )


      NEXT_CMD.whenIsActive {
        descriptor.error := False
        io.mem.a.valid := True
        io.mem.a.opcode := Opcode.A.GET
        io.mem.a.param := 0
        io.mem.a.source := 0
        io.mem.a.address := descriptor.next
        io.mem.a.size := log2Up(p.descriptorBytes)
        when(io.mem.a.ready) {
          goto(NEXT_RSP)
        }
        when(stop){
          io.mem.a.valid := False
          stop := False
          goto(IDLE)
        }
      }


      def beatHit(offset: Int) = offset / p.dataBytes === io.mem.d.beatCounter()

      def map[T <: Data](target: T, byte: Int, bit: Int) {
        val targetWidth = widthOf(target)
        var targetPtr = 0
        var sourcePtr = byte * 8 + bit
        while (targetPtr != targetWidth) {
          val w = Math.min(targetWidth - targetPtr, p.dataWidth - sourcePtr % p.dataWidth)
          when(sourcePtr / p.dataWidth === io.mem.d.beatCounter()) {
            target.assignFromBits(io.mem.d.data(sourcePtr % p.dataWidth, w bits), targetPtr, w bits)
          }
          targetPtr += w
          sourcePtr += w
        }
      }

      NEXT_RSP.whenIsActive {
        when(io.mem.d.fire) {
          map(descriptor.next, nextAt, 0)
          map(descriptor.from, fromAt, 0)
          map(descriptor.control.bytes, controlAt, 0)
          map(descriptor.control.last, controlAt, controlLastAt)
          map(descriptor.status.completed, statusAt, statusCompletedAt)
          descriptor.error := descriptor.error || io.mem.d.denied || io.mem.d.corrupt
          when(io.mem.d.isLast()) {
            descriptor.firstBlock := True
            descriptor.toPop.ready := False
            descriptor.toPop.done := False
            descriptor.blockCounter := 0
            goto(DESCRIPTOR_CALC)
          }
        }
      }

      DESCRIPTOR_CALC.whenIsActive {
        when(descriptor.error) {
          goto(IDLE) //TODO
        } elsewhen (descriptor.status.completed) {
          goto(IDLE) //TODO
        } otherwise {
          onD.toBuffer := True
          goto(READ_CMD)
        }
      }

      READ_CMD.whenIsActive {
        io.mem.a.opcode := Opcode.A.GET
        io.mem.a.param := 0
        io.mem.a.source := pendings.allocate.id
        io.mem.a.address := descriptor.from.clearedLow(log2Up(p.blockSize)) + (descriptor.blockCounter << log2Up(p.blockSize))
        io.mem.a.size := log2Up(p.blockSize)
        when(!onRam.full && !pendings.allocate.full) {
          io.mem.a.valid := True
          when(io.mem.a.ready) {
            descriptor.blockCounter := descriptor.blockCounter + 1
            descriptor.firstBlock := False
            pendings.allocate { s =>
              s.target := onRam.cmdPtr
              s.first := descriptor.firstBlock
              s.last := descriptor.lastBlock
            }
            onRam.cmdPtr := onRam.cmdPtr + 1
            when(descriptor.lastBlock) {
              goto(STATUS_CMD)
            }
          }
        }
      }

      STATUS_CMD.whenIsActive {
        when(pendings.isEmpty) {
          descriptor.toPop.ready := True
          onD.toBuffer := False
          io.mem.a.valid := True
          io.mem.a.opcode := Opcode.A.PUT_FULL_DATA
          io.mem.a.param := 0
          io.mem.a.source := 0
          io.mem.a.address := descriptor.self
          io.mem.a.size := 2
          io.mem.a.data := 0
          io.mem.a.data(statusCompletedAt) := True
          io.mem.a.mask := 0xF
          when(io.mem.a.ready) {
            goto(STATUS_RSP)
          }
        }
      }

      STATUS_RSP.whenIsActive {
        when(io.mem.d.fire) {
          goto(FINALIZE)
        }
      }

      FINALIZE whenIsActive {
        when(descriptor.toPop.done) {
          goto(NEXT_CMD)
        }
      }
    }
  }


  val onPop = popCd on new Area{
    val descriptorCc = onPush.descriptor.toPop.stream.queue(p.descriptorQueueSize, pushCd, popCd)
    val descriptor = descriptorCc.stage()

    val pip = new StagePipeline()
    val onRam = new Area {
      val read = ram.readSyncPort(clockCrossing = true)
      val popPtr = Reg(PTR()) init(0)
      cc.popPtr := popPtr
    }

    val onInsert = new pip.Area(0){
      valid := descriptor.valid
      val blockCounter = Reg(descriptor.blocks) init(0)
      val blockFirst = RegInit(True)
      val blockLast = blockCounter === descriptor.blocks

      val wordCounter = Reg(UInt(log2Up(p.blockSize/p.dataBytes) bits)) init(0)
      val wordFirst = wordCounter === 0
      val wordId = wordCounter + descriptor.startAt(beatRange).andMask(blockFirst)
      val wordLast = wordId === descriptor.endAt(beatRange).orMask(!blockLast)
      val maskStart = (0 until p.dataBytes).map(byteId => byteId >= descriptor.startAt(dataRange)).asBits
      val maskEnd = (0 until p.dataBytes).map(byteId => byteId <= descriptor.endAt(dataRange)).asBits
      val MASK = insert(maskStart.orMask(!(blockFirst && wordFirst)) & maskEnd.orMask(!(blockLast && wordLast)))
      val LAST = insert(descriptor.last && wordLast && blockLast)

      onRam.read.cmd.valid := False
      onRam.read.cmd.payload := onRam.popPtr.resize(log2Up(p.blocks)) @@ wordId

      descriptor.ready := False
      when(isValid && isReady){
        onRam.read.cmd.valid := True
        wordCounter := wordCounter + 1
        when(wordLast){
          wordCounter := 0
          blockFirst := False
          onRam.popPtr := onRam.popPtr + 1
          blockCounter := blockCounter + 1
          when(blockLast){
            blockCounter := 0
            descriptor.ready := True
            blockFirst := True
          }
        }
      }
    }

    val onReadRsp = new pip.Area(1){
      val DATA = insert(onRam.read.rsp)
    }

    val onIo = new pip.Area(1){
      arbitrateTo(io.bsb)
      io.bsb.data := onReadRsp.DATA
      io.bsb.mask := onInsert.MASK
      io.bsb.last := onInsert.LAST
    }
  }
  popCd(onPop.pip.build())

  val onCtrl = pushCd on new Area{
    val bus = new SlaveFactory(io.ctrl, false)
    bus.setOnSet(onPush.start, 0, 0)
    bus.read(onPush.fsm.isBusy,0, 0)
    bus.readAndSetOnSet(onPush.stop, 0, 1)

    val irq = new Area{
      val idleEnable = bus.createWriteOnly(Bool(), 4, 0) init(False)
      io.interrupt := idleEnable && !onPush.fsm.isBusy
    }

    bus.writeMultiWord(onPush.descriptor.next, 0x10)
  }
}
