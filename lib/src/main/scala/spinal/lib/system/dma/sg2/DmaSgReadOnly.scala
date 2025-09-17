package spinal.lib.system.dma.sg2

import spinal.core._
import spinal.core.sim.SimConfig
import spinal.lib._
import spinal.lib.bus.bsb.{Bsb, BsbParameter}
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.eda.bench.{Bench, EfinixStdTargets, Rtl}
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
  val controlIrqAt = 31
  val nextAt = 8
  val fromAt = 16
}

case class DmaSgReadOnlyParam(var addressWidth : Int,
                              var dataWidth : Int,
                              var blockSize : Int,
                              var bufferBytes : Int,
                              var pendingSlots : Int,
                              var descriptorBytes : Int = 32,
                              var irqDelayWidth : Int = 8,
                              var irqCounterWidth : Int = 8
                             ){
  def dataBytes = dataWidth/8
  def transferBytesWidth = 27
  def bufferWords = bufferBytes/dataBytes


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
    )
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

class DmaSgReadOnlyComp(val p: DmaSgReadOnlyParam,
                        val ctrlParam: BusParameter,
                        val pushCd: ClockDomain,
                        val popCd: ClockDomain) extends Component {
  val io = new Bundle{
    val ctrl = slave(Bus(ctrlParam))
    val mem = master(Bus(p.getM2sParameter(DmaSgReadOnlyComp.this)))
    val bsb = master(Bsb(p.getBsbParameter()))
    val interrupt = out Bool()
  }

  val ctrl = pushCd(new SlaveFactory(io.ctrl, false))

  val logic = new DmaSgReadOnly(
    p = p,
    bsb = io.bsb,
    mem = io.mem,
    ctrl = ctrl,
    pushCd = pushCd,
    popCd = popCd
  )
  io.interrupt := logic.onCtrl.irq.interrupt
}

class DmaSgReadOnly(val p : DmaSgReadOnlyParam,
                    val bsb : Bsb,
                    val mem : Bus,
                    val ctrl : BusSlaveFactory,
                    val pushCd : ClockDomain,
                    val popCd : ClockDomain) extends Area {
  import DmaSgReadOnly._
  assert(isPow2(p.descriptorBytes))
  assert(p.blockSize >= p.descriptorBytes)

  val burstRange = log2Up(p.blockSize)-1 downto 0
  val beatRange = log2Up(p.blockSize)-1 downto log2Up(p.dataBytes)
  val dataRange = log2Up(p.dataBytes)-1 downto 0

  assert(isPow2(p.bufferWords))
  case class Word() extends Bundle{
    val data = Bits(p.dataWidth bits)
    val mask = Bits(p.dataBytes bits)
    val last = Bool()
  }
  val ram = Mem.fill(p.bufferWords)(Word())
  val ptrWidth = log2Up(p.bufferWords) + 1
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
        val irq = Reg(Bool())
      }
      val status = new Area {
        val completed = RegInit(False)
      }

      val irqEvent = False

      val error = RegInit(False)
      val firstBlock = Reg(Bool())
      val fromLast = from(burstRange) +^ control.bytes - 1
      val headOffset = from(burstRange)
      val lastOffset = RegNext(fromLast(burstRange))
      val blocks = RegNext(fromLast >> log2Up(p.blockSize))
      val blockCounter = Reg(cloneOf(blocks))
      val lastBlock = blockCounter === blocks
      val blockBeats = lastOffset(beatRange).orMask(!lastBlock) - headOffset(beatRange).andMask(firstBlock) +^ 1

      val toPop = new Area {
        val ready, done = RegInit(False)
        val stream = Stream(PopDescriptor(p))
        done setWhen(stream.fire)
        stream.valid := ready && !done
        stream.startAt := headOffset
        stream.endAt := lastOffset
        stream.blocks := blocks
        stream.last := control.last
        stream.ready := True
      }
    }

    val onRam = new Area {
      val write = ram.writePort()
      val cmdPtr = Reg(PTR) init (0)
      val popPtr = DataCc(cc.popPtr, popCd, pushCd)(U(0, ptrWidth bits))
      val full = cmdPtr - popPtr > p.bufferWords - descriptor.blockBeats
    }


    val pendings = new SlotPool(p.pendingSlots, true)(
      new Slot {
        val target = Reg(PTR)
        val first, last = Reg(Bool())
        val ptr = Reg(PTR())
      }
    )

    val pushPtr = new Area{
      val counter = Reg(PTR) init(0)
      val fromD = RegInit(False)
      val force = False

      when(onRam.cmdPtr =/= counter && pendings.slots.map(s => s.valid && s.ptr === counter).norR){  //force || !fromD &&
        counter := counter + 1
      }
      cc.pushPtr := counter
    }

    mem.a.valid := False
    mem.a.payload.assignDontCare()
    mem.d.ready := True

    val onD = new Area {
      val toBuffer = RegInit(False)

      val reader = pendings.slots.reader(mem.d.source)
      val blockFirst = reader(_.first)
      val blockLast = reader(_.last)

      val wordFirst = mem.d.beatCounter() === descriptor.headOffset(beatRange) && blockFirst
      val wordLast = mem.d.beatCounter() === descriptor.lastOffset(beatRange) && blockLast
      val hit0 =  mem.d.beatCounter() >= descriptor.headOffset(beatRange) || !blockFirst
      val hit1 =  mem.d.beatCounter() <= descriptor.lastOffset(beatRange) || !blockLast
      val hit = hit0 && hit1

      val maskStart = (0 until p.dataBytes).map(byteId => byteId >= descriptor.headOffset(dataRange)).asBits
      val maskEnd = (0 until p.dataBytes).map(byteId => byteId <= descriptor.lastOffset(dataRange)).asBits
      val mask = maskStart.orMask(!(wordFirst)) & maskEnd.orMask(!(wordLast))

      val counter = Reg(UInt(beatRange.size bits)) init(0)
      val ptr = reader(_.ptr) + counter
      onRam.write.valid := False
      onRam.write.address := ptr.resized
      onRam.write.data.data := mem.d.data
      onRam.write.data.mask := mask
      onRam.write.data.last := wordLast && descriptor.control.last

      val pushHit = pushPtr.fromD || ptr === pushPtr.counter
      when(toBuffer && mem.d.fire) {
        when(pushHit) {
          pushPtr.fromD := True
        }
        when(hit) {
          onRam.write.valid := True
          counter := counter + 1
          when(pushHit) {
            pushPtr.force := True
          }
        }
        when(mem.d.isLast()) {
          pendings.free(mem.d.source)
          counter := 0
          pushPtr.fromD := False
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


      val isGoingIdleValid = !(isActive(NEXT_CMD) || isActive(NEXT_RSP) || isActive(DESCRIPTOR_CALC))
      val isGoingIdle = !isBusy || descriptor.status.completed

      NEXT_CMD.whenIsActive {
        descriptor.error := False
        mem.a.valid := True
        mem.a.opcode := Opcode.A.GET
        mem.a.param := 0
        mem.a.source := 0
        mem.a.address := descriptor.next
        mem.a.size := log2Up(p.descriptorBytes)
        when(mem.a.ready) {
          descriptor.self := descriptor.next
          goto(NEXT_RSP)
        }
      }

      def map[T <: Data](target: T, byte: Int, bit: Int) {
        val targetWidth = widthOf(target)
        var targetPtr = 0
        var sourcePtr = byte * 8 + bit
        while (targetPtr != targetWidth) {
          val w = Math.min(targetWidth - targetPtr, p.dataWidth - sourcePtr % p.dataWidth)
          when(sourcePtr / p.dataWidth === mem.d.beatCounter()) {
            target.assignFromBits(mem.d.data(sourcePtr % p.dataWidth, w bits), targetPtr, w bits)
          }
          targetPtr += w
          sourcePtr += w
        }
      }

      NEXT_RSP.whenIsActive {
        when(mem.d.fire) {
          map(descriptor.next, nextAt, 0)
          map(descriptor.from, fromAt, 0)
          map(descriptor.control.bytes, controlAt, 0)
          map(descriptor.control.last, controlAt, controlLastAt)
          map(descriptor.control.irq, controlAt, controlIrqAt)
          map(descriptor.status.completed, statusAt, statusCompletedAt)
          descriptor.error := descriptor.error || mem.d.denied || mem.d.corrupt
          when(mem.d.isLast()) {
            descriptor.firstBlock := True
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
          descriptor.toPop.ready := True
          onD.toBuffer := True
          goto(READ_CMD)
        }
      }

      READ_CMD.whenIsActive {
        mem.a.opcode := Opcode.A.GET
        mem.a.param := Param.Hint.NO_ALLOCATE_ON_MISS
        mem.a.source := pendings.allocate.id
        mem.a.address := descriptor.from.clearedLow(log2Up(p.blockSize)) + (descriptor.blockCounter << log2Up(p.blockSize))
        mem.a.size := log2Up(p.blockSize)
        when(!onRam.full && !pendings.allocate.full) {
          mem.a.valid := True
          when(mem.a.ready) {
            descriptor.blockCounter := descriptor.blockCounter + 1
            descriptor.firstBlock := False
            pendings.allocate { s =>
              s.target := onRam.cmdPtr
              s.first := descriptor.firstBlock
              s.last := descriptor.lastBlock
              s.ptr := onRam.cmdPtr
            }
            onRam.cmdPtr := onRam.cmdPtr + descriptor.blockBeats
            when(descriptor.lastBlock) {
              goto(STATUS_CMD)
            }
          }
        }
      }

      STATUS_CMD.whenIsActive {
        when(pendings.isEmpty) {
          onD.toBuffer := False
          mem.a.valid := True
          mem.a.opcode := Opcode.A.PUT_FULL_DATA
          mem.a.param := 0
          mem.a.source := 0
          mem.a.address := descriptor.self
          mem.a.size := 2
          mem.a.data := 0
          mem.a.data(statusCompletedAt) := True
          mem.a.mask := 0xF
          when(mem.a.ready) {
            goto(STATUS_RSP)
          }
        }
      }

      STATUS_RSP.whenIsActive {
        when(mem.d.fire) {
          descriptor.irqEvent setWhen(descriptor.control.irq)
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
    val pushPtr = DataCc(cc.pushPtr, pushCd, popCd)(cc.pushPtr.getZero)
    val popPtr = Reg(PTR()) init(0)
    val empty = popPtr === pushPtr
    cc.popPtr := popPtr

    val pip = new StagePipeline()
    val onRam = new Area {
      val read = ram.readSyncPort(clockCrossing = true)
    }

    val onInsert = new pip.Area(0){
      valid := !empty
      onRam.read.cmd.valid := False
      onRam.read.cmd.payload := popPtr.resized
      when(isFiring){
        onRam.read.cmd.valid := True
        popPtr := popPtr + 1
      }
    }

    val onReadRsp = new pip.Area(1){
      val WORD = insert(onRam.read.rsp)
    }

    val onIo = new pip.Area(1){
      arbitrateTo(bsb)
      bsb.data := onReadRsp.WORD.data
      bsb.mask := onReadRsp.WORD.mask
      bsb.last := onReadRsp.WORD.last
    }
  }
  popCd(onPop.pip.build())

  val onCtrl = pushCd on new Area{
    def bus = ctrl
    bus.setOnSet(onPush.start, 0, 0)
    bus.read(onPush.fsm.isBusy,0, 0)
    bus.readAndSetOnSet(onPush.stop, 0, 1)

    val idleTester = new Area{
      val busy = RegInit(False) clearWhen(onPush.fsm.isGoingIdleValid)
      val value = RegNextWhen(onPush.fsm.isGoingIdle, busy) init(False)
      bus.readAndSetOnSet(busy, 0, 4)
      bus.read(value, 0, 5)
    }

    val timeRef = CounterFreeRun(1023)
    val tick = timeRef.willOverflowIfInc

    val irq = new Area{
      val globalEnable = RegInit(False)
      bus.setOnSet(globalEnable, 0, 8)
      bus.clearOnSet(globalEnable, 0, 9)

      val idle = new Area{
        val enable = bus.createWriteOnly(Bool(), 4, 0) init(False)
        val irq = enable && !onPush.fsm.isBusy
      }
      val delay = new Area{
        val enable = bus.createWriteOnly(Bool(), 4, 1) init(False)
        val target = bus.createWriteOnly(UInt(p.irqDelayWidth bits), 4, 16) init(0)
        val hit = Reg(UInt(p.irqDelayWidth bits))
        val done = hit === target
        val clear = bus.isWriting(4) || onPush.descriptor.irqEvent
        val counting = RegInit(False) clearWhen(clear) setWhen(onPush.descriptor.irqEvent)
        when(tick && counting && !done){
          hit := hit + 1
        }
        when(clear){
          hit := 0
        }
        val irq = enable && done
      }
      val counter = new Area{
        val enable = bus.createWriteOnly(Bool(), 4, 2) init(False)
        val counter = bus.createWriteOnly(UInt(p.irqCounterWidth bits), 4, 24) init(0)
        val done = counter === 0
        when(onPush.descriptor.irqEvent && !done){
          counter := counter - 1
        }
        val irq = enable && done
      }
      val interrupt = globalEnable && (idle.irq || delay.irq || counter.irq)
    }

    bus.writeMultiWord(onPush.descriptor.next, 0x10)
  }
}


object DmaSgReadOnlySynt extends App{
  val p = DmaSgReadOnlyParam(
    addressWidth = 32,
    dataWidth = 64,
    blockSize = 64,
    bufferBytes = 512,
    pendingSlots = 4
  )
  val ctrlParam = p.getCtrlParam()

  val rtls = List(Rtl(SpinalVerilog(Rtl.xorOutputs(Rtl.ffIo(new DmaSgReadOnlyComp(p, ctrlParam, ClockDomain.current, ClockDomain.current))))))
  val targets = EfinixStdTargets().drop(2)
  Bench(rtls, targets)
  System.exit(0)

  val compiled = SimConfig.compile(
    new DmaSgReadOnlyComp(p, ctrlParam, ClockDomain.external("push"), ClockDomain.external("pop"))
  )
}