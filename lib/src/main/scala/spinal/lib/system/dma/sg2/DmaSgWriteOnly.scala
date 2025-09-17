package spinal.lib.system.dma.sg2

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bsb.{Bsb, BsbParameter}
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.fsm._
import spinal.lib.misc.pipeline.{StageCtrlPipeline, StagePipeline}
import spinal.lib.misc.slot.{Slot, SlotPool}

/*
Descriptor mapping :
- 32 status
- 32 control
- 64 from
- 64 to
- 64 next
 */

object DmaSgWriteOnly{
  val statusAt = 0
  val statusErrorAt = 29
  val statusLastAt = 30
  val statusCompletedAt = 31
  val statusBytesAt = 0
  val controlAt = 4
  val controlIrqAllAt = 31
  val controlIrqLastAt = 30
  val nextAt = 8
  val toAt = 16
}

case class DmaSgWriteOnlyParam(var addressWidth : Int,
                               var dataWidth : Int,
                               var blockSize : Int,
                               var bufferBytes : Int,
                               var pendingSlots : Int,
                               var bsbDataBytes : Int,
                               var descriptorBytes : Int = 32,
                               var irqDelayWidth : Int = 8,
                               var irqCounterWidth : Int = 8
                             ){
  def dataBytes = dataWidth/8
  def bsbDataWidth = bsbDataBytes*8
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
            get = SizeRange(descriptorBytes),
            putFull = SizeRange(blockSize),
            putPartial = SizeRange.upTo(blockSize)
          )
        )
      )
    )
  )
  def getBsbParameter() = BsbParameter(
    byteCount = bsbDataBytes,
    sourceWidth = 0,
    sinkWidth = 0,
    withMask = true,
    withError = true
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


class DmaSgWriteOnlyComp(val p: DmaSgWriteOnlyParam,
                         val ctrlParam: BusParameter) extends Component {
  val io = new Bundle{
    val ctrl = slave(Bus(ctrlParam))
    val mem = master(Bus(p.getM2sParameter(DmaSgWriteOnlyComp.this)))
    val bsb = slave(Bsb(p.getBsbParameter()))
    val interrupt = out Bool()
  }

  val ctrl = new SlaveFactory(io.ctrl, false)

  val logic = new DmaSgWriteOnly(
    p = p,
    bsb = io.bsb,
    mem = io.mem,
    ctrl = ctrl
  )
  io.interrupt := logic.onCtrl.irq.interrupt
}

class DmaSgWriteOnly(val p : DmaSgWriteOnlyParam,
                    val bsb : Bsb,
                    val mem : Bus,
                    val ctrl : BusSlaveFactory) extends Area {
  import DmaSgWriteOnly._
  assert(isPow2(p.descriptorBytes))
  assert(p.blockSize >= p.descriptorBytes)

  val burstRange = log2Up(p.blockSize)-1 downto 0
  val beatRange = log2Up(p.blockSize)-1 downto log2Up(p.dataBytes)
  val dataRange = log2Up(p.dataBytes)-1 downto 0

  assert(isPow2(p.bufferWords))
  case class Word() extends Bundle{
    val data = Bits(p.dataWidth bits)
//    val last = Bool()
  }
  val ram = Mem.fill(p.bufferWords)(Word())
  val ptrWidth = log2Up(p.bufferWords) + 1
  val PTR = HardType(UInt(ptrWidth bits))


  val onPush = new Area {
    val start = False
    val stop = RegInit(False)
    val descriptor = new Area {
      val self, next, to = Reg(UInt(p.addressWidth bits))
      val control = new Area {
        val bytes = Reg(UInt(p.transferBytesWidth bits))
        val irqAll, irqLast = Reg(Bool())
      }
      val status = new Area {
        val completed = RegInit(False)
        val last = Reg(Bool())
      }

      val irqEvent = False

      val error = RegInit(False)
      val toEnd = to(burstRange) +^ control.bytes
//      val toLast = toEnd - 1
//      val headOffset = to(burstRange)
//      val lastOffset = RegNext(toLast(burstRange))
//      val blocks = RegNext(toLast >> log2Up(p.blockSize))
//      val blockCounter = Reg(cloneOf(blocks))
//      val lastBlock = blockCounter === blocks
//      val blockBeats = lastOffset(beatRange).orMask(!lastBlock) - headOffset(beatRange).andMask(firstBlock) +^ 1



//      val packerOffset = Reg(UInt(burstRange.size bits))
//      val frontendLastWord = Reg(Bool())
//      frontendLastWord := ???
    }

    val config = new Area{
      val bsbNoSyncOnStart = RegInit(False)
    }

    val fsmState = new Area{
      val hit, target = Reg(UInt(p.transferBytesWidth + 1 bits))
      val packetLast, packetError = Reg(Bool())
      val bsbToBuffer = RegInit(False)
      val bsbSynced = Reg(False)
      val bsbDone = Reg(Bool())
      val bytesLeft = Reg(UInt(p.transferBytesWidth bits))

      val isBusy = Bool()
      val lastBlock = ((hit ^ target) >> log2Up(p.blockSize)) === 0
      val empty = lastBlock && (hit ^ target)(burstRange) === 0
      val startAt = hit(burstRange)
      val endAt = (target(burstRange) - 1).orMask(!lastBlock)
      val blockBytesMinusOne = endAt - startAt
      val askDrain = bsbDone || !lastBlock
      val blockBeatsMinusOne = endAt(beatRange) - startAt(beatRange)
    }

    val onRam = new Area {
      val write = ram.writePort()
      val read = ram.readSyncPort()
      val pushPtr = Reg(PTR) init (0)
      val fsmPtr = Reg(PTR) init (0)
      val popPtr = Reg(PTR) init (0)
      val full = (pushPtr ^ popPtr ^ p.bufferWords) === 0
    }


    val onBsb = new Area{
      val pip = new StagePipeline()
      val inserter = new pip.Area(0){
        arbitrateFrom(bsb)
        val DATA = insert(bsb.data)
        val MASK = insert(bsb.mask)
        val LAST = insert(bsb.last)
        val ERROR = insert(bsb.error)
      }
      val computer = new pip.Area(1){
        val off = CountOneOnEach(inserter.MASK)
        val OFFSETS = insert(Vec(U(0) +: CountOneOnEach(inserter.MASK).dropRight(1)))
        val ONES = insert(off.last)
      }
      val muxer = new pip.Area(2){
        val inputByteCount = widthOf(this(inserter.MASK))
        val mask = CombInit(this(inserter.MASK))
        val ones = CombInit(this(computer.ONES))
        val offsets = CombInit(this(computer.OFFSETS))
        val datas = CombInit(this(inserter.DATA))
        val consumed = Reg(UInt(log2Up(widthOf(mask)) bits)) init(0)
        val globalOffset = Reg(UInt(p.transferBytesWidth+1 bits))
//        val descriptorLastWord = ((globalOffset ^ descriptor.toLast) >> log2Up(p.dataBytes)).norR
//        val bytesMaxMinusOne = descriptorLastWord.mux(descriptor.lastOffset(dataRange), ~globalOffset(dataRange))
//        val bytesMax = bytesMaxMinusOne +^ 1
        val bytesMax = fsmState.bytesLeft.min(~globalOffset(dataRange)+^1)
        val bytesAvailable = ones-consumed

        // Actions
        val descriptorDataFull = globalOffset === descriptor.toEnd
        val fragmentFullyConsumed = bytesAvailable <= bytesMax
        val packetAskDescriptorCompletion = fragmentFullyConsumed && inserter.LAST
        val byteUsageCount = fragmentFullyConsumed.mux(bytesAvailable, bytesMax)

        val upReady = True
        val bufferFill = bytesAvailable =/= 0
        val bsbFirst = RegNextWhen[Bool](inserter.LAST, isFiring) init(True)

        when(stop && fsmState.bsbToBuffer && (!isValid || isReady)){
          fsmState.bsbToBuffer := False
        }

        bufferFill clearWhen(!valid)
        upReady clearWhen(!fragmentFullyConsumed)
        when(!fsmState.bsbToBuffer){
          bufferFill := False
          upReady := False
        }
        when(fsmState.bsbToBuffer && descriptorDataFull){
          bufferFill := False
          upReady := False
          fsmState.bsbToBuffer := False
        }
        when(valid && fsmState.bsbToBuffer && (fsmState.bsbSynced || bsbFirst) && inserter.ERROR){
          fsmState.packetError := True
        }

        when(bufferFill){
          consumed := consumed + byteUsageCount.resized
          globalOffset := globalOffset + byteUsageCount
          fsmState.bytesLeft := fsmState.bytesLeft - byteUsageCount
        }
        when(upReady){
          consumed := 0
          when((fsmState.bsbSynced || bsbFirst) && fsmState.bsbToBuffer && valid && packetAskDescriptorCompletion){
            fsmState.bsbToBuffer := False
            fsmState.packetLast := True
          }
        }

        ready := upReady

        val onBytes = for(i <- 0 until p.bsbDataBytes) yield new Area{
          val localOffset = consumed + i
          val oh = for(i <- 0 until inputByteCount) yield mask(i) && offsets(i) === localOffset
          val sel =  OHToUInt(oh)
          val valid = i < byteUsageCount
        }

        val buffer = new Area {
          val data = Reg(Bits(p.dataBytes*8 bits))
          val mask = Reg(Bits(p.dataBytes bits)) init (0)
          val write = mask.msb || mask =/= 0 && !fsmState.bsbToBuffer

          onRam.write.valid := write
          onRam.write.address := onRam.pushPtr.resized
          onRam.write.data.data := data
          when(onRam.write.fire){
            onRam.pushPtr := onRam.pushPtr + 1
            fsmState.target := globalOffset
            mask := 0
          }

          when(write && onRam.full){
            onRam.write.valid := False
            bufferFill := False
            upReady := False
          }
          when(!fsmState.isBusy || !fsmState.bsbSynced && !bsbFirst){
            bufferFill := False
            upReady := True
            mask := 0
          }
          fsmState.bsbSynced setWhen(isFiring && bsbFirst)


          when(!fsmState.bsbToBuffer && mask === 0){
            fsmState.bsbDone := True
          }

          @dontName val maskGroups = mask.subdivideIn(p.bsbDataBytes bits)
          @dontName val dataGroups = data.subdivideIn(p.bsbDataBytes*8 bits)
          val mappers = for(lane <- 0 until p.bsbDataBytes) yield new Area {
            assert(isPow2(inputByteCount))
            val from = U(lane)-globalOffset(dataRange)
            val reader = onBytes.reader(from.resized)
            val selected = reader(_.valid)
            val sel = reader(_.sel)
            val byte = datas.subdivideIn(8 bits)(sel)
            val chunkTmp = globalOffset(dataRange) + p.bsbDataBytes - 1 - lane
            val chunkId = chunkTmp(log2Up(p.dataBytes)-1 downto log2Up(p.bsbDataBytes))
            when(bufferFill && selected){
              maskGroups(chunkId)(lane) := True
              dataGroups(chunkId)(lane*8, 8 bits) := byte
            }
          }
        }
      }




//      val enabled = RegInit(False)
//      val buffer = Reg(Vec.fill(p.dataBytes)(Bits(8 bits)))
    }

    val pendings = new SlotPool(p.pendingSlots, true)(
      new Slot {}
    )

    mem.d.ready := True
    val onD = new Area {
      when(mem.d.fire && mem.d.isLast()) {
        pendings.free(mem.d.source)
      }
    }

    mem.a.valid := False
    mem.a.payload.assignDontCare()

    case class Cmd() extends Bundle {
      val blockId = UInt(p.transferBytesWidth + 1 - log2Up(p.blockSize) bits)
      val blockStartAt, blockEndAt = UInt(log2Up(p.blockSize) bits)
      val slotId = UInt(log2Up(p.pendingSlots) bits)
    }

    val onCmd = new Area{
      val input = Stream(Cmd())
      val pip = new StagePipeline()
      val inserter = new pip.Area(0) {
        input.ready := False
        valid := input.valid
        val CMD = insert(input.payload)
        onRam.read.cmd.valid := False
        onRam.read.cmd.payload := onRam.popPtr.resized

        val counter = Reg(UInt(log2Up(p.blockSize/p.dataBytes) bits)) init(0)
        val inBurstReg = RegInit(False)
        val WORD_FIRST = insert(counter === CMD.blockStartAt(beatRange))
        val WORD_LAST = insert(counter === CMD.blockEndAt(beatRange))
        val IN_BURST = insert(WORD_FIRST || inBurstReg)

        when(isFiring){
          onRam.read.cmd.valid := True
          inBurstReg setWhen(WORD_FIRST) clearWhen(WORD_LAST)
          counter := counter + 1
          when(IN_BURST) {
            onRam.popPtr := onRam.popPtr + 1
          }
          when(counter.andR){
            input.ready := True
          }
        }
      }

      val onReadRsp = new pip.Area(1){
        val WORD = insert(onRam.read.rsp)
        val maskStart = (0 until p.dataBytes).map(byteId => byteId >= inserter.CMD.blockStartAt(dataRange)).asBits
        val maskEnd = (0 until p.dataBytes).map(byteId => byteId <= inserter.CMD.blockEndAt(dataRange)).asBits
        val MASK = insert((maskStart.orMask(!(inserter.WORD_FIRST)) & maskEnd.orMask(!(inserter.WORD_LAST))).andMask(inserter.IN_BURST))
      }

      val onIo = new pip.Area(2){
        val isFull = inserter.CMD.blockStartAt.norR && inserter.CMD.blockEndAt.andR
        val counter = Reg(UInt(log2Up(p.blockSize/p.dataBytes) bits)) init(0)

        ready := mem.a.ready
        when(isValid){
          mem.a.valid := True
          mem.a.opcode := isFull.mux(Opcode.A.PUT_FULL_DATA, Opcode.A.PUT_PARTIAL_DATA)
          mem.a.param := Param.Hint.NO_ALLOCATE_ON_MISS
          mem.a.source := inserter.CMD.slotId
          mem.a.address := descriptor.to.clearedLow(log2Up(p.blockSize)) + (inserter.CMD.blockId << log2Up(p.blockSize))
          mem.a.size := log2Up(p.blockSize)
          mem.a.data := onReadRsp.WORD.data
          mem.a.mask := onReadRsp.MASK
        }
        when(isFiring){
          counter := counter + 1
        }
      }
    }

    val fsm = new StateMachine {
      val IDLE, NEXT_CMD, NEXT_RSP, DESCRIPTOR_CALC, WRITE_CMD, STATUS_CMD, STATUS_RSP = new State()
      setEntry(IDLE)

      fsmState.isBusy := !isActive(IDLE)
      IDLE.whenIsActive(
        when(start) {
          descriptor.self := descriptor.next
          fsmState.bsbSynced := config.bsbNoSyncOnStart
          goto(NEXT_CMD)
        }
      )

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
          map(descriptor.to, toAt, 0)
          map(descriptor.control.bytes, controlAt, 0)
          map(descriptor.control.irqAll, controlAt, controlIrqAllAt)
          map(descriptor.control.irqLast, controlAt, controlIrqLastAt)
          map(descriptor.status.last, statusAt, statusLastAt)
          map(descriptor.status.completed, statusAt, statusCompletedAt)
          descriptor.error := descriptor.error || mem.d.denied || mem.d.corrupt
          when(mem.d.isLast()) {
//            descriptor.blockCounter := 0
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
          onBsb.muxer.globalOffset := descriptor.to(burstRange).resized
          fsmState.target := descriptor.to(burstRange).resized
          fsmState.hit := descriptor.to(burstRange).resized
          fsmState.packetLast := False
          fsmState.packetError := False
          fsmState.bsbDone := False
          fsmState.bsbToBuffer := True
          fsmState.bytesLeft := descriptor.control.bytes
          goto(WRITE_CMD)
        }
      }

      onCmd.input.valid := False
      onCmd.input.blockId := fsmState.hit >> log2Up(p.blockSize)
      onCmd.input.blockStartAt := fsmState.startAt
      onCmd.input.blockEndAt := fsmState.endAt
      onCmd.input.slotId := pendings.allocate.id
      WRITE_CMD.whenIsActive {
        when(fsmState.askDrain && !fsmState.empty && !pendings.allocate.full) {
          onCmd.input.valid := True
          when(onCmd.input.ready) {
//            descriptor.blockCounter := descriptor.blockCounter + 1
            pendings.allocate { s => }
            onRam.fsmPtr := onRam.fsmPtr + fsmState.blockBeatsMinusOne + 1
            fsmState.hit := fsmState.hit + fsmState.blockBytesMinusOne + 1
          }
        }

        when(fsmState.bsbDone && !fsmState.bsbToBuffer && fsmState.empty) {
          goto(STATUS_CMD)
        }
      }

      STATUS_CMD.whenIsActive {
        when(pendings.isEmpty) {
          mem.a.valid := True
          mem.a.opcode := Opcode.A.PUT_FULL_DATA
          mem.a.param := 0
          mem.a.source := 0
          mem.a.address := descriptor.self
          mem.a.size := 2
          mem.a.data(31 downto 0) := (descriptor.control.bytes - fsmState.bytesLeft).asBits.resized
          mem.a.data(statusCompletedAt) := True
          mem.a.data(statusLastAt) := fsmState.packetLast
          mem.a.data(statusErrorAt) := fsmState.packetError
          mem.a.mask := 0xF
          when(mem.a.ready) {
            goto(STATUS_RSP)
          }
        }
      }

      STATUS_RSP.whenIsActive {
        when(mem.d.fire) {
          descriptor.irqEvent setWhen(descriptor.control.irqAll || descriptor.control.irqLast && fsmState.packetLast)
          goto(NEXT_CMD)
          when (stop) {
            goto(IDLE)
          }
        }
      }
    }
  }

  onPush.onBsb.pip.build()

  val onCtrl = new Area{
    def bus = ctrl
    bus.setOnSet(onPush.start, 0, 0)
    bus.read(onPush.fsmState.isBusy,0, 0)
    bus.readAndSetOnSet(onPush.stop, 0, 1)
    bus.write(onPush.config.bsbNoSyncOnStart, 8, 0)
    onPush.stop clearWhen(!onPush.fsmState.isBusy)


    val timeRef = CounterFreeRun(1023)
    val tick = timeRef.willOverflowIfInc

    val irq = new Area{
      val globalEnable = RegInit(False)
      bus.setOnSet(globalEnable, 0, 8)
      bus.clearOnSet(globalEnable, 0, 9)

      val idle = new Area{
        val enable = bus.createWriteOnly(Bool(), 4, 0) init(False)
        val irq = enable && !onPush.fsmState.isBusy
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

  onPush.onCmd.pip.build()
}
