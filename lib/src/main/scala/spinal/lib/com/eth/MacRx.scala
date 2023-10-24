package spinal.lib.com.eth

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory


case class MacRxPreamble(dataWidth : Int) extends Component{
  val io = new Bundle {
    val input = slave(Stream(Fragment(PhyRx(dataWidth))))
    val output = master(Stream(Fragment(PhyRx(dataWidth))))
  }

  val startDelimiter = 0xD5
  val startDelimiterWidth = 8
  val history = History(io.input, 0 until startDelimiterWidth/dataWidth, when = io.input.fire)
  val historyDataCat = B(Cat(history.map(_.data).reverse))
  val hit = history.map(_.valid).andR && historyDataCat === startDelimiter
  val inFrame = RegInit(False)

  io.output.valid := False
  io.output.payload  := io.input.payload
  io.input.ready := !inFrame || io.output.ready

  when(!inFrame){
    when(hit){
      inFrame := True
    }
  } otherwise {
    when(io.input.valid) {
      io.output.valid := True
      when(io.output.ready && io.input.last){
        inFrame := False
      }
    }
  }
}


case class CrcKind(polynomial      : BigInt,
                   polynomialWidth : Int,
                   initValue       : BigInt,
                   inputReflected  : Boolean,
                   outputReflected : Boolean,
                   finalXor        : BigInt)

object CrcKind{
  val Crc32 =  new CrcKind(
    polynomial = BigInt("04C11DB7", 16),
    polynomialWidth = 32,
    initValue = BigInt("FFFFFFFF", 16),
    inputReflected = true,
    outputReflected = true,
    finalXor = BigInt("FFFFFFFF", 16)
  )
  val usb = new {
    val crc5 = new CrcKind(
      polynomial = BigInt("5", 16),
      polynomialWidth = 5,
      initValue = BigInt("1F", 16),
      inputReflected = true,
      outputReflected = true,
      finalXor = BigInt("1F", 16)
    )
    val crc16 = new CrcKind(
      polynomial = BigInt("8005", 16),
      polynomialWidth = 16,
      initValue = BigInt("FFFF", 16),
      inputReflected = true,
      outputReflected = true,
      finalXor = BigInt("FFFF", 16)
    )
    val crc5Check = new CrcKind(
      polynomial = BigInt("5", 16),
      polynomialWidth = 5,
      initValue = BigInt("1F", 16),
      inputReflected = true,
      outputReflected = false,
      finalXor = BigInt("00", 16)
    )
    val crc16Check = new CrcKind(
      polynomial = BigInt("8005", 16),
      polynomialWidth = 16,
      initValue = BigInt("FFFF", 16),
      inputReflected = true,
      outputReflected = false,
      finalXor = BigInt("0000", 16)
    )
  }
}

case class Crc(kind : CrcKind, dataWidth : Int) extends Component{
  val io = new Bundle {
    val flush = in Bool()
    val input = slave Flow(Bits(dataWidth bits))
    val result = out Bits(kind.polynomialWidth bits)
    val resultNext = out Bits(kind.polynomialWidth bits)
  }

  val state = Reg(Bits(kind.polynomialWidth bits)) init(kind.initValue)

  var acc = CombInit(state)
  for(i <- if(kind.inputReflected) 0 to dataWidth-1 else dataWidth-1 downto 0){
    acc \= ((acc |<< 1) ^ ((io.input.payload(i) ^ acc.msb) ? B(kind.polynomial, kind.polynomialWidth bits) | B(0, kind.polynomialWidth bits)))
  }

  when(io.input.fire){
    state := acc
  }
  when(io.flush){
    state := kind.initValue
  }

  val stateXor = state ^ kind.finalXor
  val accXor = acc ^ kind.finalXor
  io.result := (if(kind.outputReflected) stateXor.asBools.reverse.asBits() else stateXor)
  io.resultNext := (if(kind.outputReflected) accXor.asBools.reverse.asBits() else accXor)
}

case class MacRxChecker(dataWidth : Int) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Fragment(PhyRx(dataWidth))))
    val output = master(Stream(Fragment(PhyRx(dataWidth))))
  }

  val crc = Crc(CrcKind.Crc32, dataWidth)
  crc.io.input.valid := io.input.valid
  crc.io.input.payload := io.input.data
  crc.io.flush := io.output.lastFire

  val crcHit = crc.io.resultNext === 0x2144DF1C

  io.output.valid := io.input.valid
  io.output.last := io.input.last
  io.output.data := io.input.data
  io.output.error := io.input.error | io.input.last && !crcHit
  io.input.ready := io.output.ready
}


case class MacRxAligner(dataWidth : Int) extends Component{
  val io = new Bundle{
    val enable = in Bool()
    val input = slave(Stream(Fragment(PhyRx(dataWidth))))
    val output = master(Stream(Fragment(PhyRx(dataWidth))))
  }
  val alignWidth = 16
  assert(dataWidth <= alignWidth)

  val stateCount = alignWidth/dataWidth
  val state = Reg(UInt(log2Up(stateCount + 1) bits)) init(0)

  io.output << io.input
  when(io.enable && state =/= stateCount){
    io.output.valid := True
    io.output.last := False
    io.output.error := False
    io.input.ready := False
    when(io.output.ready){
      state := state + 1
    }
  }

  when(io.input.lastFire){
    state := 0
  }
}


case class MacRxBuffer(pushCd : ClockDomain,
                       popCd : ClockDomain,
                       pushWidth : Int,
                       popWidth : Int,
                       byteSize : Int) extends Component {
  assert(isPow2(byteSize))
  assert(isPow2(popWidth/pushWidth))
  assert(popWidth >= pushWidth)
  assert(popWidth >= 16)

  val io = new Bundle {
    val push = new Bundle{
      val stream = slave(Stream(Fragment(PhyRx(pushWidth))))
      val drop = out Bool()
      val commit = out Bool()
      val error = out Bool()
    }

    val pop = new Bundle{
      val stream = master(Stream(Bits(popWidth bits)))
      val stats = new Bundle {
        val clear = in Bool()
        val drops, errors = out UInt (8 bits)
      }
    }
  }

  val ram = Mem(Bits(popWidth bits), byteSize*8/popWidth)
  val ptrWidth = ram.addressWidth + 1

  val popToPush = new StreamCCByToggle(UInt(ptrWidth bits), popCd, pushCd, withOutputBuffer = false)
  val pushToPop = new StreamCCByToggle(UInt(ptrWidth bits), pushCd, popCd, withOutputBuffer = false)

  popToPush.io.input.valid := True
  popToPush rework { popToPush.pushArea.data init(0) }

  pushToPop.io.input.valid := True
  pushToPop rework { pushToPop.pushArea.data init(0) }

  def isFull(a: UInt, b: UInt) = a.msb =/= b.msb && a(ptrWidth - 2 downto 0) === b(ptrWidth - 2 downto 0)
  def isEmpty(a: UInt, b: UInt) = a === b

  val lengthWidth = log2Up(byteSize*8)

  val push = pushCd on new Area{
    val currentPtr, oldPtr = Reg(UInt(ptrWidth bits)) init(0)
    val currentPtrPlusOne = currentPtr + 1
    val popPtr  = popToPush.io.output.toFlow.toReg(0).addTag(crossClockDomain)
    pushToPop.io.input.payload := oldPtr

    val ratio = popWidth/pushWidth
    val buffer = Reg(Bits(popWidth-pushWidth bits))
    val state = Reg(UInt(log2Up(ratio) bits)) init(0)
    val length = Reg(UInt(lengthWidth bits)) init(0)


    val port = ram.writePort
    port.valid := False
    port.payload.assignDontCare()

    val error = RegInit(False)
    val drop = RegInit(False)
    when(io.push.stream.fire && io.push.stream.error){
      error := True
    }

    val doWrite = False
    when(io.push.stream.fire){
      state := state + 1
      length := length + pushWidth
      for(i <- 0 to ratio-2) when(state === i){
        buffer(i*pushWidth, pushWidth bits) := io.push.stream.data
      }
      when(state === ratio-1){
        doWrite := True
      }
    }


    val full = isFull(currentPtrPlusOne, popPtr)
    when(doWrite){
      when(full){
        drop := True
      } otherwise {
        port.valid := True
        port.address := currentPtrPlusOne.resized
        port.data := io.push.stream.data ## buffer
        currentPtr := currentPtrPlusOne
      }
    }

    val cleanup =  RegNext(io.push.stream.lastFire) init(False)
    val commit = RegNext(cleanup) init(False)
    io.push.stream.ready := !cleanup && !commit

    when(cleanup && state =/= 0){
      doWrite := True
    }

    when(commit){
      when(error || drop || full) {
        currentPtr := oldPtr
      } otherwise {
        oldPtr := currentPtrPlusOne
        currentPtr := currentPtrPlusOne
        port.valid := True
        port.address := oldPtr.resized
        port.data := B(length).resized
      }
      error := False
      drop := False
      state := 0
      length := 0
    }

    io.push.drop := drop || commit && full
    io.push.commit := commit
    io.push.error := error
  }

  val pop = popCd on new Area{
    val currentPtr = Reg(UInt(ptrWidth bits)) init(0)
    val pushPtr = pushToPop.io.output.toFlow.toReg(0).addTag(crossClockDomain)
    popToPush.io.input.payload := currentPtr

    val cmd = Stream(ram.addressType())
    cmd.valid := !isEmpty(currentPtr, pushPtr)
    cmd.payload := currentPtr.resized

    io.pop.stream << ram.streamReadSync(cmd, crossClock = true)

    when(cmd.fire){
      currentPtr := currentPtr + 1
    }

    val stats = new Area{
      val drop = PulseCCByToggle(
        input = push.commit && push.drop,
        clockIn = pushCd,
        clockOut = popCd
      )
      val error = PulseCCByToggle(
        input = push.commit && push.error,
        clockIn = pushCd,
        clockOut = popCd
      )

      val drops, errors = Reg(UInt(8 bits)) init(0)
      drops := drops + U(drop && drops =/= drops.maxValue)
      errors := errors + U(error && errors =/= errors.maxValue)
      when(io.pop.stats.clear){
        drops := 0
        errors := 0
      }

      io.pop.stats.drops := drops
      io.pop.stats.errors := errors
    }
  }
}


