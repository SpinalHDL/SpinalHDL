package spinal.lib.com.eth

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory


case class PhyRx(dataWidth : Int) extends Bundle {
  val error = Bool()
  val data = Bits(dataWidth bits)
}


case class PhyTx(dataWidth : Int) extends Bundle {
  val data = Bits(dataWidth bits)
}

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
}

case class Crc(kind : CrcKind, dataWidth : Int) extends Component{
  val io = new Bundle {
    val flush = in Bool()
    val input = slave Flow(Bits(dataWidth bits))
    val result = out Bits(kind.polynomialWidth bits)
    val resultNext = out Bits(kind.polynomialWidth bits)
  }

  val state = Reg(Bits(kind.polynomialWidth bits)) init(kind.initValue)

  var acc = state
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
  io.result := (if(kind.inputReflected) stateXor.asBools.reverse.asBits() else stateXor)
  io.resultNext := (if(kind.inputReflected) accXor.asBools.reverse.asBits() else accXor)
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



case class MacRxBuffer(pushCd : ClockDomain,
                       popCd : ClockDomain,
                       pushWidth : Int,
                       popWidth : Int,
                       byteSize : Int,
                       lengthMax : Int) extends Component {
  assert(isPow2(byteSize))
  assert(isPow2(popWidth/pushWidth))
  assert(popWidth >= pushWidth)
  assert(popWidth >= 16)

  val io = new Bundle {
    val push = new Bundle{
      val stream = slave(Stream(Fragment(PhyRx(pushWidth))))
    }

    val pop = new Bundle{
      val stream = master(Stream(Bits(popWidth bits)))
    }
  }

  val ram = Mem(Bits(popWidth bits), byteSize*8/popWidth)
  val ptrWidth = ram.addressWidth + 1

  val popToPushGray = Bits(ptrWidth bits)
  val pushToPopGray = Bits(ptrWidth bits)

  def isFull(a: Bits, b: Bits) = a(ptrWidth - 1 downto ptrWidth - 2) === ~b(ptrWidth - 1 downto ptrWidth - 2) && a(ptrWidth - 3 downto 0) === b(ptrWidth - 3 downto 0)
  def isEmpty(a: Bits, b: Bits) = a === b

  val push = pushCd on new Area{
    val currentPtr, oldPtr = Reg(UInt(ptrWidth bits)) init(0)
    val currentPtrPlusOne = currentPtr + 1
    val popPtrGray  = BufferCC(popToPushGray, init = B(0, ptrWidth bits))
    pushToPopGray := RegNext(toGray(oldPtr)) init(0)

    val ratio = popWidth/pushWidth
    val buffer = Reg(Bits(popWidth-pushWidth bits))
    val state = Reg(UInt(log2Up(ratio) bits)) init(0)
    val length = Reg(UInt(log2Up(lengthMax*8 + 1) bits)) init(0)


    val port = ram.writePort
    port.valid := False
    port.payload.assignDontCare()

    val error = RegInit(False)
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


    when(doWrite){
      when(isFull(toGray(currentPtrPlusOne), popPtrGray)){
        error := True
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
      when(error) {
        currentPtr := oldPtr
      } otherwise {
        oldPtr := currentPtrPlusOne
        currentPtr := currentPtrPlusOne
        port.valid := True
        port.address := oldPtr.resized
        port.data := B(length).resized
      }
      error := False
      state := 0
      length := 0
    }
  }

  val pop = popCd on new Area{
    val currentPtr = Reg(UInt(ptrWidth bits)) init(0)
    val pushPtrGray  = BufferCC(pushToPopGray, init = B(0, ptrWidth bits))
    val popPtrGray = toGray(currentPtr)
    popToPushGray := RegNext(popPtrGray) init(0)

    val cmd = Stream(ram.addressType())
    cmd.valid := !isEmpty(popPtrGray, pushPtrGray)
    cmd.payload := currentPtr.resized

    io.pop.stream << ram.streamReadSync(cmd)

    when(cmd.fire){
      currentPtr := currentPtr + 1
    }
  }
}


