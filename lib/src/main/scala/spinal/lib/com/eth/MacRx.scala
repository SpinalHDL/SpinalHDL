package spinal.lib.com.eth

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory


case class RxPayload(dataWidth : Int) extends Bundle {
  val error = Bool()
  val data = Bits(dataWidth bits)
}

//case class MacRxPreamble(dataWidth : Int) extends Component{
//  val io = new Bundle {
//    val phy = slave(Flow(RxPayload(dataWidth)))
//    val data = master(Flow(RxPayload(dataWidth)))
//  }
//
//  val startDelimiter = 0xAB
//  val startDelimiterWidth = 8
//  val history = History(io.phy, 0 to startDelimiterWidth/dataWidth)
//  val historyDataCat = B(Cat(history.map(_.data).reverse)).asBools.reverse.asBits()
//  val alignement = Reg(UInt(log2Up(dataWidth) bits))
//
//  io.data.valid := False
//  io.data.data  := (historyDataCat >> alignement).resize(4 bits).asBools.reverse.asBits
//  io.data.error := io.phy.error
//
//  val inFrame = RegInit(False)
//  when(!inFrame){
//    when(history.map(_.valid).andR){
//      for(shift <- 0 until dataWidth){
//        when(historyDataCat(shift, startDelimiterWidth bits) === startDelimiter){
//          alignement := shift
//          inFrame := True
//        }
//      }
//    }
//  } otherwise {
//    when(history(0).valid) {
//      io.data.valid := True
//    } otherwise {
//      inFrame := False
//    }
//  }
//}

case class MacRxPreamble(dataWidth : Int) extends Component{
  val io = new Bundle {
    val phy = slave(Flow(RxPayload(dataWidth)))
    val data = master(Flow(RxPayload(dataWidth)))
  }

  val startDelimiter = 0xD5
  val startDelimiterWidth = 8
  val history = History(io.phy, 0 until startDelimiterWidth/dataWidth)
  val historyDataCat = B(Cat(history.map(_.data).reverse))
  val hit = history.map(_.valid).andR && historyDataCat === startDelimiter

  io.data.valid := False
  io.data.data  := io.phy.data
  io.data.error := io.phy.error

  val inFrame = RegInit(False)
  when(!inFrame){
    when(hit){
      inFrame := True
    }
  } otherwise {
    when(history(0).valid) {
      io.data.valid := True
    } otherwise {
      inFrame := False
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
  io.result := (if(kind.inputReflected) stateXor.asBools.reverse.asBits() else stateXor)
}

case class MacRxChecker(dataWidth : Int) extends Component {
  val io = new Bundle {
    val input = slave(Flow(RxPayload(dataWidth)))
    val discard = out Bool()
    val validate = out Bool()
    val clear = in Bool()
  }

  val crc = Crc(CrcKind.Crc32, dataWidth)
  crc.io.input.valid := io.input.valid
  crc.io.input.payload := io.input.data
  crc.io.flush := False

  val crcHit = crc.io.result === 0x2144DF1C
  io.discard := False
  io.validate := False

  when(io.input.fire.fall(False)){
    crc.io.flush := True
    when(crcHit){
      io.validate := True
    } otherwise {
      io.discard := True
    }
  }
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
      val stream = slave(Flow(Bits(pushWidth bits)))
      val discard = in Bool()
      val validate = in Bool()
      val clear = in Bool()
    }

    val pop = new Bundle{
      val stream = master(Stream(Bits(popWidth bits)))
      val clear = in Bool()
    }
  }

  val ram = Mem(Bits(popWidth bits), byteSize*8/popWidth)
  val ptrWidth = ram.addressWidth + 1

  val popToPushGray = Bits(ptrWidth bits)
  val pushToPopGray = Bits(ptrWidth bits)

  def isFull(a: Bits, b: Bits) = a(ptrWidth - 1 downto ptrWidth - 2) === ~b(ptrWidth - 1 downto ptrWidth - 2) && a(ptrWidth - 3 downto 0) === b(ptrWidth - 3 downto 0)
  def isEmpty(a: Bits, b: Bits) = a === b

  val push = pushCd on new Area{
    val currentPtr, oldPtr = Reg(UInt(ptrWidth bits))
    val currentPtrPlusOne = currentPtr + 1
    val popPtrGray  = BufferCC(popToPushGray)
    pushToPopGray := RegNext(toGray(oldPtr))

    val ratio = popWidth/pushWidth
    val buffer = Reg(Bits(popWidth-pushWidth bits))
    val state = Reg(UInt(log2Up(ratio) bits))
    val length = Reg(UInt(log2Up(lengthMax*8 + 1) bits))


    val port = ram.writePort
    port.valid := False
    port.payload.assignDontCare()

    val overflow = RegInit(False)

    val doWrite = False
    when(io.push.stream.valid){
      state := state + 1
      length := length + pushWidth
      for(i <- 0 to ratio-2) when(state === i){
        buffer(i*pushWidth, pushWidth bits) := io.push.stream.payload
      }
      when(state === ratio-1){
        doWrite := True
      }
    } otherwise {
      when(state =/= 0){
        doWrite := True
        state := 0
      }
    }

    when(doWrite){
      when(isFull(toGray(currentPtrPlusOne), popPtrGray)){
        overflow := True
      } otherwise {
        port.valid := True
        port.address := currentPtrPlusOne.resized
        port.data := io.push.stream.payload ## buffer
        currentPtr := currentPtrPlusOne
      }
    }

    val cleanup = False

    when(cleanup){
      overflow := False
      state := 0
      length := 0
    }

    when(io.push.discard || io.push.validate && overflow){
      currentPtr := oldPtr
      cleanup := True
    }
    when(io.push.validate && !overflow){
      oldPtr := currentPtrPlusOne
      currentPtr := currentPtrPlusOne
      cleanup := True
      port.valid := True
      port.address := oldPtr.resized
      port.data := B(length).resized
    }

    when(io.push.clear){
      cleanup := True
      oldPtr := 0
      currentPtr := 0
    }
  }

  val pop = popCd on new Area{
    val currentPtr = Reg(UInt(ptrWidth bits))
    val pushPtrGray  = BufferCC(pushToPopGray)
    val popPtrGray = toGray(currentPtr)
    popToPushGray := RegNext(popPtrGray)

    val cmd = Stream(ram.addressType())
    cmd.valid := !isEmpty(popPtrGray, pushPtrGray)
    cmd.payload := currentPtr.resized

    io.pop.stream << ram.streamReadSync(cmd)

    when(cmd.fire){
      currentPtr := currentPtr + 1
    }
    when(io.pop.clear){
      currentPtr := 0
    }
  }
}


