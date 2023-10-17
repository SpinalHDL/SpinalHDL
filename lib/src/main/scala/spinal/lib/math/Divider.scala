package spinal.lib.math

import spinal.core._
import spinal.lib._

case class SignedDividerCmd(nWidth : Int, dWidth : Int) extends Bundle{
  val numerator = SInt(nWidth bit)
  val denominator = SInt(dWidth bit)
}
case class SignedDividerRsp(nWidth : Int, dWidth : Int) extends Bundle{
  val quotient = SInt(nWidth bit)
  val remainder = SInt(dWidth bit)
  val error = Bool()
}
class SignedDivider(nWidth : Int, dWidth : Int,storeDenominator : Boolean) extends Component{
  val io = new Bundle{
    val flush = in Bool()
    val cmd = slave Stream(SignedDividerCmd(nWidth,dWidth))
    val rsp = master Stream(SignedDividerRsp(nWidth,dWidth))
  }
  val divider = new UnsignedDivider(nWidth,dWidth,storeDenominator,Bits(2 bit))
  divider.io.flush := io.flush
  divider.io.cmd.arbitrationFrom(io.cmd)
  divider.io.cmd.numerator := io.cmd.numerator.abs
  divider.io.cmd.denominator := io.cmd.denominator.abs
  divider.io.cmd.context(0) := (io.cmd.numerator.msb ^ io.cmd.denominator.msb)
  divider.io.cmd.context(1) := io.cmd.numerator.msb

  io.rsp.arbitrationFrom(divider.io.rsp)
  io.rsp.quotient := divider.io.rsp.quotient.twoComplement(divider.io.rsp.context(0)).resized
  io.rsp.remainder := divider.io.rsp.remainder.twoComplement(divider.io.rsp.context(1)).resized
  io.rsp.error := divider.io.rsp.error
}


case class MixedDividerCmd(nWidth : Int, dWidth : Int) extends Bundle{
  val numerator = Bits(nWidth bit)
  val denominator = Bits(dWidth bit)
  val signed = Bool()
}
case class MixedDividerRsp(nWidth : Int, dWidth : Int) extends Bundle{
  val quotient = Bits(nWidth bit)
  val remainder = Bits(dWidth bit)
  val error = Bool()
}
class MixedDivider(nWidth : Int, dWidth : Int,storeDenominator : Boolean) extends Component{
  val io = new Bundle{
    val flush = in Bool()
    val cmd = slave Stream(MixedDividerCmd(nWidth,dWidth))
    val rsp = master Stream(MixedDividerRsp(nWidth,dWidth))
  }
  val divider = new UnsignedDivider(nWidth,dWidth,storeDenominator,Bits(2 bit))

  divider.io.flush := io.flush
  divider.io.cmd.arbitrationFrom(io.cmd)
  divider.io.cmd.numerator := io.cmd.numerator.asSInt.abs(io.cmd.signed)
  divider.io.cmd.denominator := io.cmd.denominator.asSInt.abs(io.cmd.signed)
  divider.io.cmd.context(0) := io.cmd.signed && (io.cmd.numerator.msb ^ io.cmd.denominator.msb)
  divider.io.cmd.context(1) := io.cmd.signed && io.cmd.numerator.msb

  io.rsp.arbitrationFrom(divider.io.rsp)
  io.rsp.quotient := divider.io.rsp.quotient.twoComplement(divider.io.rsp.context(0)).asBits.resized
  io.rsp.remainder := divider.io.rsp.remainder.twoComplement(divider.io.rsp.context(1)).asBits.resized
  io.rsp.error := divider.io.rsp.error
}



case class UnsignedDividerCmd[T <: Data](nWidth : Int, dWidth : Int,contextType : T) extends Bundle{
  val numerator = UInt(nWidth bit)
  val denominator = UInt(dWidth bit)
  val context = cloneOf(contextType)
}
case class UnsignedDividerRsp[T <: Data](nWidth : Int, dWidth : Int,contextType : T)extends Bundle{
  val quotient = UInt(nWidth bit)
  val remainder = UInt(dWidth bit)
  val error = Bool()
  val context = cloneOf(contextType)
}


class UnsignedDivider[T <: Data](nWidth : Int, dWidth : Int,storeDenominator : Boolean,contextType : T = NoData) extends Component{
  val io = new Bundle{
    val flush = in Bool()
    val cmd = slave Stream(UnsignedDividerCmd(nWidth,dWidth,contextType))
    val rsp = master Stream(UnsignedDividerRsp(nWidth,dWidth,contextType))
  }
  val done = RegInit(True)
  val waitRsp = RegInit(False)
  val counter = Counter(nWidth)
  val numerator = Reg(UInt(nWidth bit))
  val denominator = if(storeDenominator) Reg(UInt(dWidth bit)) else io.cmd.denominator
  val context = if(storeDenominator) Reg(contextType) else io.cmd.context
  val remainder = Reg(UInt(dWidth bit))
  val remainderShifted = (remainder ## numerator.msb).asUInt
  val remainderMinusDenominator = remainderShifted - denominator

  io.cmd.ready := False
  io.rsp.valid := waitRsp
  io.rsp.quotient := numerator
  io.rsp.remainder := remainder
  io.rsp.context := context

  when(io.rsp.ready){
    waitRsp := False
  }
  io.rsp.error := denominator === 0
  when(done){
    when(!waitRsp || io.rsp.ready){ //ready for new command
      counter.clear()
      remainder := 0
      numerator := io.cmd.numerator
      if(storeDenominator){
        denominator := io.cmd.denominator
        context := io.cmd.context
      }

      done := !io.cmd.valid
      if(storeDenominator) {
        io.cmd.ready := True
      }
    }

  }otherwise{
    counter.increment()
    remainder := remainderShifted.resized
    numerator := (numerator ## !remainderMinusDenominator.msb).asUInt.resized
    when(!remainderMinusDenominator.msb){
      remainder := remainderMinusDenominator.resized
    }
    when(counter.willOverflowIfInc){
      done := True
      waitRsp := True
      if(storeDenominator) {
        io.cmd.ready := True
      }
    }
  }

  when(io.flush){
    done := True
    waitRsp := False
  }

}