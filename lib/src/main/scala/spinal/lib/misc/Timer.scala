package spinal.lib.misc
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory


case class Timer(width : Int) extends Component{
  val io = new Bundle{
    val tick      = in Bool
    val clear     = in Bool
    val limit     = in UInt(width bits)

    val overflow  = out Bool
    val value     = out UInt(width bits)
  }
  
  val counter = Reg(UInt(width bits))
  when(io.tick){
    counter := counter + 1
  }
  when(io.clear){
    counter := 0
  }

  io.overflow := counter === io.limit
  io.value := counter


  def driveFrom(busCtrl : BusSlaveFactory,baseAddress : BigInt,clears : Seq[Bool],ticks : Seq[Bool]) = new Area {
    //Address 0 => clear/tick masks + bus
    val clearsEnable = busCtrl.createReadWrite(Bits(clears.length bits),baseAddress,0) init(0)
    val ticksEnable  = busCtrl.createReadWrite(Bits(ticks.length bits),baseAddress,16) init(0)

    io.clear := (clearsEnable & clears.asBits).orR
    io.tick  := (ticksEnable  & ticks.asBits ).orR

    //Address 4 => read/write limit (+ auto clear)
    busCtrl.driveAndRead(io.limit,4)
    io.clear := busCtrl.isWriting(4)

    //Address 8 => read timer value / write => clear timer value
    busCtrl.read(io.value,8)
    io.clear.setWhen(busCtrl.isWriting(8))
  }
}