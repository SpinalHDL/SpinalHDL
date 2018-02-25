package spinal.lib.misc
import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory


case class Timer(width : Int) extends Component{
  val io = new Bundle {
    val tick  = in Bool
    val clear = in Bool
    val limit = in UInt (width bits)

    val full  = out Bool
    val value = out UInt (width bits)
  }
  val counter = Reg(UInt(width bits))
  val limitHit = counter === io.limit
  val inhibitFull = RegInit(False)
  when(io.tick){
    inhibitFull := limitHit
    counter := counter + (!limitHit).asUInt
  }
  when(io.clear){
    counter := 0
    inhibitFull := False
  }
  io.full  := limitHit && io.tick && !inhibitFull
  io.value := counter


  def driveFrom(busCtrl : BusSlaveFactory,baseAddress : BigInt)
               (ticks : Seq[Bool],clears : Seq[Bool]) = new Area {
    //Address 0 => clear/tick masks + bus
    val ticksEnable  = busCtrl.createReadAndWrite(Bits(ticks.length bits) ,baseAddress + 0,0) init(0)
    val clearsEnable = busCtrl.createReadAndWrite(Bits(clears.length bits),baseAddress + 0,16) init(0)
    val busClearing  = False

    //Address 4 => read/write limit (+ auto clear)
    busCtrl.driveAndRead(io.limit,baseAddress + 4)
    busClearing.setWhen(busCtrl.isWriting(baseAddress + 4))

    //Address 8 => read timer value / write => clear timer value
    busCtrl.read(io.value,baseAddress + 8)
    busClearing.setWhen(busCtrl.isWriting(baseAddress + 8))

    io.clear := (clearsEnable & clears.asBits).orR | busClearing
    io.tick  := (ticksEnable  & ticks.asBits ).orR
  }
}