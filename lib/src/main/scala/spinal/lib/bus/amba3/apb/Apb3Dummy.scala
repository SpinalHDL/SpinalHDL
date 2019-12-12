package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._

case class Apb3Dummy(config : Apb3Config) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(config))
  }
  io.apb.PREADY := True
  io.apb.PRDATA := 0
  if(config.useSlaveError) io.apb.PSLVERROR := False
}
