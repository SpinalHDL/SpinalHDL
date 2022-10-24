package spinal.lib.blackbox.xilinx.s7
import spinal.core._

case class FDRE() extends BlackBox{
  val D, CE, C, R = in Bool()
  val Q = out Bool()

  mapCurrentClockDomain(clock = C)
}
