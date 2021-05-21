package spinal.lib.com.usb


import spinal.core._
import spinal.lib._

class UsbTimer(counterTimeMax : Double, fsRatio : Int) extends  Area {
  val lowSpeed = Bool()
  val counter = Reg(UInt((log2Up(12e6 * fsRatio * counterTimeMax toInt) + 2) bits))
  val clear = False
  counter := counter + 1
  when(clear) {
    counter := 0
  }

  def trigger(t: Double): Bool = {
    val at = (12e6 * fsRatio * t toInt) - 1
    counter === at
  }

  def cycles(c: Int): Bool = {
    val ls = c * fsRatio*8 - 1
    val fs = c * fsRatio - 1
    counter === (lowSpeed ? U(ls) | U(fs))
  }
}