package spinal.demo.mandelbrot

import net.liftweb.json.JsonAST.JValue
import spinal.core._

import scala.util.Random

//This class is used everywhere into the mandelbrot core as "generic" "construction" parameter
case class MandelbrotCoreParameters(iterationLimit: Int,
                                    pixelTaskSolverCount: Int,
                                    screenResX: Int,
                                    screenResY: Int,
                                    fixExp: Int,
                                    fixWidth: Int) {
  def fix = SFix(fixExp exp, fixWidth bit)
  def iterationWidth = log2Up(iterationLimit + 1)

  val uid : Int = Random.nextInt()
}


case class FrameTask(p: MandelbrotCoreParameters) extends Bundle {
  val start = SFix2D(p.fixExp exp, p.fixWidth bit)
  val inc = SFix2D(p.fixExp - 4 exp, p.fixWidth + 8 bit)

  def fullRangeSFix = SFix(p.fixExp exp, p.fixWidth + 12 bit)
}

case class PixelTask(p: MandelbrotCoreParameters) extends Bundle {
  val mandelbrotPosition = SFix2D(p.fix)
}
case class PixelResult(p: MandelbrotCoreParameters) extends Bundle {
  val iteration = UInt(p.iterationWidth bit)
}