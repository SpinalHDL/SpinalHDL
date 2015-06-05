package spinal.demo.mandelbrot

import net.liftweb.json.JsonAST.JValue
import spinal.core._

import scala.util.Random


case class MandelbrotJsonReport(p : MandelbrotCoreParameters,uid : String,clazz : String = "uidPeripheral",kind : String = "mandelbrotCore")
case class MandelbrotCoreParameters(iterationLimit: Int,
                                    pixelTaskSolverCount: Int,
                                    screenResX: Int,
                                    screenResY: Int,
                                    fixExp: Int,
                                    fixWidth: Int
                                    ) {
  def fix = SFix(fixExp, fixWidth bit)
  def iterationWidth = log2Up(iterationLimit + 1)

  val uid : Int = Random.nextInt()


//  import net.liftweb.json.JsonDSL._

//  def fromJson(json: JValue): Unit = {
//    name = (json \ "name").extract[String]
//    scope ++= (json \ "scope").children.map(_.extract[String])
//    kind = (json \ "kind").extract[String]
//    width = (json \ "width").extract[Int]
//  }
//
//  def toJson: JValue = {
//    ("clazz" -> "uidPeripheral") ~
//      ("kind" -> "MandelbrotCore") ~
//      ("screenResX" -> screenResX.toString) ~
//      ("screenResY" -> screenResY.toString) ~
//      ("fixExp" -> fixExp.toString) ~
//      ("fixWidth" -> fixWidth.toString)
//  }
}


case class FrameTask(p: MandelbrotCoreParameters) extends Bundle {
  val start = SFix2D(p.fixExp, p.fixWidth bit)
  val inc = SFix2D(p.fixExp - 4, p.fixWidth + 8 bit)

  def fullRangeSFix = SFix(p.fixExp, p.fixWidth + 12 bit)
}

case class PixelTask(p: MandelbrotCoreParameters) extends Bundle {
  val mandelbrotPosition = SFix2D(p.fix)
}
case class PixelResult(p: MandelbrotCoreParameters) extends Bundle {
  val iteration = UInt(p.iterationWidth bit)
}