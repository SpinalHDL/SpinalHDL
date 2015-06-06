package spinal.demo.mandelbrot

import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST.JValue
import spinal.debugger.gui._
import spinal.lib.BitAggregator

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.canvas.Canvas
import scalafx.scene.effect.BlendMode
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.{Group, Scene}
import scalafx.stage.Stage


class MandelbrotManager(address: Seq[Byte], hal: IBytePacketHal, r: JValue) extends PeripheralManager(address, hal) {

  implicit val formats = DefaultFormats // Brings in default date formats etc.

  val p = r.\("p").extract[MandelbrotCoreParameters]

  sendCoord(-1.0, -1.0, 1.0, 1.0)

  openGui
  //  sendCoord(-1.0,-1.0,0.0,1.0)
  //  sendCoord(-1.0,-1.0,1.0,1.0)

  def sendCoord(xStart: Double, yStart: Double, xEnd: Double, yEnd: Double) {
    implicit def easyByte(i: Int) = i.toByte
    val aggregator = new BitAggregator
    aggregator.add(0x01, 8)
    aggregator.add(BigDecimal(xStart * Math.pow(2, p.fixWidth - p.fixExp)).toBigInt(), p.fixWidth)
    aggregator.add(BigDecimal(yStart * Math.pow(2, p.fixWidth - p.fixExp)).toBigInt(), p.fixWidth)
    aggregator.add(BigDecimal((xEnd - xStart) / p.screenResX * Math.pow(2, p.fixWidth + 8 - (p.fixExp - 4))).toBigInt(), p.fixWidth + 8)
    aggregator.add(BigDecimal((yEnd - yStart) / p.screenResY * Math.pow(2, p.fixWidth + 8 - (p.fixExp - 4))).toBigInt(), p.fixWidth + 8)
    tx(aggregator.toBytes)
  }

  override def rx(packet: Seq[Byte]): Unit = {

  }

  var xStart = -1.0
  var yStart = -1.0
  var xEnd = 1.0
  var yEnd = 1.0

  def openGui: Unit = {
    Platform.runLater {
      val resX = 200
      val resY = 200
      val canvasBack = new Canvas(resX, resY)
      val canvasFront = new Canvas(resX, resY)

      val rootPane = new Group
      rootPane.children = List(canvasBack,canvasFront)
      canvasFront.toFront()
      val backGc = canvasBack.graphicsContext2D
      val frontGc = canvasFront.graphicsContext2D

      def drawMandelbrot: Unit = {
        val w = backGc.getPixelWriter
        val incX = (xEnd - xStart) / resX
        val incY = (yEnd - yStart) / resY
        for (y <- 0 until resY) {
          for (x <- 0 until resX) {
            val cx = xStart + incX * x
            val cy = yStart + incY * y
            def getIterCount: Int = {
              var mx = 0.0
              var my = 0.0
              val iterMax = 255
              for (i <- 0 until iterMax) {
                val temp = mx * mx - my * my + cx
                my = 2.0 * mx * my + cy
                mx = temp
                if (mx * mx + my * my > 4) return i
              }
              return iterMax
            }
            w.setArgb(x, y, getIterCount + (0xFF000000))
          }
        }
      }
      drawMandelbrot

      var dragStartX = 0.0
      var dragStartY = 0.0
      canvasFront.onMousePressed = (e: MouseEvent) => {
        dragStartX = e.x
        dragStartY = e.y
      }

      canvasFront.onMouseDragged  = (e: MouseEvent) => {
        frontGc.clearRect(0,0,resX,resY)
        frontGc.setStroke(Color.White);
        frontGc.strokeRect(dragStartX, dragStartY,-dragStartX +  e.x,-dragStartY + e.y)
      }

      canvasFront.onMouseReleased = (e: MouseEvent) => {
        frontGc.clearRect(0,0,resX,resY)
        val newXStart = xStart + (xEnd - xStart) * (dragStartX / resX)
        val newYStart = yStart + (yEnd - yStart) * (dragStartY / resY)

        xEnd = xStart + (xEnd - xStart) * (e.x / resX)
        yEnd = yStart + (yEnd - yStart) * (e.y / resY)

        xStart = newXStart
        yStart = newYStart

        drawMandelbrot
        sendCoord(xStart, yStart, xEnd, yEnd)
      }

      val stage = new Stage {
        title = "Mandelbrot"
        scene = new Scene(resX, resY) {
          root = rootPane
        }
        show()
      }
    }
  }
}

object MandelbrotGuiMain {
  def main(args: Array[String]) {
    PeripheralManagerFactoryRegistry.peripheralManagerFactories += new IPeripheralManagerFactory {
      override def newPeripheral(address: Seq[Byte], hal: IBytePacketHal, r: JValue): Unit = new MandelbrotManager(address, hal, r)
      override def getPassportKind(): String = "mandelbrotCore"
    }

    DebuggerGui.main(args)
  }
}

