//package spinal.demo.mandelbrot
//
//import net.liftweb.json.DefaultFormats
//import net.liftweb.json.JsonAST.JValue
//import spinal.debugger.gui._
//import spinal.lib.BitAggregator
//
//import scalafx.Includes._
//import scalafx.application.Platform
//import scalafx.event.ActionEvent
//import scalafx.scene.canvas.Canvas
//import scalafx.scene.control.RadioButton
//import scalafx.scene.input.{MouseButton, MouseEvent}
//import scalafx.scene.layout.VBox
//import scalafx.scene.{Group, Scene}
//import scalafx.stage.Stage
//
//
//class MandelbrotManager(address: Seq[Byte], hal: IBytePacketHal, r: JValue) extends PeripheralManager(address, hal) {
//
//  implicit val formats = DefaultFormats // Brings in default date formats etc.
//
//  val p = r.\("p").extract[MandelbrotCoreParameters]
//
//  sendConfig
//  sendCoord(-1.0, -1.0, 1.0, 1.0)
//
//  openGui
//  //  sendCoord(-1.0,-1.0,0.0,1.0)
//  //  sendCoord(-1.0,-1.0,1.0,1.0)
//  implicit def easyByte(i: Int) = i.toByte
//  def sendCoord(xStart: Double, yStart: Double, xEnd: Double, yEnd: Double) {
//    val aggregator = new BitAggregator
//    aggregator.add(0x01, 8)
//    aggregator.add(BigDecimal(xStart * Math.pow(2, p.fixWidth - p.fixExp - 1)).toBigInt, p.fixWidth)
//    aggregator.add(BigDecimal(yStart * Math.pow(2, p.fixWidth - p.fixExp - 1)).toBigInt, p.fixWidth)
//    aggregator.add(BigDecimal((xEnd - xStart) / p.screenResX * Math.pow(2, p.fixWidth + 8 - 1 - (p.fixExp - 4))).toBigInt, p.fixWidth + 8)
//    aggregator.add(BigDecimal((yEnd - yStart) / p.screenResY * Math.pow(2, p.fixWidth + 8 - 1 - (p.fixExp - 4))).toBigInt, p.fixWidth + 8)
//    tx(aggregator.toBytes)
//  }
//
//  def sendConfig: Unit = {
//    val aggregator = new BitAggregator
//    aggregator.add(0x02, 8)
//    aggregator.add(frameTaskFilterEnable)
//    tx(aggregator.toBytes)
//  }
//
//  override def rx(packet: Seq[Byte]): Unit = {
//
//  }
//
//  var xStart = -1.0
//  var yStart = -1.0
//  var xEnd = 1.0
//  var yEnd = 1.0
//  var frameTaskFilterEnable = true
//
//
//  def openGui: Unit = {
//    Platform.runLater {
//
//      val canvasBack = new Canvas(p.screenResX/2,p.screenResY/2)
//      val canvasFront = new Canvas(p.screenResX/2,p.screenResY/2)
//      def resX = canvasFront.getWidth.toInt
//      def resY = canvasFront.getHeight.toInt
//      val frameTaskFilterEnableBt = new RadioButton() {
//        maxWidth = 200
//        maxHeight = 50
//        text = "Frame task filter"
//
//        onAction  = {
//          e: ActionEvent => {
//            frameTaskFilterEnable = delegate.isSelected
//            sendConfig
//          }
//        }
//      }
//
//      val rootPane = new Group
//      rootPane.children = List(canvasBack, canvasFront)
//      canvasFront.toFront()
//      val backGc = canvasBack.graphicsContext2D
//      val frontGc = canvasFront.graphicsContext2D
//
//      def drawMandelbrot: Unit = {
//        val w = backGc.getPixelWriter
//        val incX = (xEnd - xStart) / resX
//        val incY = (yEnd - yStart) / resY
//        for (y <- 0 until resY) {
//          for (x <- 0 until resX) {
//            val cx = xStart + incX * x
//            val cy = yStart + incY * y
//            def getIterCount: Int = {
//              var mx = 0.0
//              var my = 0.0
//              val iterMax = 255
//              for (i <- 0 until iterMax) {
//                val temp = mx * mx - my * my + cx
//                my = 2.0 * mx * my + cy
//                mx = temp
//                if (mx * mx + my * my > 4) return i
//              }
//              return iterMax
//            }
//            w.setArgb(x, y, getIterCount + (0xFF000000))
//          }
//        }
//      }
//      drawMandelbrot
//
//      var dragStartX = 0.0
//      var dragStartY = 0.0
//      canvasFront.onMousePressed = (e: MouseEvent) => {
//        //        if (e.button == MouseButton.PRIMARY) {
//        //          dragStartX = e.x
//        //          dragStartY = e.y
//        //        }
//        if (e.button == MouseButton.PRIMARY) {
//          val newXStart = xStart + (xEnd - xStart) * (e.x / resX) * 0.5
//          val newYStart = yStart + (yEnd - yStart) * (e.y / resY) * 0.5
//
//          xEnd = xEnd - (xEnd - xStart) * (1 - (e.x / resX)) * 0.5
//          yEnd = yEnd - (yEnd - yStart) * (1 - (e.y / resY)) * 0.5
//
//          xStart = newXStart
//          yStart = newYStart
//
//          drawMandelbrot
//          sendCoord(xStart, yStart, xEnd, yEnd)
//        }
//        if (e.button == MouseButton.SECONDARY) {
//          val newXStart = xStart - (xEnd - xStart) * (e.x / resX)
//          val newYStart = yStart - (yEnd - yStart) * (e.y / resY)
//
//          xEnd = xEnd + (xEnd - xStart) * (1 - (e.x / resX))
//          yEnd = yEnd + (yEnd - yStart) * (1 - (e.y / resY))
//
//          xStart = newXStart
//          yStart = newYStart
//
//          drawMandelbrot
//          sendCoord(xStart, yStart, xEnd, yEnd)
//        }
//      }
//
//      //      canvasFront.onMouseDragged = (e: MouseEvent) => {
//      //        if (e.button == MouseButton.PRIMARY) {
//      //          frontGc.clearRect(0, 0, resX, resY)
//      //          frontGc.setStroke(Color.White);
//      //          frontGc.strokeRect(dragStartX, dragStartY, -dragStartX + e.x, -dragStartY + e.y)
//      //        }
//      //      }
//      //
//      //      canvasFront.onMouseReleased = (e: MouseEvent) => {
//      //        if (e.button == MouseButton.PRIMARY) {
//      //          frontGc.clearRect(0, 0, resX, resY)
//      //          val newXStart = xStart + (xEnd - xStart) * (dragStartX / resX)
//      //          val newYStart = yStart + (yEnd - yStart) * (dragStartY / resY)
//      //
//      //          xEnd = xStart + (xEnd - xStart) * (e.x / resX)
//      //          yEnd = yStart + (yEnd - yStart) * (e.y / resY)
//      //
//      //          xStart = newXStart
//      //          yStart = newYStart
//      //
//      //          drawMandelbrot
//      //          sendCoord(xStart, yStart, xEnd, yEnd)
//      //        }
//      //      }
//
//
//      val stage = new Stage {
//        title = "Mandelbrot"
//        scene = new Scene {
//          val vBox = new VBox() {
//            children = Seq(
//              rootPane,
//              frameTaskFilterEnableBt)
//          }
//          root = vBox
//        }
//        show()
//      }
//    }
//  }
//}
//
//object MandelbrotGuiMain {
//  def main(args: Array[String]) {
//    PeripheralManagerFactoryRegistry.peripheralManagerFactories += new IPeripheralManagerFactory {
//      override def newPeripheral(address: Seq[Byte], hal: IBytePacketHal, r: JValue): Unit = new MandelbrotManager(address, hal, r)
//      override def getPassportKind(): String = "mandelbrotCore"
//    }
//
//    DebuggerGui.main(args)
//  }
//}
//
