package spinal.demo.mandelbrot

import spinal.debugger.gui.BytePacketHal
import spinal.gui.SerialPortDialog


import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.DoubleProperty.sfxDoubleProperty2jfx
import scalafx.event.EventHandler
import scalafx.scene.Group.sfxGroup2jfx
import scalafx.scene.canvas.Canvas
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Stop.sfxStop2jfx
import scalafx.scene.paint.Color
import scalafx.scene.paint.CycleMethod
import scalafx.scene.paint.LinearGradient
import scalafx.scene.paint.Stop
import scalafx.scene.shape.Rectangle
import scalafx.scene.Group
import scalafx.scene.Scene
import scalafx.stage.Stage
/**
 * Created by PIC on 04.06.2015.
 */
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.Includes._
import scalafx.scene.canvas.{GraphicsContext, Canvas}
import scalafx.scene.layout.Pane
import scalafx.scene.input._
import scalafx.geometry.Rectangle2D
import scalafx.scene.transform.Affine
import scalafx.scene.effect.BlendMode
object MandelbrotGui extends JFXApp {
  val canvas = new Canvas(200, 200)

  // Draw background with gradient
  val rect = new Rectangle {
    height = 400
    width = 400
    fill = new LinearGradient(0, 0, 1, 1, true, CycleMethod.REFLECT, List(Stop(0, Color.RED), Stop(1, Color.YELLOW)))
  }

  val rootPane = new Group
  rootPane.children = List(canvas)

  stage = new PrimaryStage {
    title = "Canvas Doodle Test"
    scene = new Scene(400, 400) {
      root = rootPane
    }
  }

  canvas.translateX = 100
  canvas.translateY = 100

  val gc = canvas.graphicsContext2D

  reset(Color.BLUE)

//  // Clear away portions as the user drags the mouse
  canvas.onMouseDragged = (e: MouseEvent) => {
    gc.clearRect(e.x - 2, e.y - 2, 5, 5)
  }

  // Fill the Canvas with a Blue rectnagle when the user double-clicks
  canvas.onMouseClicked = (e: MouseEvent) => {
    if (e.clickCount > 1) {
      reset(Color.BLUE);
    }
  }

  private def reset(color: Color) {
    gc.fill = color
    gc.fillRect(0, 0, canvas.width.get, canvas.height.get);
  }


  new SerialPortDialog((streamHal => {
    stage.show()
    val packetHal = new BytePacketHal(streamHal)
    packetHal.open



  }))

}
