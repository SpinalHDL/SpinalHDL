package spinal.debugger.gui


import javafx.event.Event
import javafx.scene.input
import javafx.scene.input.KeyCodeCombination

import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST.JValue
import spinal.debugger.{LogicAnalyser, LogicAnalyserParameter}
import spinal.lib.BitAggregator

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.input.KeyEvent
import scalafx.scene.layout.GridPane
import scalafx.stage.Stage

/**
 * Created by PIC on 22.04.2015.
 */


class LogicAnalyserManager(address: Seq[Byte], hal: IBytePacketHal, report: JValue) extends PeripheralManager(address, hal) {
  implicit val formats = DefaultFormats
  val hardwareParameters = report.\("parameters").extract[LogicAnalyserParameter]

  init
  def init: Unit = {
    createGui
  }

  override def rx(packet: Seq[Byte]): Unit = {

  }

  def createGui: Unit = {
    Platform.runLater {
      var dialogStage: Stage = null
      dialogStage = new Stage {

        title = "Logic Analyser"
        scene = new Scene {
          content = new GridPane {
            padding = Insets(10)
            hgap = 5
            vgap = 5


            val openGui = new Button {
              text = "Capture"
              maxWidth = Double.MaxValue
              onAction = handle {
                triggerDelayGui.getEditor.onActionProperty.get().handle(null)
                captureOffset.getEditor.onActionProperty.get().handle(null)

                val aggregator = new BitAggregator

                aggregator.add(BigInt(LogicAnalyser.configsHeader),8)
                aggregator.add(BigInt(triggerDelayGui.getValue),32)
                aggregator.add(BigInt(captureOffset.getValue),hardwareParameters.memAddressWidth)
                println(aggregator)
                tx(aggregator.toBytes)

                aggregator.clear
                aggregator.add(BigInt(LogicAnalyser.waitTriggerHeader),8)
                aggregator.add(BigInt(1),1)
                println(aggregator)
                tx(aggregator.toBytes)
              }
            }


            val triggerDelayGui = new Spinner[Integer](0, 1000000000, 0) {
              prefWidth = 100
              editable = true
            }

            val captureOffset = new Spinner[Integer](-hardwareParameters.memAddressCount/2+10, hardwareParameters.memAddressCount/2-10, 0) {
              prefWidth = 100
              editable = true
            }

            add(triggerDelayGui, 0, 0)
            add(captureOffset, 0, 1)
            add(openGui, 0, 2, 1, 1)
          }
        }
      }

      dialogStage.show()
    }
  }
}


