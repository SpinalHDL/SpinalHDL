package spinal.gui

import purejavacomm.{CommPortIdentifier, SerialPort}
import spinal.debugger.gui.{IByteStreamHal, SerialPortByteStreamHal}

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ComboBox}
import scalafx.scene.layout.GridPane
import scalafx.stage.Stage

/**
 * Created by PIC on 04.06.2015.
 */
class SerialPortDialog(callBack: (IByteStreamHal) => Unit) {
  Platform.runLater {
    var dialogStage: Stage = null
    dialogStage = new Stage {

      title = "Add serial port"
      scene = new Scene {
        content = new GridPane {
          padding = Insets(10)
          hgap = 5
          vgap = 5

          val baudrateGui = new ComboBox[String] {
            editable = true
            items = ObservableBuffer(1200, 2400, 4800, 9600, 19200, 38400, 57600, 115200, 230400, 460800, 921600).map(_.toString)
            value = "57600"
          }

          val portNameGui = new ComboBox[String] {
            val ports = CommPortIdentifier.getPortIdentifiers
            val itemsBuffer = ObservableBuffer[String]()
            items = itemsBuffer
            while (ports.hasMoreElements) {
              ports.nextElement() match {
                case port: CommPortIdentifier => {
                  itemsBuffer += port.getName
                }
                case _ =>

              }
            }
            value = items.get().headOption.getOrElse("No port")
          }

          val openGui = new Button {
            text = "Open"
            maxWidth = Double.MaxValue
            onAction = handle {
              val streamHal = new SerialPortByteStreamHal(portNameGui.getValue, baudrateGui.getValue.toInt, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE)

              callBack(streamHal)
              dialogStage.close()
            }
          }
          add(baudrateGui, 0, 0)
          add(portNameGui, 1, 0)
          add(openGui, 0, 1, 2, 1)
        }
      }
    }

    dialogStage.show()
  }
}
