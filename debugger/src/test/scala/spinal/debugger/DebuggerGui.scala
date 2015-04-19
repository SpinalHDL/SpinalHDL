package spinal.debugger


import purejavacomm.{CommPortIdentifier, SerialPort, SerialPortEvent, SerialPortEventListener}

import scala.collection.mutable.ArrayBuffer
import scalafx.Includes._
import scalafx.application.{JFXApp, Platform}
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{MenuItem, _}
import scalafx.scene.layout._
import scalafx.stage.Stage

object DebuggerGui extends JFXApp {

  var busTree: ObservableBuffer[TreeItem[String]] = null
  var busTreeRoot: TreeItem[String] = null

  stage = new JFXApp.PrimaryStage {
    title = "Menu Example"
    width = 400
    scene = new Scene {
      root = new VBox {
        //alignment = Pos.TopLeft
        children = Seq(
        {
          new MenuBar {
            useSystemMenuBar = true
            menus = List(
              new Menu("Menu") {
                items = List(
                  new MenuItem("Exit") {
                    onAction = handle {
                      System.exit(0)
                    }
                  }
                )
              },
              new Menu("Add") {
                items = List(new MenuItem("Serial port") {
                  onAction = handle {
                    serialPortGui
                  }
                })
              })
          }
        }, {
          new StackPane {
            padding = Insets(0)
            children = new TreeView[String] {
              minWidth = 200
              minHeight = 200
              showRoot = false
              busTreeRoot = new TreeItem[String]("Root Node") {
                expanded = true
                busTree = ObservableBuffer(

                )
              }
              root = busTreeRoot
            }
          }
        })
      }

    }
  }


  def serialPortGui: Unit = {
    Platform.runLater {
      var dialogStage: Stage = null
      dialogStage = new Stage {
        title = "CheckBox Test"
        scene = new Scene {
          content = new GridPane {
            padding = Insets(10)
            hgap = 5
            vgap = 5

            val baudrateGui = new ComboBox[String] {
              editable = true
              items = ObservableBuffer(1200, 2400, 4800, 9600, 19200, 38400, 57600, 115200, 230400, 460800, 921600).map(_.toString)
              value = "115200"
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
                busTree += new TreeItem[String] {
                  val p = CommPortIdentifier.getPortIdentifier("COM11").open("spinalDebugger", 0).asInstanceOf[SerialPort];
                  p.setSerialPortParams(57600, SerialPort.DATABITS_8, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE);

                  val out = p.getOutputStream
                  val in = p.getInputStream

                  val cMagic = 0x74
                  val cLast = 0x53

                  value = "Serial port " + portNameGui.getValue
                  dialogStage.close()


                  var periphTree  = ObservableBuffer[TreeItem[String]]()
                  implicit def b(x: Int) = x.toByte
                  p.notifyOnDataAvailable(true)
                  p.addEventListener(new SerialPortEventListener {
                    val bytesBuffer = ArrayBuffer[Byte]()
                    var inMagic = false
                    override def serialEvent(serialPortEvent: SerialPortEvent): Unit = {
                      while (in.available() != 0) {
                        val byte = in.read().toByte
                        if (inMagic) {
                          if (byte == cMagic) {
                            bytesBuffer += byte
                          } else {
                            rx
                          }
                          inMagic = false
                        } else {
                          if (byte == cMagic) {
                            inMagic = true
                          } else {
                            bytesBuffer += byte
                          }
                        }
                        print(BigInt(byte).toString(16) + " ")
                      }
                    }

                    def rx: Unit ={
                      periphTree +=  new TreeItem[String] {
                        value = bytesBuffer.mkString(" ")
                      }
                      children = periphTree
                      bytesBuffer.clear()
                    }
                  })


                  //INIT
                  {
                    flush
                    passportCall
                  }

                  def passportCall: Unit = {
                    sendPacket(Seq(0xFF, 0xFF, 0xFF, 0xFF))
                  }

                  def sendPacket(bytes: Seq[Byte]): Unit = {
                    for (byte <- bytes) {
                      if (byte == cMagic) out.write(cMagic)
                      out.write(byte)
                    }
                    flush
                  }
                  def flush: Unit = {
                    out.write(cMagic)
                    out.write(cLast)
                  }
                }
                busTreeRoot.children = busTree
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

  //  stage = new JFXApp.PrimaryStage {
  //    title = "Button Example"
  //    scene = new Scene {
  //      root = new TilePane {
  //        padding = Insets(10)
  //        orientation = Orientation.VERTICAL
  //        hgap = 10
  //        vgap = 10
  //        children = List(
  //          new Button {
  //            text = "Button 1"
  //            maxWidth = Double.MaxValue
  //            onAction = handle  {
  //              println("asd")
  //              text = "ad"
  //            }
  //
  //          },
  //          new Button {
  //            text = "Default Button - Enter Key"
  //            defaultButton = true
  //            maxWidth = Double.MaxValue
  //          },
  //          new Button {
  //            text = "Cancel Button - Esc Key"
  //            cancelButton = true
  //            maxWidth = Double.MaxValue
  //          },
  //          new Button {
  //            text = "Disabled Button"
  //            disable = true
  //            maxWidth = Double.MaxValue
  //          })
  //      }
  //    }
  //  }
}


//      grid.add(new VBox {
//        vgrow = Priority.Always
//        hgrow = Priority.Always
//        spacing = 10
//        padding = Insets(20)
//
//        //Radio Button Toggle Group
//        val tog = new ToggleGroup()
//
//        children = List(
//          new RadioButton {
//            maxWidth = 200
//            maxHeight = 50
//            text = "None"
//            toggleGroup = tog
//          },
//          new RadioButton {
//            maxWidth = 200
//            maxHeight = 50
//            text = "Odd"
//            selected = true
//            toggleGroup = tog
//          },
//          new RadioButton {
//            maxWidth = 200
//            maxHeight = 50
//            text = "Even"
//          })
//      },0,1)

//      grid.add(new VBox {
//        vgrow = Priority.Always
//        hgrow = Priority.Always
//        spacing = 10
//        padding = Insets(20)
//
//        //Radio Button Toggle Group
//        val tog = new ToggleGroup()
//
//        children = List(
//          new RadioButton {
//            maxWidth = 200
//            maxHeight = 50
//            text = "None"
//            toggleGroup = tog
//          },
//          new RadioButton {
//            maxWidth = 200
//            maxHeight = 50
//            text = "Odd"
//            selected = true
//            toggleGroup = tog
//          },
//          new RadioButton {
//            maxWidth = 200
//            maxHeight = 50
//            text = "Even"
//          })
//      },1,1)
//      grid.add(check, 0, 0)
//      grid.add(lblCheckState, 1, 0)
//      grid.add(btnAllowIndeterminate, 0, 1)
//      grid.add(lblAllowIndeterminate, 1, 1)
//      grid.add(btnFire, 0, 2)
//      grid.add(txfText, 1, 2)