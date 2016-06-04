//package spinal.debugger.gui
//
//import java.io.{File, FileFilter}
//import java.nio.charset.Charset
//import java.nio.file.Files
//import javafx.stage.DirectoryChooser
//
//import net.liftweb.json.JsonAST.JValue
//import purejavacomm.{CommPortIdentifier, SerialPort}
//import spinal.lib.BitAggregator
//
//import scala.collection.mutable.ArrayBuffer
//import scalafx.Includes._
//import scalafx.application.{JFXApp, Platform}
//import scalafx.collections.ObservableBuffer
//import scalafx.geometry.Insets
//import scalafx.scene.Scene
//import scalafx.scene.control.{MenuItem, _}
//import scalafx.scene.layout._
//import scalafx.stage.Stage
//
//object PeripheralManagerFactoryRegistry{
//  val peripheralManagerFactories = ArrayBuffer[IPeripheralManagerFactory]()
//  peripheralManagerFactories += new IPeripheralManagerFactory {
//    override def newPeripheral(address: Seq[Byte], hal: IBytePacketHal, json: JValue): Unit = new LogicAnalyserManager(address,hal,json)
//    override def getPassportKind(): String = "logicAnalyser"
//  }
//}
//
//object DebuggerGui extends JFXApp {
//
//
//
//  var busTree: ObservableBuffer[TreeItem[String]] = null
//  var busTreeRoot: TreeItem[String] = null
//  val reports = ArrayBuffer[JValue]()
//
//  def scanFile(file: File): Unit = {
//    try {
//      if (file.isDirectory) {
//        val jsonsFile = file.listFiles(new FileFilter {
//          override def accept(pathname: File): Boolean = pathname.getName.endsWith(".json")
//        })
//        jsonsFile.foreach(scanFile(_))
//      } else {
//        val str = new String(Files.readAllBytes(file.toPath),Charset.defaultCharset())
//        val root = net.liftweb.json.parse(str)
//        val reportsJson = root.\\("reports")
//        reports += reportsJson
//        val i = 0
//      }
//    } catch {
//      case e : Exception =>
//    }
//  }
//
//
//  scanFile(new File(System.getProperty("user.dir")))
//
//  stage = new JFXApp.PrimaryStage {
//    title = "Bus manager"
//    width = 400
//    scene = new Scene {
//      root = new VBox {
//        //alignment = Pos.TopLeft
//        children = Seq(
//        {
//          new MenuBar {
//            useSystemMenuBar = true
//            menus = List(
//              new Menu("Menu") {
//                items = List(
//                  new MenuItem("Add report") {
//                    onAction = handle {
//                      val directoryChooser = new DirectoryChooser()
//                      val selectedDirectory = directoryChooser.showDialog(stage);
//                      if (selectedDirectory != null) {
//                        scanFile(selectedDirectory)
//                      }
//                    }
//                  },
//                  new MenuItem("Exit") {
//                    onAction = handle {
//                      System.exit(0)
//                    }
//                  }
//                )
//              },
//              new Menu("Add") {
//                items = List(new MenuItem("Serial port") {
//                  onAction = handle {
//                    serialPortGui
//                  }
//                })
//              })
//          }
//        }, {
//          new StackPane {
//            padding = Insets(0)
//            children = new TreeView[String] {
//              minWidth = 200
//              minHeight = 200
//              showRoot = false
//              busTreeRoot = new TreeItem[String]("Root Node") {
//                expanded = true
//                busTree = ObservableBuffer(
//
//                )
//              }
//              root = busTreeRoot
//            }
//          }
//        })
//      }
//
//    }
//  }
//
//
//  def serialPortGui: Unit = {
//    Platform.runLater {
//      var dialogStage: Stage = null
//      dialogStage = new Stage {
//
//        title = "Add serial port"
//        scene = new Scene {
//          content = new GridPane {
//            padding = Insets(10)
//            hgap = 5
//            vgap = 5
//
//            val baudrateGui = new ComboBox[String] {
//              editable = true
//              items = ObservableBuffer(1200, 2400, 4800, 9600, 19200, 38400, 57600, 115200, 230400, 460800, 921600).map(_.toString)
//              value = "57600"
//            }
//
//            val portNameGui = new ComboBox[String] {
//              val ports = CommPortIdentifier.getPortIdentifiers
//              val itemsBuffer = ObservableBuffer[String]()
//              items = itemsBuffer
//              while (ports.hasMoreElements) {
//                ports.nextElement() match {
//                  case port: CommPortIdentifier => {
//                    itemsBuffer += port.getName
//                  }
//                  case _ =>
//
//                }
//              }
//              value = items.get().headOption.getOrElse("No port")
//            }
//
//            val openGui = new Button {
//              text = "Open"
//              maxWidth = Double.MaxValue
//              onAction = handle {
//                busTree += new TreeItem[String] {
//                  val periphTree = ObservableBuffer[TreeItem[String]]()
//
//                  val streamHal = new SerialPortByteStreamHal(portNameGui.getValue, baudrateGui.getValue.toInt, SerialPort.STOPBITS_1, SerialPort.PARITY_NONE)
//                  val packetHal = new BytePacketHal(streamHal)
//                  val busManager = new BusManager(packetHal, new IGuiTreeViewManager {
//                    override def add(info: Seq[String]): AnyRef = {
//                      val key = new TreeItem[String] {
//                        value = info.mkString(" ")
//                      }
//                      periphTree += key
//                      children = periphTree
//                      key
//                    }
//
//                    override def remove(key: Object): Unit = {
//                      periphTree -= key.asInstanceOf[TreeItem[String]]
//                      children = periphTree
//                    }
//                  },reports,PeripheralManagerFactoryRegistry.peripheralManagerFactories)
//
//
//                  value = "Serial port " + portNameGui.getValue
//                  dialogStage.close()
//                }
//                busTreeRoot.children = busTree
//              }
//            }
//
//            add(baudrateGui, 0, 0)
//            add(portNameGui, 1, 0)
//            add(openGui, 0, 1, 2, 1)
//          }
//        }
//      }
//
//      dialogStage.show()
//    }
//  }
//
//}
//
//
//trait IGuiTreeViewManager {
//  def add(info: Seq[String]): Object
//  def remove(key: Object)
//}
//
////      grid.add(new VBox {
////        vgrow = Priority.Always
////        hgrow = Priority.Always
////        spacing = 10
////        padding = Insets(20)
////
////        //Radio Button Toggle Group
////        val tog = new ToggleGroup()
////
////        children = List(
////          new RadioButton {
////            maxWidth = 200
////            maxHeight = 50
////            text = "None"
////            toggleGroup = tog
////          },
////          new RadioButton {
////            maxWidth = 200
////            maxHeight = 50
////            text = "Odd"
////            selected = true
////            toggleGroup = tog
////          },
////          new RadioButton {
////            maxWidth = 200
////            maxHeight = 50
////            text = "Even"
////          })
////      },0,1)
//
////      grid.add(new VBox {
////        vgrow = Priority.Always
////        hgrow = Priority.Always
////        spacing = 10
////        padding = Insets(20)
////
////        //Radio Button Toggle Group
////        val tog = new ToggleGroup()
////
////        children = List(
////          new RadioButton {
////            maxWidth = 200
////            maxHeight = 50
////            text = "None"
////            toggleGroup = tog
////          },
////          new RadioButton {
////            maxWidth = 200
////            maxHeight = 50
////            text = "Odd"
////            selected = true
////            toggleGroup = tog
////          },
////          new RadioButton {
////            maxWidth = 200
////            maxHeight = 50
////            text = "Even"
////          })
////      },1,1)
////      grid.add(check, 0, 0)
////      grid.add(lblCheckState, 1, 0)
////      grid.add(btnAllowIndeterminate, 0, 1)
////      grid.add(lblAllowIndeterminate, 1, 1)
////      grid.add(btnFire, 0, 2)
////      grid.add(txfText, 1, 2)