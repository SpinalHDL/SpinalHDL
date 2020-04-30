package spinal.lib.blackbox

import java.io.File
import java.nio.file.Paths
import scala.sys.process._

/**
  * Created by csliu on 30/04/2020.
  * This Utils is used to generate SpinalHDL BlackBox from Verilog File
  * csliu@ultrasemi.com
  */

object GenBlackbox {
  def doCmd(cmd: String): Unit = {
    println(cmd)
    if (isWindows)
      Process("cmd /C " + cmd) !
    else
      Process(cmd) !
  }

  def genBlackboxFileHead(): String = {
    var ret = ""
    ret += "import spinal.core._\n\n"
    ret += "/**\n"
    ret += " * Created by genBlackbox utils\n"
    ret += " * csliu@ultrasemi.com\n"
    ret += " */\n\n"
    ret
  }

  def genBlackboxConfigClassHead(name: String): String = {
    "case class " + name + "_Config(){\n"
  }

  def genBlackboxConfigClassBody(t: String, name: String, value: String): String = {
    var ret = ""
    if (t == "str") {
      ret = "  var " + name + " = \"" + value + "\"\n"
    } else if (t == "l") {
      ret = "  var " + name + " = B\"" + value.length + "'b" + value + "\"\n"
    }
    ret
  }

  def genBlackboxConfigClassTail(): String = {
    "}\n\n"
  }

  def genBlackboxClassHead(name: String): String = {
    "case class " + name + "(c:" + name + "_Config = " + name + "_Config()) extends BlackBox {\n"
  }

  def genBlackboxClassBody(dir: String, len: String, name: String): String = {
    var ret = "  val " + name + " = "
    if (dir == "INOUT") {
      ret += "inout(Analog(Bits(" + len + " bit)))"
    } else if (dir == "INPUT") {
      ret += "in(Bits(" + len + " bit))"
    } else if (dir == "OUTPUT") {
      ret += "out(Bits(" + len + " bit))"
    }
    ret += "\n"
    ret
  }

  def genBlackboxClassGeneric(t: String, name: String, value: String): String = {
    "  addGeneric(\"" + name + "\", c." + name + ")\n"
  }

  def genBlackboxClassTail(): String = {
    "}\n\n"
  }

  def gen(pformPath: String):String = {
    import scala.io.Source
    import util.control.Breaks._

    var bb_txt = genBlackboxFileHead()
    var bb_name = ""
    var bb_port = List(new Tuple3("", "", ""))
    var bb_para = List(new Tuple3("", "", ""))
    bb_port = List()
    bb_para = List()
    for (line <- Source.fromFile(pformPath).getLines()) {
      //println(line)
      breakable {
        val igoreReg = "^[:#]".r
        if (igoreReg.findFirstIn(line).nonEmpty) {
          //println(line)
          break()
        }

        val moduleReg = "(S_[a-zA-Z0-9]{16}\\s\\.scope module,\\s\")([a-zA-Z0-9_]+)(\"\\s\")([a-zA-Z0-9_]+)(\"\\s\\d+\\s\\d+;)".r
        moduleReg.findAllIn(line).matchData foreach {
          m => {
            //println(m.group(1))
            //println(m.group(2))
            //println(m.group(3))
            //println(m.group(4))
            if (bb_name != "") {
              //genBlackboxConfigClass
              var bb_config_class_txt = genBlackboxConfigClassHead(bb_name)
              bb_para.foreach {
                m => {
                  bb_config_class_txt += genBlackboxConfigClassBody(m._1, m._2, m._3)
                }
              }
              bb_config_class_txt += genBlackboxConfigClassTail()
              //genBlackboxClass
              var bb_class_txt = genBlackboxClassHead(bb_name)
              bb_port.foreach {
                m => {
                  bb_class_txt += genBlackboxClassBody(m._1, m._2, m._3)
                }
              }
              bb_class_txt += "\n"
              bb_para.foreach {
                m => {
                  bb_class_txt += genBlackboxClassGeneric(m._1, m._2, m._3)
                }
              }
              bb_class_txt += genBlackboxClassTail()

              bb_txt += bb_config_class_txt
              bb_txt += bb_class_txt
              //println(bb_config_class_txt)
              //println(bb_class_txt)
              //println(bb_name)
              //println(bb_port)
              //println(bb_para)

              //start new module
              bb_port = List()
              bb_para = List()
            }
            bb_name = m.group(2)
            break()
          }
        }

        val portReg = "(\\s{4}\\.port_info\\s\\d+\\s/)(OUTPUT|INPUT|INOUT)(\\s)(\\d+)(\\s\")([a-zA-Z0-9_]+)(\";)".r
        portReg.findAllIn(line).matchData foreach {
          m => {
            //println(m.group(1))
            //println(m.group(2))
            //println(m.group(3))
            //println(m.group(4))
            //println(m.group(5))
            //println(m.group(6))
            //println(m.group(7))
            //bb_txt += genBlackboxPort(m.group(2),m.group(4),m.group(6))
            bb_port = bb_port :+ new Tuple3(m.group(2), m.group(4), m.group(6))
            break()
          }
        }

        val paraStrReg = "(P_[a-zA-Z0-9]{16}\\s\\.param/str\\s\")([a-zA-Z0-9_]+)(\"\\s\\d+\\s\\d+\\s\\d+,\\s\")([a-zA-Z0-9_]+)(\";)".r
        val paraLReg = "(P_[a-zA-Z0-9]{16}\\s\\.param/l\\s\")([a-zA-Z0-9_]+)(\"\\s\\d+\\s\\d+\\s\\d+,\\sC4<)([01]+)(>;)".r
        paraStrReg.findAllIn(line).matchData foreach {
          m => {
            //println(m.group(2))
            //println(m.group(4))
            //genBlackboxPara("str",m.group(2),m.group(4))
            bb_para = bb_para :+ Tuple3("str", m.group(2), m.group(4))
            break()
          }
        }
        paraLReg.findAllIn(line).matchData foreach {
          m => {
            //println(m.group(2))
            //println(m.group(4))
            //genBlackboxPara("l",m.group(2),m.group(4))
            bb_para = bb_para :+ Tuple3("l", m.group(2), m.group(4))
            break()
          }
        }

      }
    }
    bb_txt
  }

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  def apply(ivlPathStr: String, srcFileStr: String) = {
    val ivlPath = Paths.get(ivlPathStr)
    val srcFilePath = Paths.get(srcFileStr)
    val iverilogOutFile = Paths.get(srcFilePath.getParent.toRealPath().toString + "\\" + srcFilePath.getFileName().toString.split("[.]").head)

    doCmd(s"""${ivlPath} -o ${iverilogOutFile} ${srcFilePath}""")
    val g = gen(iverilogOutFile.toString)
    //println(g)
    val bbFile = new java.io.FileWriter(Paths.get(srcFilePath.getParent.toRealPath().toString ,srcFilePath.getFileName().toString.split("[.]").head + ".scala").toFile)
    bbFile.write(g)
    bbFile.flush()
    bbFile.close()
  }

  def main(args: Array[String]): Unit = {
    val g = GenBlackbox(
      ivlPathStr = "C:\\iverilog\\bin\\iverilog.exe",
      srcFileStr = "E:\\working\\working\\SpinalAll\\SpinalHDL\\lib\\src\\main\\scala\\spinal\\lib\\blackbox\\gowin\\prim_syn.v"
    )
  }
}
