package spinal.lib.bus.regif

import spinal.core.GlobalData
import spinal.lib.bus.regif.{BusIfVisitor, FifoDescr, RegDescr}

import java.io.PrintWriter

final case class RalfGenerator(name: String) extends BusIfVisitor {
  val sb : StringBuilder = new StringBuilder
  var prefix = ""

  def clean(str : String) : String = {
    str.replace("\n","\\n").replace("\r","\\r")
  }

  def begin(busDataWidth : Int) : Unit = {
    sb ++=
      s"""block ${name}{
         |  endian little;
         |  bytes ${scala.math.ceil(busDataWidth/4).toInt};
         |""".stripMargin
  }

  def visit(descr : FifoDescr)  : Unit = {

  }

  def visit(descr : RegDescr) : Unit = {
    sb ++= prefix

    sb ++=
      s"  register ${descr.getName()} @'h${descr.getAddr().toHexString} {\n"

    descr.getFieldDescrs().foreach(f =>{
      sb ++=
        s"""|    field ${f.getName()} @${f.getSection().last} {
            |      bits ${f.getWidth()} ;
            |      access ${f.getAccessType().toString.toLowerCase()} ;
            |      reset  ${f.getWidth()}'h${f.getResetValue().toHexString};
            |     }
            |""".stripMargin
    })
    sb ++= "\n"

    sb ++=
      s"  }".stripMargin
  }

  def end() : Unit = {
    val pc = GlobalData.get.phaseContext
    val targetPath = s"${pc.config.targetDirectory}/${name}.ralf"
    val pw = new PrintWriter(targetPath)

    sb ++= "}\n"
    pw.write(sb.toString())

    pw.close()
  }
}