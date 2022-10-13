package spinal.lib.bus.regif

import spinal.core.GlobalData
import spinal.lib.bus.regif._
import java.io.PrintWriter
import java.util.Calendar

final case class SystemRdlGenerator(
    fileName: String,
    addrmapName: String,
    name: Option[String] = None,
    desc: Option[String] = None
) extends BusIfVisitor {
  val sb: StringBuilder = new StringBuilder
  var prefix = ""

  def clean(str: String): String = {
    str.replace("\n", "\\n").replace("\r", "\\r")
  }

  def begin(busDataWidth: Int): Unit = {
    sb ++=
      s"""|// SystemRDL 2.0 register description for ${addrmapName}
          |// Generated from SpinalHDL RegIf definition. Don't edit.
          |// Date: ${Calendar.getInstance().getTime()}
          |
          |addrmap ${addrmapName} {
          |""".stripMargin
    if (name.isDefined) {
      sb ++= s"""    name = "${name}";\n"""
    }
    if (desc.isDefined) {
      sb ++= s"""    desc = "${desc}";\n"""
    }
    if (name.isDefined || desc.isDefined) {
      sb ++= "\n"
    }
    sb ++=
      """|    default hw = rw;
         |    default sw = rw;
         |
         |""".stripMargin
  }

  def visit(descr: BaseDescriptor): Unit = {
    descr match {
      case descr: RegDescr => regDescrVisit(descr)
      case _               => ???
    }
  }

  private def regDescrVisit(descr: RegDescr) = {
    sb ++= prefix

    sb ++=
      s"""|    reg {
          |        name = "${descr.getName()}";
          |        desc = "${clean(descr.getDoc())}";
          |
          |""".stripMargin

    var fieldPrefix = ""
    descr
      .getFieldDescrs()
      .foreach(f => {
        // Ignore reserved.
        if (f.getName() != "--") {
          sb ++= fieldPrefix
          val section = s"${f.getSection().start}:${f.getSection().end}"
          // TODO: Add AccessType mapping.
          sb ++=
            f"""|        field {
                |            name = "${f.getName()}%s";
                |            desc = "${clean(f.getDoc())}%s";
                |            reset = 0x${f.getResetValue()}%X;
                |        } ${f.getName()}%s[${section}%s];""".stripMargin
          fieldPrefix = "\n\n"
        }
      })
    sb ++= "\n"
    sb ++= f"""    } ${descr.getName()} @ 0x${descr.getAddr()}%X;"""

    prefix = "\n\n"
  }

  def end(): Unit = {
    val pc = GlobalData.get.phaseContext
    val targetPath = s"${pc.config.targetDirectory}/${fileName}.rdl"
    val pw = new PrintWriter(targetPath)

    sb ++= "};\n"
    pw.write(sb.toString())
    pw.close()
  }
}
