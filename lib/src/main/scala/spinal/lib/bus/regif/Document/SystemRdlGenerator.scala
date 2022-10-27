package spinal.lib.bus.regif

import spinal.core.GlobalData
import spinal.lib.bus.regif._
import java.io.PrintWriter
import java.util.Calendar
import spinal.lib.bus.regif.AccessType._

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
      sb ++= s"""    name = "${name.get}";\n"""
    }
    if (desc.isDefined) {
      sb ++= s"""    desc = "${desc.get}";\n"""
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

  private def mapAccessType(accessType: AccessType): String = {
    val indent = "            "
    val out = accessType match {
      case RO    => "sw = r;"
      case RW    => "sw = rw;"
      case RC    => "sw = r;\nonread = rclr;"
      case RS    => "sw = r;\nonread = rset;"
      case WRC   => "sw = rw;\nonread = rclr;"
      case WRS   => "sw = rw;\nonread = rset;"
      case WC    => "sw = rw;\nonwrite = wclr;"
      case WS    => "sw = rw;\nonwrite = wset;"
      case WSRC  => "sw = rw;\nonwrite = wset;\nonread = rclr;"
      case WCRS  => "sw = rw;\nonwrite = wclr;\nonread = rset;"
      case W1C   => "sw = rw;\nonwrite = woclr;"
      case W1S   => "sw = rw;\nonwrite = woset;"
      case W1T   => "sw = rw;\nonwrite = wot;"
      case W0C   => "sw = rw;\nonwrite = wzc;"
      case W0S   => "sw = rw;\nonwrite = wzs;"
      case W0T   => "sw = rw;\nonwrite = wzt;"
      case W1SRC => "sw = rw;\nonwrite = woset;\nonread = rclr;"
      case W1CRS => "sw = rw;\nonwrite = woclr;\nonread = rset;"
      case W0SRC => "sw = rw;\nonwrite = wzs;\nonread = rclr;"
      case W0CRS => "sw = rw;\nonwrite = wzc;\nonread = rset;"
      case WO    => "sw = w;"
      case WOC   => "sw = w;\nonwrite = wclr;"
      case WOS   => "sw = w;\nonwrite = wset;"
      case W1 =>
        "sw = rw;\nonwrite = wuser;  // First one after HARD reset is as-is, other W have no effect."
      case WO1 =>
        "sw = w;\nonwrite = wuser;  // First one after HARD reset is as-is, other W have no effect."
      case NA  => "sw = na;"
      case W1P => "sw = rw;\nsinglepulse;  // On 1 pulse on matching bit."
      case W0P =>
        "sw = rw;\nonwrite = wuser;  // On 0 write pulse on matching bit."
      case _ => "// Unknown access type. Report an issue on SpinalHDL's GitHub."
    }
    out.replaceAll("\n", "\n" + indent)
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
          sb ++=
            f"""|        field {
                |            name = "${f.getName()}%s";
                |            desc = "${clean(f.getDoc())}%s";
                |            reset = 0x${f.getResetValue()}%X;
                |            ${mapAccessType(f.getAccessType())}
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

    sb ++= "\n};\n"
    pw.write(sb.toString())
    pw.close()
  }
}
