package spinal.lib.bus.regif

import java.util.Calendar
import spinal.lib.bus.regif.AccessType._

final case class DocSystemRdl(name: String) extends BusIfDoc {
  override val suffix: String = "rdl"

  override def body(): String = {
    s"""|// SystemRDL 2.0 register description for ${name}
        |// Generated from SpinalHDL RegIf definition. Don't edit.
        |// Date: ${Calendar.getInstance().getTime()}
        |
        |addrmap ${name} {
        |    default hw = rw;
        |    default sw = rw;
        |${bi.slices.map(_.toSystemRdl).mkString("\n")}
        |}""".stripMargin
  }

  implicit class RegSliceExtend(reg: RegSlice) {
    def toSystemRdl: String = {
      s"""   reg {
        |          name = "${reg.upperName()}";
        |          desc = "${clean(reg.getDoc())}";
        |${reg.getFields().map(_.toSystemRdl).mkString("\n")}
        |   } ${reg.getName()} @ 0x${reg.getAddr()}%X;
        |""".stripMargin
    }
  }

  implicit class FieldDescrExtend(fd: Field) {
    def toSystemRdl: String = {
      f"""|        field {
          |            name = "${fd.getName()}%s";
          |            desc = "${clean(fd.getDoc())}%s";
          |            reset = 0x${fd.getResetValue()}%X;
          |            ${mapAccessType(fd.getAccessType())}
          |        } ${fd.getName()}%s[${fd.section}%s];""".stripMargin
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
      case W1    => "sw = rw;\nonwrite = wuser;  // First one after HARD reset is as-is, other W have no effect."
      case WO1   => "sw = w;\nonwrite = wuser;  // First one after HARD reset is as-is, other W have no effect."
      case NA    => "sw = na;"
      case W1P   => "sw = rw;\nsinglepulse;  // On 1 pulse on matching bit. Pulse reg out"
      case W1I   => "sw = rw;\nsinglepulse;  // On 1 pulse on matching bit. ImPulse combination out"
      case W0P   => "sw = rw;\nonwrite = wuser;  // On 0 write pulse on matching bit."
      case _     => "// Unknown access type. Report an issue on SpinalHDL's GitHub."
    }
    out.replaceAll("\n", "\n" + indent)
  }
}
