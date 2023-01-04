package spinal.lib.bus.regif
import spinal.core.GlobalData
import spinal.lib.bus.regif._

import java.io.PrintWriter

final case class RalfGenerator(fileName : String, backdoor: Boolean = true) extends BusIfVisitor {
  val sb : StringBuilder = new StringBuilder
  var prefix = ""
  var width = 0

  def clean(str : String) : String = {
    str.replace("\n","\\n").replace("\r","\\r")
  }

  def begin(busDataWidth : Int) : Unit = {
    width = busDataWidth
  }

  def visit(descr : BaseDescriptor) : Unit = {
    descr match {
      case descr: RegDescr => regDescrVisit(descr)
      case _ => ???
    }
  }

  private def regDescrVisit(descr: RegDescr) = {
    def formatResetValue(value: BigInt, bitCount: Int):String = {
      val hexCount = scala.math.ceil(bitCount/4.0).toInt
      val unsignedValue = if(value >= 0) value else ((BigInt(1) << bitCount) + value)
      s"${bitCount}'h%${hexCount}s".format(unsignedValue.toString(16)).replace(' ','0')
    }

    def fieldDescr(fd: FieldDescr) = {
      def rename(name: String) = {
//        if(fd.getWidth() == 1){
//          if(name == "--") s"RSV_${fd.getSection().start}" else name
//        } else {
//          if(name == "--") s"RSV_${fd.getSection().start}_${fd.getSection().end}" else name
//        }
        val pre = if(fd.getWidth() == 1) s"${fd.getSection().start}" else s"${fd.getSection().start}_${fd.getSection().end}"
        val name = fd.getAccessType match {
                        case AccessType.NA  => s"rsv_${pre}"
                        case AccessType.ROV => s"rov_${pre}"
                        case _ => fd.getName()
                    }
        name
      }

      def access = {
        fd.getAccessType() match{
          case x if fd.uvmBaseAcc.contains(x) => x.toString.toLowerCase()
          case AccessType.NA  => "ro"
          case AccessType.ROV => "ro"
          case _ => "rw"
        }
      }

      def attribute = {
        if(fd.getAccessType() == AccessType.NA){
          "\n      attributes {NO_REG_TEST 1};"
        } else ""
      }

      val forhdlpath = if(backdoor) s"(${rename(fd.getName())})" else ""
      s"""    field ${rename(fd.getName())} $forhdlpath @${fd.getSection().end} {
         |      bits ${fd.getWidth()};
         |      access ${access};
         |      reset ${formatResetValue(fd.getResetValue(), fd.getWidth())};${attribute}
         |    }""".stripMargin
    }

    val ret =
      s"""  register ${descr.getName()} @'h${descr.getAddr().toString(16).toUpperCase} {
         |${descr.getFieldDescrs().map(fieldDescr).mkString("\n")}
         |  }
         |""".stripMargin

    sb ++= ret
  }

  def end() : Unit = {
    val pc = GlobalData.get.phaseContext
    val targetPath = s"${pc.config.targetDirectory}/${fileName}.ralf"
    val pw = new PrintWriter(targetPath)

    val cont = s"""
      |block ${this.fileName} {
      |  endian little;
      |  bytes ${scala.math.ceil(this.width/8.0).toInt};
      |${sb}}
      |""".stripMargin

    pw.write(cont.toString())

    pw.close()
  }
}