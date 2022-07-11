package spinal.lib.bus.regif
import spinal.core.GlobalData
import spinal.lib.bus.regif._

import java.io.PrintWriter

final case class RalfGenerator(fileName : String) extends BusIfVisitor {
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
        if(fd.getWidth() == 1){
          if(name == "--") s"RSV_${fd.getSection().start}" else name
        } else {
          if(name == "--") s"RSV_${fd.getSection().start}_${fd.getSection().end}" else name
        }
      }
      def access = {
        val ret = fd.getAccessType().toString.toLowerCase()
        if(ret == "na") "ro" else ret
      }
      s"""    field ${rename(fd.getName())} @${fd.getSection().end} {
         |      bits ${fd.getWidth()};
         |      access ${access};
         |      reset ${formatResetValue(fd.getResetValue(), fd.getWidth())};
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