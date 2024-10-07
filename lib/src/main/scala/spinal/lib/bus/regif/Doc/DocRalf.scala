package spinal.lib.bus.regif


final case class DocRalf(name : String, backdoor: Boolean = true) extends BusIfDoc {
  override val suffix: String = "ralf"
  override def body(): String = {
      s"""
         |block ${this.name} {
         |  endian little;
         |  bytes ${bi.busByteWidth};
         |${bi.RegAndFifos.map(_.toRalf).mkString(";\n")};
         |${bi.RamInsts.map(_.toRalf).mkString(";\n")};
         |}""".stripMargin
  }

  implicit class RegSliceExtend(reg: RegSlice) {
    def toRalf: String = {
        s"""  register ${reg.upperName()} @'h${reg.getAddr().toString(16).toUpperCase} {
           |${reg.getFields().map(_.toRalf).mkString("\n")}
           |  }""".stripMargin
    }
  }

  implicit class RamSliceExtend(ram: RamInst) {
    def toRalf: String = {
      val hdlpath = if(backdoor) s"(${ram.getName()})" else ""
      s"""  memory ${ram.upperName()} ${hdlpath} @'h${ram.getAddr().toString(16).toUpperCase} {
         |    size  ${ram.getSize()};
         |    bits  ${ram.bi.busDataWidth};
         |    access rw;
         |  }""".stripMargin
    }
  }

  implicit class FieldDescrExtend(fd: Field) {
    def toRalf: String = {
      def rename(name: String) = {
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
  }
}
