package spinal.lib.bus.regif

final case class DocJson(name : String) extends BusIfDoc {
  override val suffix: String = "json"

  override def body(): String = {
    s"""{
      |  "system"     : "${name}",
      |  "busport"    : "${bi.busName}",
      |  "datawidth"  : ${bi.busDataWidth},
      |  "addrwidth"  : ${bi.busAddrWidth},
      |  "byteMask"   : ${bi.withStrb},
      |  "date"       : "${java.time.LocalDate.now}",
      |  "ToolVersion": "${bi.getVersion}",
      |  "slices"     : [
      |     ${bi.slices.map(_.toJson).mkString(",\n     ")}]
      |}""".stripMargin
  }

  implicit class RegSliceExtend(reg: RegSlice) {
    def toJson: String = {
      s"""|{"addr"   : ${reg.getAddr()},
          |      "name"   : "${reg.getName()}",
          |      "type"   : "${reg.regType}",
          |      "doc"    : "${clean(reg.getDoc())}",
          |      "grp"    : "${reg.getGrp}",
          |      "fields" : [
          |         ${reg.getFields().map(_.toJson).mkString(",\n         ")}
          |      ]
          |     }""".stripMargin
    }
  }

  implicit class FieldDescrExtend(field: Field) {
    def toJson: String = {
      s"""|{"name":"${field.getName()}", "pos":{"start":${field.section.min}, "end":${field.section.max}}, "access":"${field.getAccessType()}", "width":${field.getWidth()}, "reset":${field.getResetValue()}, "doc":"${clean(field.getDoc())}"}""".stripMargin
    }
  }
}
