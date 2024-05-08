package spinal.lib.bus.regif

final case class DocJson(name : String) extends BusIfDoc {
  override val suffix: String = "json"

  override def body(): String = {
    s"""[
      |${bi.slices.map(_.toJson).mkString(",\n")}
      |]
      |""".stripMargin
  }

  implicit class RegSliceExtend(reg: RegSlice) {
    def toJson: String = {
      s"""|{
          |   "addr"   : ${reg.getAddr()},
          |   "name"   : "${reg.getName()}",
          |   "doc"    : "${clean(reg.getDoc())}",
          |   "fields" :[${reg.getFields().map(_.toJson).mkString(",\n")}]
          |}""".stripMargin
    }
  }

  implicit class FieldDescrExtend(field: Field) {
    def toJson: String = {
      s"""|       {
          |           "accType" : "${field.getAccessType()}",
          |           "name"    : "${field.getName()}",
          |           "width"   : ${field.getWidth()},
          |           "reset"   : ${field.getResetValue()},
          |           "doc"     : "${clean(field.getDoc())}"
          |       }""".stripMargin
    }
  }
}
