package spinal.lib.bus.regif


object HtmlGenerator {
  @deprecated("Use DocHtml instead of HtmlGenerator", "2024.12.31")
  def apply(fileName: String, title: String): BusIfDoc = {
    DocHtml(fileName)
  }
}

object CHeaderGenerator {
  @deprecated("Use DocCHeader instead of CHeaderGenerator", "2024.12.31")
  def apply(fileName: String, prefix: String, regType: String = "u32", headers: List[String] = Nil, withshiftmask: Boolean = true) = {
    DocCHeader(fileName, prefix, regType, withshiftmask)
      .setheader(headers.mkString("\n")).asInstanceOf[DocCHeader]
  }
}

object JsonGenerator {
  @deprecated("Use DocJson instead of JsonGenerator", "2024.12.31")
  def apply(fileName: String): DocJson = {
    DocJson(fileName)
  }
}

object  RalfGenerator {
  @deprecated("Use DocRalf instead of RalfGenerator", "2024.12.31")
  def apply(fileName: String): DocRalf = {
    DocRalf(fileName)
  }
}

object SystemRdlGenerator {
  @deprecated("Use DocSystemRdl instead of SystemRdlGenerator", "2024.12.31")
  def apply(fileName: String, addrmapName: String, name: Option[String] = None, desc: Option[String] = None): DocSystemRdl = {
    DocSystemRdl(fileName)
  }
}