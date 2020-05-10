package spinal.lib.bus.regif

object CHeads {
  def reservedRenamed(fields: List[Field], pre: String = "uint32_t") = {
    var t = ""
    fields.map{ fd =>
      val name = if (fd.accType == AccessType.NA) "na_" + t else fd.name
      if (fd.accType == AccessType.NA) t += "_"
      s"        $pre ${name}:${fd.hardbit.getWidth};"
    }.mkString("\n")
  }

  implicit class RegInstCStruct(reginst : RegInst) {
    def cStruct(pre: String = ""): String = {
      val uint_t = s"uint${reginst.busif.busDataWidth}_t"
      s"""
         |typedef union{
         |    $uint_t val;
         |    struct{
         |${reservedRenamed(reginst.getFields.toList, uint_t)}
         |    } reg;
         |} ${(pre + reginst.name).toLowerCase()}_t""".stripMargin
    }
  }

  implicit class RegInstCHead(reginst : RegInst) {
    def cHeadDefine(alignWidth: Int = 20, pre: String = ""): String = {
      s"#define %-${alignWidth}s = 0x%s".format((pre + reginst.name).toUpperCase(), reginst.addr.toHexString)
    }
  }
}
