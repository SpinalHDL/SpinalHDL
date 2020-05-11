package spinal.lib.bus.regif

object RegIfDocument{

  def FormatResetValue(value: BigInt, bitCount: Int):String = {
    val hexCount = scala.math.ceil(bitCount/4.0).toInt
    val unsignedValue = if(value >= 0) value else ((BigInt(1) << bitCount) + value)
    if(value == 0) s"${bitCount}'b0" else s"${bitCount}'h%${hexCount}s".format(unsignedValue.toString(16)).replace(' ','0')
  }

  implicit class FieldsDocument(fd: Field){
    def tds: String = {
      val reserved = if (fd.accType == AccessType.NA) "reserved" else ""
      s"""            <td class="${reserved}" >${Section(fd.section)}</td>
         |            <td class="${reserved}" >${fd.name}</td>
         |            <td class="${reserved}" align="center">${fd.accType}</td>
         |            <td class="${reserved}" align="right">${FormatResetValue(fd.resetValue, fd.hardbit.getWidth)}</td>
         |            <td class="${reserved} fixWidth2" >${fd.doc}</td>""".stripMargin
    }

    def tr: String = {
      s"""          <tr align="left">
         |${tds}
         |          </tr>""".stripMargin
    }
  }

  implicit class RegInstDocument(reginst : RegInst){
    def tr0(pre: String = ""): String = {
      val fieldsNumbers = reginst.getFields.size
      s"""          <tr class="reg" align="left">
         |            <td align="center" rowspan="${fieldsNumbers}">0x${reginst.addr.toHexString}</td>
         |            <td align="left" rowspan="${fieldsNumbers}">${(pre + reginst.name).toUpperCase()}</td>
         |            <td class="fixWidth" align="center" rowspan="${fieldsNumbers}">${reginst.doc} </td>
         |            <td align="center" rowspan="${fieldsNumbers}">${reginst.busif.busDataWidth}</td>
         |${reginst.getFields.last.tds}
         |          </tr>""".stripMargin
    }

    def trs(pre: String = ""): String = {
      reginst.checkLast
      tr0(pre) + reginst.getFields.reverse.tail.map(_.tr).mkString
    }
  }
}

