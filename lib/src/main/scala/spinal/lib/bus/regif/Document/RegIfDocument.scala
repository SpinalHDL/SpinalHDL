package spinal.lib.bus.regif

object RegIfDocument{
  implicit class FieldsDocument(fd: Field){
    def tds: String = {
      val reserved = if (fd.accType == AccessType.NA) "reserved" else ""
      s"""
         |            <td class="${reserved}" >${Section(fd.section)}</td>
         |            <td class="${reserved}" >${fd.name}</td>
         |            <td class="${reserved}" align="center">${fd.accType}</td>
         |            <td class="${reserved}" align="center">0x${fd.resetValue.toHexString}</td>
         |            <td class="${reserved} fixWidth2" >${fd.doc}</td>
         |""".stripMargin
    }

    def tr: String = {
      s"""
         |          <tr align="left">
         |${tds}
         |          </tr>
         |""".stripMargin
    }
  }

  implicit class RegInstDocument(reginst : RegInst){
    def tr0: String = {
      val fieldsNumbers = reginst.getFields.size
      s"""
         |          <tr class="reg" align="left">
         |            <td align="center" rowspan="${fieldsNumbers}">0x${reginst.addr.toHexString}</td>
         |            <td align="center" rowspan="${fieldsNumbers}">${reginst.name}</td>
         |            <td class="fixWidth" align="center" rowspan="${fieldsNumbers}">${reginst.doc} </td>
         |            <td align="center" rowspan="${fieldsNumbers}">${reginst.busif.busDataWidth}</td>
         |${reginst.getFields.last.tds}
         |          </tr>
         |""".stripMargin
    }

    def trs: String = {
      tr0 + reginst.getFields.reverse.tail.map(_.tr).foldLeft("")(_+_)
    }
  }
}

