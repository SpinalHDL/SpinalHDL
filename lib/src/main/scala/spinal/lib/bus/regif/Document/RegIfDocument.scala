package spinal.lib.bus.regif

object RegIfDocument{
  implicit class FieldsDocument(fd: Field){
    def tds: String = {
      s"""
         |            <td>${Section(fd.section)}</td>
         |            <td>${fd.name}</td>
         |            <td align="center">${fd.accType}</td>
         |            <td align="center">0x${fd.resetValue.toHexString}</td>
         |            <td>${fd.doc}</td>
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
      val fieldsNumbers = reginst.fields.size
      s"""
         |          <tr class="reg" align="left">
         |            <td align="center" rowspan="${fieldsNumbers}">0x${reginst.addr.toHexString}</td>
         |            <td align="center" rowspan="${fieldsNumbers}">${reginst.name}</td>
         |            <td align="center" rowspan="${fieldsNumbers}">${reginst.doc} </td>
         |            <td align="center" rowspan="${fieldsNumbers}">${reginst.busif.busDataWidth}</td>
         |${reginst.fields.last.tds}
         |          </tr>
         |""".stripMargin
    }

    def trs: String = {
      tr0 + reginst.fields.reverse.tail.map(_.tr).foldLeft("")(_+_)
    }
  }
}

