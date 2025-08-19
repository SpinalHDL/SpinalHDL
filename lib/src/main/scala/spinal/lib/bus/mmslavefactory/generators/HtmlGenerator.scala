package spinal.lib.bus.mmslavefactory.generators

import spinal.core.GlobalData
import spinal.lib.bus.mmslavefactory._
import java.io.PrintWriter

final case class HtmlGenerator(fileName : String, name : String) extends MMSlaveFactoryVisitor {
    val sb : StringBuilder = new StringBuilder("")
    var dataWidth : Int = 32

    def begin(busDataWidth : Int) : Unit = {
        dataWidth = busDataWidth
    }

    def visit(descr : FifoDescr)  : Unit = {
        
    }

    def formatResetValue(value: BigInt, bitCount: Int):String = {
        val hexCount = scala.math.ceil(bitCount/4.0).toInt
        val unsignedValue = if(value >= 0) value else ((BigInt(1) << bitCount) + value)
        if(value == 0) s"${bitCount}'b0" else s"${bitCount}'h%${hexCount}s".format(unsignedValue.toString(16)).replace(' ','0')
    }

    def genTds(field : FieldDescr, access : String) : String = {
        val reserved = if (field.isReserved) "reserved" else ""
        s"""            <td class="${reserved}">${Section(field.getSection)}</td>
           |            <td class="${reserved}">${field.getName}</td>
           |            <td class="${reserved}" align="center">${access}</td>
           |            <td class="${reserved}" align="right">${formatResetValue(field.getResetValue, field.getWidth)}</td>
           |            <td class="${reserved} fixWidth2">${field.getDoc}</td>""".stripMargin
    }

    def genTr(field : FieldDescr, access : String): String = {
        s"""          <tr align="left">
           |${genTds(field, access)}
                      </tr>""".stripMargin
    }

    def visit(descr : RegDescr) : Unit = {
        val fieldsNumbers = descr.getFieldDescrs.size
        sb ++=
        s"""          <tr class="reg" align="left">
           |            <td align="center" rowspan="${fieldsNumbers}">0x${descr.getAddr.toHexString}</td>
           |            <td align="left" rowspan="${fieldsNumbers}">${(descr.getName).toUpperCase()}</td>
           |            <td class="fixWidth" align="center" rowspan="${fieldsNumbers}">${descr.getDoc} </td>
           |            <td align="center" rowspan="${fieldsNumbers}">${dataWidth}</td>
           |${genTds(descr.getFieldDescrs.last, descr.getAccess)}
           |          </tr>""".stripMargin

        descr.getFieldDescrs().reverse.tail.foreach(sb ++= genTr(_,descr.getAccess))
    }

    def end() : Unit = {
        val pc = GlobalData.get.phaseContext
        val targetPath = s"${pc.config.targetDirectory}/${fileName}"
        val pw = new PrintWriter(targetPath)

        pw.write(HtmlTemplate.getHTML(name, sb.toString()))

        pw.close()
    }
}