package spinal.core.internals

import java.text.SimpleDateFormat
import java.util.Calendar

import spinal.core._

trait MemBitsMaskKind
object MULTIPLE_RAM extends MemBitsMaskKind
object SINGLE_RAM extends MemBitsMaskKind

object VhdlVerilogBase{
  def getHeader(commentSymbole : String,toplevel : Component): String =
    s"""$commentSymbole Generator : SpinalHDL v${Spinal.version}    git head : ${spinal.core.Info.gitHash}
       |$commentSymbole Date      : ${new SimpleDateFormat("dd/MM/yyyy, HH:mm:ss").format(Calendar.getInstance().getTime)}
       |$commentSymbole Component : ${toplevel.definitionName}
       |
       |""".stripMargin
}


trait VhdlVerilogBase {

}
