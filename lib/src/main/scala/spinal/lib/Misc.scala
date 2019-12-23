package spinal.lib

import spinal.core._

class BoolPimped(pimped : Bool){

  //Warning, there is no overflow protection
  def genEvent : Event = {
    val e = Event
    val reg = RegInit(False)

    reg := (reg | pimped) & !e.ready
    e.valid := pimped | reg

    e
  }
}

/**
 * Endianness enumeration
 */
sealed trait Endianness
/** Little-Endian */
object LITTLE extends Endianness
/** Big-Endian */
object BIG    extends Endianness





object KeepAttribute{
  object syn_keep_verilog extends AttributeFlag("synthesis syn_keep = 1", COMMENT_ATTRIBUTE){
    override def isLanguageReady(language: Language) : Boolean = language == Language.VERILOG || language == Language.SYSTEM_VERILOG
  }

  object syn_keep_vhdl extends AttributeFlag("syn_keep"){
    override def isLanguageReady(language: Language) : Boolean = language == Language.VHDL
  }
  object keep extends AttributeFlag("keep")

  def apply[T <: Data](that : T) = that.addAttribute(keep).addAttribute(syn_keep_verilog).addAttribute(syn_keep_vhdl)
}