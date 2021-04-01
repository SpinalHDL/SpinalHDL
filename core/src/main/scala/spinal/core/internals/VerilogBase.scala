/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core.internals

import spinal.core._

trait VerilogTheme{
  def tab      = "  "
  def porttab  = ""
  def maintab  = ""
}

class Tab2 extends VerilogTheme {
  override def tab      = "  "
  override def porttab  = "  "
  override def maintab  = "  "
}

class Tab4 extends VerilogTheme {
  override def tab      = "    "
  override def porttab  = ""
  override def maintab  = ""
}


trait VerilogBase extends VhdlVerilogBase{
  val theme = new Tab2 //TODO add into SpinalConfig

  def expressionAlign(net: String, section: String, name: String) = {
    f"$net%-10s $section%-8s $name"
  }

  def emitExpressionWrap(e: Expression, name: String): String = {
//    s"  wire ${emitType(e)} ${name};\n"
    if (!e.isInstanceOf[SpinalStruct])
      theme.maintab + expressionAlign("wire", emitType(e), name) + ";\n"
    else
      theme.maintab + expressionAlign(e.asInstanceOf[SpinalStruct].getTypeString, "", name) + ";\n"
  }

  def emitExpressionWrap(e: Expression, name: String, nature: String): String = {
//    s"  $nature ${emitType(e)} ${name};\n"
    theme.maintab + expressionAlign(nature, emitType(e), name) + ";\n"
  }

  def emitClockEdge(clock: String, edgeKind: EdgeKind): String = {
    s"${
      edgeKind match {
        case RISING  => "posedge"
        case FALLING => "negedge"
      }
    } ${clock}"
  }

  def emitResetEdge(reset: String, polarity: Polarity): String = {
    s"${
      polarity match {
        case HIGH => "posedge"
        case LOW  => "negedge"
      }
    } ${reset}"
  }

  def emitSyntaxAttributes(attributes: Iterable[Attribute]): String = {
    val values = for (attribute <- attributes if attribute.attributeKind() == DEFAULT_ATTRIBUTE) yield attribute match {
      case attribute: AttributeString => attribute.getName + " = \"" + attribute.value + "\""
      case attribute: AttributeFlag => attribute.getName
    }

    if(values.isEmpty) return ""

    "(* " + values.reduce(_ + " , " + _) + " *) "
  }

  def emitCommentAttributes(attributes: Iterable[Attribute]): String = {
    val values = for (attribute <- attributes if attribute.attributeKind() == COMMENT_ATTRIBUTE) yield attribute match {
      case attribute: AttributeString => attribute.getName + " = \"" + attribute.value + "\""
      case attribute: AttributeFlag => attribute.getName
    }

    if(values.isEmpty) return ""

    " /* " + values.reduce(_ + " , " + _) + " */ "
  }

  def emitEnumLiteral[T <: SpinalEnum](enum: SpinalEnumElement[T], encoding: SpinalEnumEncoding, prefix: String = "`"): String = {
    prefix + enum.spinalEnum.getName() + "_" + encoding.getName() + "_" + enum.getName()
  }

  def emitEnumType[T <: SpinalEnum](enum: SpinalEnumCraft[T], prefix: String): String = emitEnumType(enum.spinalEnum, enum.getEncoding, prefix)

  def emitEnumType(enum: SpinalEnum, encoding: SpinalEnumEncoding, prefix: String = "`"): String = {
    prefix + enum.getName() + "_" + encoding.getName() + "_type"
  }

  def getReEncodingFuntion(spinalEnum: SpinalEnum, source: SpinalEnumEncoding, target: SpinalEnumEncoding): String = {
    s"${spinalEnum.getName()}_${source.getName()}_to_${target.getName()}"
  }

  def emitStructType(struct: SpinalStruct): String = {
    return struct.getTypeString
  }

  def emitType(e: Expression): String = e.getTypeObject match {
    case `TypeBool` => ""
    case `TypeBits` => emitRange(e.asInstanceOf[WidthProvider])
    case `TypeUInt` => emitRange(e.asInstanceOf[WidthProvider])
    case `TypeSInt` => emitRange(e.asInstanceOf[WidthProvider])
    case `TypeEnum` => e match {
      case e : EnumEncoded => emitEnumType(e.getDefinition, e.getEncoding)
    }
    case `TypeStruct` => emitStructType(e.asInstanceOf[SpinalStruct])
  }

  def emitDirection(baseType: BaseType) = baseType.dir match {
    case `in`    => "input "
    case `out`   => "output"
    case `inout` => "inout"
    case _       => throw new Exception("Unknown direction"); ""
  }

  def emitRange(node: WidthProvider) = s"[${node.getWidth - 1}:0]"

  def signalNeedProcess(baseType: BaseType): Boolean = {
    if(baseType.isReg) return true
    if(baseType.dlcIsEmpty || baseType.isAnalog) return false
    if(!baseType.hasOnlyOneStatement || baseType.head.parentScope != baseType.rootScopeStatement) return true
    return false
  }

}
