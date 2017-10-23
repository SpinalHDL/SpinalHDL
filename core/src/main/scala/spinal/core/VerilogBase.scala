/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal.core

import scala.collection.mutable

/**
 * Created by PIC18F on 07.01.2015.
 */


trait VerilogBase extends VhdlVerilogBase{

  def emitExpressionWrap(e : Expression, name : String) : String = {
    s"  wire ${emitType(e)} ${name};\n"
  }

  def emitExpressionWrap(e : Expression, name : String, nature : String) : String = {
    s"  $nature ${emitType(e)} ${name};\n"
  }

//  def emitSignal(ref: Node, typeNode: Node): String = {
//    s"  ${emitDataType(typeNode)} ${emitReference(ref)};\n"
//  }
//

  def emitClockEdge(clock: String, edgeKind: EdgeKind): String = {
    s"${
      edgeKind match {
        case RISING => "posedge"
        case FALLING => "negedge"
      }
    } ${clock}"
  }

  def emitResetEdge(reset: String, polarity: Polarity): String = {
    s"${
      polarity match {
        case HIGH => "posedge"
        case LOW => "negedge"
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
    "/* " + values.reduce(_ + " , " + _) + " */ "
  }

  def emitEnumLiteral[T <: SpinalEnum](enum : SpinalEnumElement[T],encoding : SpinalEnumEncoding,prefix : String = "`") : String = {
    return prefix + enum.spinalEnum.getName() + "_" + encoding.getName() + "_" + enum.getName()
  }
  def emitEnumType[T <: SpinalEnum](enum : SpinalEnumCraft[T],prefix : String) : String = emitEnumType(enum.spinalEnum,enum.getEncoding,prefix)
  def emitEnumType(enum : SpinalEnum,encoding : SpinalEnumEncoding,prefix : String = "`") : String = {
    return prefix + enum.getName() + "_" + encoding.getName() + "_type"
  }

  def getReEncodingFuntion(spinalEnum: SpinalEnum, source: SpinalEnumEncoding, target: SpinalEnumEncoding): String = {
    s"${spinalEnum.getName()}_${source.getName()}_to_${target.getName()}"
  }



  def emitType(e : Expression) : String = e.getTypeObject match {
    case `TypeBool` => ""
    case `TypeBits` => emitRange(e.asInstanceOf[WidthProvider])
    case `TypeUInt` => emitRange(e.asInstanceOf[WidthProvider])
    case `TypeSInt` => emitRange(e.asInstanceOf[WidthProvider])
    case `TypeEnum` => e match {
      case e : EnumEncoded => emitEnumType(e.getDefinition, e.getEncoding)
    }
  }


  //  def emitDataType(node: Node) = node match {
//    case bool: Bool => ""
//    case bitvector: BitVector =>  emitRange(bitvector)
//    case enum: SpinalEnumCraft[_] => emitEnumType(enum,"`")
//    case _ => throw new Exception("Unknown datatype"); ""
//  }
//
  def emitDirection(baseType: BaseType) = baseType.dir match {
    case `in` => "input "
    case `out` => "output"
    case _ => throw new Exception("Unknown direction"); ""
  }


  def emitRange(node: WidthProvider) = s"[${node.getWidth - 1}:0]"

  def signalNeedProcess(baseType: BaseType) : Boolean = {
    if(baseType.isReg) return true
    if(baseType.dlcIsEmpty) return false
    if(!baseType.hasOnlyOneStatement || baseType.head.parentScope != baseType.rootScopeStatement) return true
    return false
  }

//
//  var referenceSet : mutable.Set[Node with Nameable with ContextUser] = null
//  def emitReference(node: Node): String = {
//    node match {
//      case n: Nameable with ContextUser => {
//        if(referenceSet != null) referenceSet.add(n)
//        n.getNameElseThrow
//      }
//    }
//  }
//  def emitAssignedReference(node: Node): String = {
//    node match {
//      case n: Nameable with ContextUser => {
//        n.getNameElseThrow
//      }
//    }
//  }
}
