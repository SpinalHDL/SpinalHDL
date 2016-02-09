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

/**
 * Created by PIC18F on 07.01.2015.
 */
trait VhdlBase {

  val vhdlKeyWords = Set[String]("in", "out", "buffer", "inout", "entity", "component", "architecture","type","open")

//  override def getReservedKeyword(): Iterable[String] = {
//    return reservedKeyWords
//  }


  def emitSignal(ref: Node, typeNode: Node): String = {
    s"  signal ${emitReference(ref)} : ${emitDataType(typeNode)};\n"
  }


  def emitClockEdge(clock: Bool, edgeKind: EdgeKind): String = {
    s"${
      edgeKind match {
        case RISING => "rising_edge"
        case FALLING => "falling_edge"
      }
    }(${emitReference(clock)}) then\n"
  }


  def emitEnumLiteral[T <: SpinalEnum](enum : SpinalEnumElement[T],encoding: SpinalEnumEncoding) : String = {
    return enum.parent.getName() + "_" + encoding.getName() + "_" + enum.getName()
  }

  def emitEnumType[T <: SpinalEnum](enum : SpinalEnumCraft[T]) : String = {
    return enum.blueprint.getName() + "_" + enum.encoding.getName() + "_type"
  }
  def emitEnumType(enum : SpinalEnum,encoding: SpinalEnumEncoding) : String = {
    return enum.getName() + "_" + encoding.getName() + "_type"
  }

  def emitDataType(node: Node, constrained: Boolean = true) = node match {
    case bool: Bool => "std_logic"
    case uint: UInt => s"unsigned${if (constrained) emitRange(uint) else ""}"
    case sint: SInt => s"signed${if (constrained) emitRange(sint) else ""}"
    case bits: Bits => s"std_logic_vector${if (constrained) emitRange(bits) else ""}"
    case mem: Mem[_] => s"${emitReference(mem)}_type"
    case enum: SpinalEnumCraft[_] => emitEnumType(enum)
    case _ => throw new Exception("Unknown datatype"); ""
  }

  def emitDirection(baseType: BaseType) = baseType.dir match {
    case `in` => "in"
    case `out` => "out"
    case _ => throw new Exception("Unknown direction"); ""
  }


  def emitRange(node: Node) = s"(${node.getWidth - 1} downto 0)"

  def emitReference(node: Node): String = {
    node match {
      case n: Nameable => {
        n.getNameElseThrow
      }
    }
  }

  def isReferenceable(node: Node) = node.isInstanceOf[Nameable]
}
