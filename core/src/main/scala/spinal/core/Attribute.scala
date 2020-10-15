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
package spinal.core


sealed trait AttributeKind
object COMMENT_ATTRIBUTE extends AttributeKind
object DEFAULT_ATTRIBUTE extends AttributeKind


sealed trait Language
object Language {
  object VERILOG        extends Language
  object SYSTEM_VERILOG extends Language
  object VHDL           extends Language
}


trait Attribute extends SpinalTag{
  def getName: String
  def sameType(that: Attribute): Boolean
  def attributeKind(): AttributeKind

  override def toString: String = getName

  def isLanguageReady(language: Language): Boolean = true
}


class AttributeString(name: String, val value: String, kind: AttributeKind = DEFAULT_ATTRIBUTE) extends Attribute {
  override def getName: String = name
  override def sameType(that: Attribute): Boolean = that.isInstanceOf[AttributeString]
  override def attributeKind() = kind
}


class AttributeFlag(name: String, kind: AttributeKind = DEFAULT_ATTRIBUTE) extends Attribute {
  override def getName: String = name
  override def sameType(that: Attribute): Boolean = that.isInstanceOf[AttributeFlag]
  override def attributeKind() = kind
}


object Verilator{
  object public extends AttributeFlag("verilator public", COMMENT_ATTRIBUTE){
    override def isLanguageReady(language: Language) : Boolean = language == Language.VERILOG || language == Language.SYSTEM_VERILOG
  }

  object tracing_off extends AttributeFlag("verilator tracing_off", COMMENT_ATTRIBUTE){
    override def isLanguageReady(language: Language) : Boolean = language == Language.VERILOG || language == Language.SYSTEM_VERILOG
  }    

  object tracing_on extends AttributeFlag("verilator tracing_on", COMMENT_ATTRIBUTE){
    override def isLanguageReady(language: Language) : Boolean = language == Language.VERILOG || language == Language.SYSTEM_VERILOG
  } 
}

