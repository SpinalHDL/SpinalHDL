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

import spinal.idslplugin.Location


/** Declare a register with a signal type.
  * @example {{{
  * val reg1 = Reg(UInt(4 bits))
  * }}}
  * @see [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Sequential%20logic/registers.html#instantiation Register documentation]]
  */
object Reg {

  def apply[T <: Data](dataType: HardType[T], init: T = null.asInstanceOf[T], next : T = null.asInstanceOf[T]): T = {
    val regOut = cloneOf(dataType)

    for ( e <- regOut.flatten) {
      e.setAsReg()
    }

    if (init != null) regOut.init(init)
    if (next != null) regOut := next
    regOut
  }

  def apply[T <: SpinalEnum](enumType: T): enumType.C = Reg(enumType())
}


/** Register a signal by one clock.
  * 
  * @example {{{
  * val reg1 = Reg(UInt(4 bits))
  * // Register that updates itself every cycle with a sample of reg1 incremented by 1
  *  val reg2 = RegNext(reg1 + 1)
  * }}}
  * @see [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Sequential%20logic/registers.html#instantiation Register documentation]]
  */
object RegNext {
  def apply[T <: Data](next: T, init: T = null.asInstanceOf[T]): T = Reg(next, init,next).setCompositeName(next, "regNext", true)
}


/** Register a signal when a condition is true.
  * @see [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Sequential%20logic/registers.html#instantiation Register documentation]]
  */
object RegNextWhen {
  def apply[T <: Data](next: T, cond: Bool, init: T = null.asInstanceOf[T])(implicit loc: Location): T = {
    val reg = Reg(next, init).setCompositeName(next, "regNextWhen", true)
    when(cond){
      reg := next
    }
    reg
  }
}


/** Declare a register with an initialize value.
  * 
  * @example {{{
  * // UInt register of 4 bits initialized with 0 when the reset occurs
  * val reg = RegInit(U"0000")
  * }}}
  * @see [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Sequential%20logic/registers.html#instantiation Register documentation]]
  */
object RegInit {

  def apply[T <: Data](init: T): T = Reg(init, init)

  def apply[T <: SpinalEnum](init : SpinalEnumElement[T]) : SpinalEnumCraft[T] = apply(init())
}
