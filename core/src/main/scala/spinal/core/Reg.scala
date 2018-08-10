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


/**
  * Create a register
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


/**
  * Register a signal of one clock
  */
object RegNext {
  def apply[T <: Data](next: T, init: T = null.asInstanceOf[T]): T = Reg(next, init,next).setCompositeName(next, "regNext", true)
}


/**
  * Register a signal when a condition is true
  */
object RegNextWhen {
  def apply[T <: Data](next: T, cond: Bool, init: T = null.asInstanceOf[T]): T = {
    val reg = Reg(next, init).setCompositeName(next, "regNextWhen", true)
    when(cond){
      reg := next
    }
    reg
  }
}


/**
  * Declare a register with an initialize value
  */
object RegInit {

  def apply[T <: Data](init: T): T = Reg(init, init)

  def apply[T <: SpinalEnum](init : SpinalEnumElement[T]) : SpinalEnumCraft[T] = apply(init())
}
