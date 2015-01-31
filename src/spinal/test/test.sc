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




import spinal._
import spinal.IntBuilder._

import scala.collection.mutable


//class SpinalEnum2 extends Enumeration {
//  implicit def valueToCraft(x: Value): SpinalEnumCraft[this.type] = {
//    val ret = craft()
//    ret.inputs(0) = new EnumLiteral(this, x.id)
//    ret
//  }
//  def getWidth = log2Up(values.foldLeft(0)((v, n) => Math.max(v, n.id)) + 1)
//  def craft(): SpinalEnumCraft[this.type] = new SpinalEnumCraft[this.type](this)
//
//  type SpinalEnum = Val
//
//  abstract class Value extends super.Value{
//    def caca = 2
//  }
//
//  class Val(i: Int, name: String) extends Val(i, name) {
//    def ===(that: this.type) = "hallo3"
//
//  }
//}
//object WeekDay3 extends SpinalEnum2 {
//  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
//  Mon.caca
//}









object WeekDay extends SpinalEnum {
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}

object WeekDay2 extends SpinalEnum {

 // val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value

}

object MyEnum extends SpinalEnum {
  val e0 = 0;
  val e1 = 1;
  val e2 = 3;
}

//val s_idle :: s_5 :: s_10 :: s_15 :: s_ok :: Nil = SpinalEnum(6)

/*object IntBuilder {
  implicit def EnumToCraft(that : WeekDay2.V) = new IntBuilder(value)
}*/
ClockDomain.push(ClockDomain(null))

println("a" + WeekDay)
val myCraft = WeekDay.craft()
println("b")
myCraft := WeekDay.Fri
println("c")
val r0 = RegNext(WeekDay.craft())
println("d")
val str = WeekDay.Fri == r0
str
println("e")
println("str :" + str)
