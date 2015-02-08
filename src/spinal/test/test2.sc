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




object ImportMe{
  implicit class BitCount(value : Int){
    def bit : BitCount = this
  }
}

import ImportMe._

def f1(value : BitCount) = {

}


f1(3 bit)
def getIndex[T, CC](seq: CC, value: T)(implicit conv: CC => Seq[T]) = seq.indexOf(value)

getIndex("abc", 'a')


object Obj1{
  import spinal._
  import spinal.IntBuilder._
  Bits(3 bit)
}
/*
val myList = List[String]("a")

m[Int](myList)

import scala.reflect.runtime.universe._
def m[T : TypeTag] (par : List[String]): Unit = {
  par match{
    case a : List[T] if T =:= typeOf[String]=> {
      println("is List[T]")
    }
    case _ => println("no match")
  }
}


*/