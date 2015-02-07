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
