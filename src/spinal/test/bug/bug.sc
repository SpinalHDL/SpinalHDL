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

class A;
object dsl {
  object A { //If you move this object outside dsl object all is ok
  def apply(): A = new A()
    def apply(value: Boolean): A = new A() //If you remove this apply def all is ok
  }
}
var myA = dsl.A ()   //If you do dsl.A() all is ok
myA = new A   //myA is interpretted by the scala plugin as Nothing type instead of A