import scala.runtime.Nothing$

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


BigInt(15) &~ BigInt(2)
BigInt(15) &~ BigInt(32)
BigInt(15) & BigInt(2)


abstract class Helper extends DelayedInit {
  def delayedInit(body: => Unit) = {
    println("a")
    body // evaluates the initialization code of C
    println("b")
  }
}

class C extends Helper {
  println("Code")
}
val c = new C
trait OnCreate extends DelayedInit {
  def onCreate:Unit
  def delayedInit(body: => Unit) = {
    body

//    if ((body _).getClass.getDeclaringClass == this.getClass)
//    {
      onCreate
//    }
  }
}

class A extends DelayedInit {
  def delayedInit(body: => Unit) = {
    body
    //    if ((body _).getClass.getDeclaringClass == this.getClass)
    //    {
    onCreate
    //    }
  }
  def onCreate = println("Creation is fully complete")

  println("a complete")
}

class B extends A {

  println("b complete")
}


new B