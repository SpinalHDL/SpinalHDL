
import scala.collection.BitSet
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

val bitSet : BitSet = BitSet()

//bitSet.&()

object Twice {
  def apply(x: Int): Int = x * 2
  def unapply(z: Int): Option[Int] = if (z%2 == 0) Some(z/2) else None
}


val x = Twice(21)
x match { case Twice(n) => Console.println(n) } // prints 21



BigInt(10).bitCount

for(i <- 0 to  2){
  println(i)
}
for(i <- 0 to 4) println(i)
for(i <- 0 until 4) println(i)
class Test{
  val a = 1
  private val b = 2
}
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