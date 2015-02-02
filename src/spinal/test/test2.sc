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

object Chm {

  abstract class Data[T <: MyType] {
    def :=[S <: T](that: Data[S]): Unit

  }

  trait MyType

  class BitType extends MyType

  class IntType extends MyType

  class Bits extends Data[BitType] {
    override def :=[T <: BitType](that: Data[T]): Unit = println(s"$this := $that")
  }

  class Int extends Data[IntType] {
    override def :=[T <: IntType](that: Data[T]): Unit = println(s"$this := $that")
  }


  val a = new Bits()
  val b = new Int()

  class Flow[T <: MyType](val gen: Data[T]) {
    def <<(that: Flow[T]) = {
      this.gen := that.gen
    }
  }

  val f1 = new Flow(new Bits)
  val f2 = new Flow(new Bits)
  f1 << f2
  new Bits() := new Bits
}

/*abstract class Data[T] {
  def :=(that: Data[T])
}

class Bits extends Data[Bits] {
  override def :=(that: Data[Bits]): Unit = println(s"$this := $that")
}

class Flow[T <: Data](val gen: T) {
  def <<(that: Flow[T]) = {
    this.gen := that.gen
  }
}*/

//
//abstract class Data {
//  def :=(that: this.type)
//}
//
//class Bits extends Data {
//  override def :=(that: this.type): Unit = println(s"$this := $that")
//}
//
//class Flow[T <: Data](val gen: T) {
//  def <<(that: Flow[T]) = {
//    this.gen := that.asInstanceOf[Flow[T]].gen
//  }
//}


//
//abstract class Data[T <: MyType] {
//  def :=[S <: T](that: Data[S]): Unit
//
//}
//
//trait MyType
//class BitType extends MyType
//class IntType extends MyType
//class Bits extends Data[BitType] {
//  override def :=[T <: BitType](that: Data[T]): Unit = println(s"$this := $that")
//}
//
//class Int extends Data[IntType] {
//  override def :=[T <: IntType](that: Data[T]): Unit = println(s"$this := $that")
//}
//
//
//val a = new Bits()
//val b = new Int()
//
//a := b


/*
def a[T <: Data[_]](d: T): T = {
  d
}*/


//val b1, b2 = new Bits
///*b1 := a(b2)*/
////object obj{
////  val f1,f2 = new Flow(new Bits)
////}
//val f1,f2 = new Flow(new Bits)
//f1 << f2
"done"
// override def clone(): this.type = ???
//  override def clone(): this.type = new Bits().asInstanceOf[this.type]
// val bits = gen.clone()

//class Stack[A] {
//  def push[B >: A](elem: B): Stack[B] = new Stack[B] {
//    override def top: B = elem
//    override def pop: Stack[B] = Stack.this
//    override def toString() = elem.toString() + " " +
//      Stack.this.toString()
//  }
//  def top: A = sys.error("no element on stack")
//  def pop: Stack[A] = sys.error("no element on stack")
//  override def toString() = ""
//}
//object VariancesTest extends App {
//  var s: Stack[Any] = new Stack().push("hello");
//  s = s.push(new Object())
//  s = s.push(7)
//  println(s)
//}