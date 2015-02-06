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

object impl {
  // implicit def trololol[T <: Data](that : T) : that.Self = that.asInstanceOf[that.Self]
  // implicit def trololol2(that : Data)  : that.Self = that.autoCast
  val t: Data = new Bits()
  // val r : t.Self = (t)
}

import impl._
import spinal.test.Try.S

object Data {
  //implicit def trololol(that : Data) : Int = 2
  ///implicit def trololol2[T <: Data](that : T) = that.autoCast
}

import Data._

class Data {
  type Self <: Data
  //def :=(that: Self): Unit = println(s"$this := $that")
  def :=[T2 <: Data#Self](that: T2): Unit = println(s"$this := $that")
}
class Bits extends Data {
  override type Self = Bits
 // override def :=(that: Self): Unit = ???
}
class UInt extends Data {
  override type Self = UInt
//  override def :=(that: Self): Unit = ???
}
object Something {
  implicit def trololol2[T3 <: Data](that : T3) = that.asInstanceOf[that.Self]
  def doIt[T2 <: Data](that: T2): Unit = {
    that := that
  }
}


class Flow[T2 <: Data](val gen: T2) {
  def <<(that: Flow[gen.Self]) = {
  //  this.gen := that.gen
  }
}
//Some stupide exemples of usage
//t.Self
val b1 = new Bits
val b2 = new Bits
val u1 = new UInt
val u2 = new Bits
Something.doIt(b1)
new Bits() := new Bits()
val f1 = new Flow(new Bits)
val f2 = new Flow(new Bits)
f1 << f2

//abstract class Data {
//  type Self <: Data
//  def :=(that: Self)
//}
//
//class Bits extends Data {
//  type Self = Bits
//  override def :=(that: Self): Unit = println(s"$this := $that")
//}
//class UInt extends Data{
//  type Self = UInt
//  override def :=(that: Self): Unit = println(s"$this := $that")
//}
//
//class Flow[T <: Data](gen: T){
//  def <<(that: Flow[T]) = {
//    this.gen := that.gen
//  }
//}
//
//new Bits() := new Bits()
//val f1,f2 = new Flow(new Bits)
//f1 << f2
//class A;
//class B extends A;
//
//class Tricky[T] {
//  def trickyMethod[S1<:T](s1:S1)(implicit ev: S1=:=T) = println()
//}
//
//
//val t = new Tricky[B]
//t.trickyMethod(new A)
//t.trickyMethod(new B)


//abstract class Fruit[T <: Fruit[T]] {
//  def trick(that: T)
//}
//
//class Apple extends Fruit[Apple] {
//  override def trick(that: Apple) = {
//    println("Apple")
//  }
//}
//
//class Banana extends Fruit[Banana] {
//  override def trick(that: Banana) = {
//    println("Banana")
//  }
//}
//
//class TrickyClass {
//  def trickyMethod[T <: Fruit[T]](p1: T, p2: T) = p1.trick(p2)
//}
//
//
//val tricky = new TrickyClass()
//tricky.trickyMethod(new Apple, new Apple) //this should be OK
//tricky.trickyMethod(new Banana, new Banana) //this should be OK
////tricky.trickyMethod(new Banana, new Apple) //this should NOT compile
