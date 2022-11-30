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

/*
object tt1 {

  class A {

  }

  class B {

  }

  implicit def trololol(that: A) = new B


  val t1: B = new A
  ///implicit def trololol2[T <: Data](that : T) = that.autoCast
}

object tt2 {

  class A {
    type Self <: A
    def doIt(that: Self): Unit = println("doit")
  }

  class B extends A {
    override type Self = B
  }

  implicit def autoCast[T <: A](that: T): T#Self = that.asInstanceOf[T#Self]

  def f1[T <: A](that: T): Unit = {
    val t1: T#Self = that

    that.doIt(that)
  }
  f1(new B)

  //val t1 : B = new B
  ///implicit def trololol2[T <: Data](that : T) = that.autoCast
}*/



object Data {
  implicit def autoCast[T <: Data, T2 <: T](that: T): T2#SSelf = that.asInstanceOf[T2#SSelf]
}

trait Data {
  type SSelf <: Data
  def :=(that: SSelf): Unit = this.assignFrom(that)
  def assignFrom(that: Data): Unit = ???
  def autoConnect(that: Data): Unit = ???
}

class Bits extends Data {
  override type SSelf = Bits
  override def :=(that: SSelf): Unit = ???
  override def autoConnect(that: Data): Unit = {
    this := that
  }
  final override def assignFrom(that: Data): Unit = println(s"$this := $that")
}
class UInt extends Data {
  override type SSelf = UInt
  override def :=(that: SSelf): Unit = ???
}
object Something {
  def doIt[T2 <: Data](that: T2): Unit = {
    that := that
  }
}

class Flow[T2 <: Data](val gen: T2) {
  def <<(that: Flow[gen.SSelf]) = {
    this.gen := that.gen
  }
}


//Some stupide exemples of usage
//t.Self
val b1 = new Bits
val b2 = new Bits
val u1 = new UInt
val u2 = new UInt

"***"
b1 := u1
"***"
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
