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



abstract class Data {
  type Self <: Data
  def :=(that: Self):Unit = println(s"$this := $that")
}

class Bits extends Data {
  override type Self = Bits
 // override def :=(that: Self): Unit = println(s"$this := $that")
}

class UInt extends Data {
  override type Self = UInt
  //override def :=(that: Self): Unit = println(s"$this := $that")
}

class Flow[T <: Data](val gen: T){
  def <<(that: Flow[gen.type]) = {
    this.gen := that.gen  // <- There is a compilation error  : value gen is not a member of spinal.test.Try.Flow[T]
    //^
  }
}

//Some stupide exemples of usage
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
