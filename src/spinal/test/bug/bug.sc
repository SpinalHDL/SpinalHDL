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

//class A;
//object dsl {
//  object A { //If you move this object outside dsl object all is ok
//  def apply(): A = new A()
//    def apply(value: Boolean): A = new A() //If you remove this apply def all is ok
//  }
//}
//var myA = dsl.A ()   //If you do dsl.A() all is ok
//myA = new A   //myA is interpretted by the scala plugin as Nothing type instead of A
//abstract class Data {
//  type Self <: Data
//  def :=(that: Self)
//}
//class Flow[T <: Data](val gen: T){
//  def <<(that: Flow[gen.Self]) = {
//    this.gen := that.gen         //that.gen  is marked as read (type mismatch, expected Flow.this.gen.Self, actual that.gen.Self
//  }
//}




abstract class Data[T]{
  def :=(that: Data[T])= println(s"$this := $that")
  //def :=[S <: T](that: Data[S]) = println(s"$this := $that")
}
class Bits extends Data[Bits] {
 /// override def :=[S <: Bits](that: Data[S]): Unit = println(s"$this := $that")
}
class UInt extends Data[UInt] {
 // override def :=[S <: UInt](that: Data[S]): Unit = println(s"$this := $that")
}
class Bundle extends Data[Bundle] {
 // override def :=[S <: Bundle](that: Data[S]): Unit = println(s"$this := $that")
}
class MyBundle extends Bundle {
}
class Flow[T](val gen: Data[T]) extends Bundle{
  def <<(that: Flow[T]) = {
    this.gen := that.gen
  }
}

object DoIt{
  def doIt[S <: Data](that : S) : Unit = {
    that := that
    //  that := that
  }
}
val b1 = new Flow(new MyBundle)
val b2 = new Flow(new Bundle)
b1 << b2
b1 := b2
val f1 = new Flow(new Bits)
val f2 = new Flow(new Bits)
f1 << f2