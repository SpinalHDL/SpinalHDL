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

abstract class Data[T]{
  def :=[S <: T](that: Data[S])
}
class Bits extends Data[Bits] {
  override def :=[S <: Bits](that: Data[S]): Unit = println(s"$this := $that")
}
class UInt extends Data[UInt] {
  override def :=[S <: UInt](that: Data[S]): Unit = println(s"$this := $that")
}
class Bundle extends Data[Bundle] {
  override def :=[S <: Bundle](that: Data[S]): Unit = println(s"$this := $that")
}
class MyBundle extends Bundle {
}
class Flow[T](val gen: Data[T]) extends Bundle{
  def <<(that: Flow[T]) = {
    this.gen := that.gen
  }
}
val b1 = new Flow(new MyBundle)
val b2 = new Flow(new Bundle)
b1 << b2
b1 := b2
val f1 = new Flow(new Bits)
val f2 = new Flow(new Bits)
f1 << f2
