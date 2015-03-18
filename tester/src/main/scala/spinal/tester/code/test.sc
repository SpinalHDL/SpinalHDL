import java.util

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

//a = b =>
//
//val c = cloneOf(b)
//c := b
//return c


class MyClass(i: Int) {
  def +(j: Int) = new MyClass(j + i)
  def -(j: Int) = new MyClass(i - j)
  def ^(j: Int) = new MyClass(j)
  def +|(j: Int) = new MyClass(j + i / 3)
  def \(a : Int) = new MyClass(a)
}


var c = new MyClass(1)
c \= 6

class Bar {
  def foo = 4
  def foo_=(x: Int) = {
    println(x)
  }
}

val b = new Bar
b.foo = 2

class A {
  def aa: A = {
    println("A")
    this
  }
  def aa_=(i: Int): A = {
    println("B " + i)
    new A
  }

  //  def apply_= (i : Int) : A = {
  //    println("B " + i)
  //    this
  //  }

  def update(value: Int): Unit = {
    println(value)
  }
}

val a = new A

a.aa = 2



scala.collection.mutable.HashMap