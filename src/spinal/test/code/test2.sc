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


object B{
  object C{
    new Throwable().getStackTrace
  }
}

class A(val i : Int){
  def >>(that : A): A ={
    println(s"$i >> ${that.i}")
    that
  }
}

def one = new A(1)
def two = new A(2)
def tree = new A(3)

one >> two >> tree



object O{
  def apply = 2
}



//
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

/*
val myList = List[String]("a")

m[Int](myList)

import scala.reflect.runtime.universe._
def m[T : TypeTag] (par : List[String]): Unit = {
  par match{
    case a : List[T] if T =:= typeOf[String]=> {
      println("is List[T]")
    }
    case _ => println("no match")
  }
}


*/