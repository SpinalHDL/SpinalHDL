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

package spinal

import java.lang.reflect.{Method}

import scala.collection.mutable
import scala.collection.mutable.Stack

/**
 * Created by PIC18F on 08.01.2015.
 */


import scala.reflect.runtime.universe._

class utils extends scala.annotation.StaticAnnotation

import reflect.runtime.{universe => ru}

object Misc {
  def reflect(o: Object, onEach: (String, Object) => Unit): Unit = {

   /* val fieldSymbols = ru.typeOf[o.type].members.collect { case m: ru.Constant => m}
    val mmm = ru.typeOf[o.type].paramLists
    val mmm2 = ru.typeOf[o.type]
    val tt = o.getClass.getFields
    val ttt = o.getClass.getDeclaredFields
    val tttt = o.getClass.getSuperclass
    val ttttt = o.getClass.getSuperclass.getDeclaredFields
    val tttttt = o.getClass.getSuperclass.getFields
    ttt(0).setAccessible(true)
    val r = ttt(0).get(o)*/
   /* for (method <- o.getClass.getMethods) {
      if (method.getParameterTypes.length == 0
        && Modifier.isPublic(method.getModifiers)) {
        val tt = method.getAnnotations
        val obj = method.invoke(o)
        if (obj != null) {
          onEach(method.getName, obj)
        }
      }
    }*/

    val refs = mutable.Set[Object]()
    explore(o.getClass)
    def explore(c : Class[_]): Unit ={
      if(c == null) return
      explore(c.getSuperclass)
      for(f <- c.getDeclaredFields){
        f.setAccessible(true)
        val fieldRef = f.get(o)
        if(!refs.contains(fieldRef)){
          onEach(f.getName, fieldRef)
          refs += fieldRef
        }

      }

    }

  }

  def resize(to: Node, inputId: Integer, width: Int) {
    val input = to.inputs(inputId)
    if (input == null || input.getWidth == width) return;

    val that = input.asInstanceOf[BitVector]
    Component.push(that.component)
    to.inputs(inputId) = that.resize(width)
    Component.pop(that.component)
  }


}


class Scope {
  val map = mutable.Map[String, Int]()


  def allocateName(name: String): String = {
    val count = map.get(name).getOrElse(0)
    map(name) = count + 1
    if (count == 0) name else name + "_" + count
  }

  def lockName(name : String): Unit ={
    val count = map.get(name).getOrElse(1)
    map(name) = count
  }
}

/*
class UniqueNameAllocator {
  val map = mutable.Map[String, Int]()


  def get(obj: Object): String = {
    val clazz = obj.getClass()
    val name = clazz.getSimpleName
    val count = map.get(name).getOrElse(0)
    map(name) = count + 1
    if (count == 0) clazz.getSimpleName else clazz.getSimpleName + "_" + count
  }


}*/

class SafeStack[T] {
  val stack = new Stack[T]()

  def push(e: T): Unit = {
    stack.push(e)
  }

  def pop(e: T): Unit = {
    if (stack.head != e)
      throw new Exception("Stack pop fail")
    stack.pop()
  }

  def head() = stack.headOption.getOrElse(null.asInstanceOf[T])
  def oldest() = stack.lastOption.getOrElse(null.asInstanceOf[T])

  def size() = stack.size
  def reset = stack.clear()
}


