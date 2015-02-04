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

import java.lang.reflect.Method

import scala.collection.mutable
import scala.collection.mutable.Stack

/**
 * Created by PIC18F on 08.01.2015.
 */


import scala.reflect.runtime.universe._

class utils extends scala.annotation.StaticAnnotation

import reflect.runtime.{universe => ru}

object Misc {

  def addReflectionExclusion(o: Object) = reflectExclusion += o.getClass
  val reflectExclusion = mutable.Set[Class[_]]()


  addReflectionExclusion(new Bundle())
  addReflectionExclusion(new Vec(null))
  addReflectionExclusion(new Bool)
  addReflectionExclusion(new Bits)
  addReflectionExclusion(new UInt)
  addReflectionExclusion(new SInt)
  addReflectionExclusion(new SpinalEnum)
  addReflectionExclusion(new SpinalEnumCraft(null))


  //  def reflect(o: Object, onEach: (String, Object) => Unit): Unit = {
  //    val refs = mutable.Set[Object]()
  //    explore(o.getClass)
  //    def explore(c: Class[_]): Unit = {
  //      if (c == null) return
  //      if (reflectExclusion.contains(c) || c.getName + "$" == Component.getClass.getName)
  //        return
  //      explore(c.getSuperclass)
  //      for (f <- c.getDeclaredFields) {
  //        f.setAccessible(true)
  //        val fieldRef = f.get(o)
  //        if (!refs.contains(fieldRef)) {
  //          fieldRef match {
  //            case vec: Vec[_] =>
  //            case seq: Seq[_] => {
  //              for ((obj, i) <- seq.zipWithIndex) {
  //                onEach(f.getName + i, obj.asInstanceOf[Object])
  //                refs += fieldRef
  //              }
  //            }
  //            case _ =>
  //          }
  //          onEach(f.getName, fieldRef)
  //          refs += fieldRef
  //        }
  //      }
  //    }
  //  }
  def reflect(o: Object, onEach: (String, Object) => Unit): Unit = {
    val refs = mutable.Set[Object]()
    explore(o.getClass)
    def explore(c: Class[_]): Unit = {
      if (c == null) return
      if (reflectExclusion.contains(c) || c.getName + "$" == Component.getClass.getName)
        return
      explore(c.getSuperclass)

      val fields = c.getDeclaredFields
      def isValDef(m: Method) = fields exists (fd => fd.getName == m.getName && fd.getType == m.getReturnType)
      val methods = c.getMethods filter (m => m.getParameterTypes.isEmpty && isValDef(m))

      for(method <- methods){
        method.setAccessible(true)
        val fieldRef = method.invoke(o)
        if (!refs.contains(fieldRef)) {
          fieldRef match {
            case vec: Vec[_] =>
            case seq: Seq[_] => {
              for ((obj, i) <- seq.zipWithIndex) {
                onEach(method.getName + i, obj.asInstanceOf[Object])
                refs += fieldRef
              }
            }
            case _ =>
          }
          onEach(method.getName, fieldRef)
          refs += fieldRef
        }
       }
    }
  }

  def normalizeResize(to: Node, inputId: Integer, width: Int) {
    val input = to.inputs(inputId)
    if (input == null || input.getWidth == width) return;

    val that = input.asInstanceOf[BitVector]
    Component.push(that.component)
    val resize = that.resize(width)
    resize.inferredWidth = width
    to.inputs(inputId) = resize
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

  def lockName(name: String): Unit = {
    val count = map.get(name).getOrElse(1)
    map(name) = count
  }

  def iWantIt(name: String): Unit = {
    if (map.contains(name)) SpinalError("Reserved name $name is not free")
    map(name) = 1
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
  def isEmpty: Boolean = stack.isEmpty
  def size() = stack.size
  def reset = stack.clear()
}

object SpinalExit {
  def apply(message: String = "Unspecified exit"): Unit = {
    throw new SpinalExit("\n" + message)
  }
}


object SpinalInfoPhase {
  def apply(message: String) = println(s"[Progress] at ${f"${Driver.executionTime}%1.3f"} : $message")
}

object SpinalInfo {
  def apply(message: String) = println(s"[Info] $message")
}

class SpinalExit(message: String) extends Exception(message);

//class SpinalCantInferredWidth extends Exception;

/*
object SpinalCantInferredWidth{
  def apply(node : Node): Unit = {
    SpinalError.printError(s"Can't infer width on $node because of unspecified width")
    throw new SpinalCantInferredWidth
  }
}*/


object SpinalError {
  def apply() = {
    SpinalExit()
  }

  def apply(message: String) = {
    SpinalExit(message)
  }
  def apply(messages: Seq[String]) = {
    SpinalExit(messages.reduceLeft(_ + "\n" + _))
  }

  def printError(message: String) = println(s"[Error] $message")
}