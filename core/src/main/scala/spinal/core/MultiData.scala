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

package spinal.core

import scala.collection.mutable.ArrayBuffer

object MultiData{
 // var tab = 0
}
abstract class MultiData extends Data with DelayedInit {
  globalData.dataStack.push(this)
  //println("  " * MultiData.tab + "push")
 // MultiData.tab += 1
  override def delayedInit(body: => Unit) = {
    body

    if ((body _).getClass.getDeclaringClass == this.getClass) {
      //println("  " * MultiData.tab + "-")
      //if (globalData.dataStack.head() == this) {
      if(globalData.dataStack.head() != this){
        SpinalWarning(
          """
            |*** One of your bundle as a empty body.
            |*** It's not allowed for the moment (in scala 2.12 it will be)
            |*** Please, put a dummy field like "val dummy = 0" into the empty bundle """.stripMargin + globalData.dataStack.head().getScalaLocationString)
      }

      globalData.dataStack.pop(this)
        //MultiData.tab -= 1
        //println("  " * MultiData.tab + "pop")
      //}
      //else
       // println("????")
    }
  }


  def elements: ArrayBuffer[(String, Data)]


  override def addTag(spinalTag: SpinalTag): this.type = {
    super.addTag(spinalTag)
    elements.foreach(_._2.addTag(spinalTag))
    this
  }

  def find(name: String): Data = {
    val temp = elements.find((tuple) => tuple._1 == name).getOrElse(null)
    if (temp == null) return null
    temp._2
  }

  override def toBits: Bits = {
    var ret: Bits = null
    for ((eName, e) <- elements) {
      if (ret == null.asInstanceOf[Object]) ret = e.toBits
      else ret = e.toBits ## ret
    }
    if (ret.asInstanceOf[Object] == null) ret = Bits(0 bit)
    ret
  }

  override def nameChangeEvent(weak: Boolean): Unit = {
    super.nameChangeEvent(weak)
    for ((eName, e) <- elements) e match {
      case nameable: Nameable => nameable.setName(getName() + "_" + eName, weak)
    }
  }

  override def getBitsWidth: Int = {
    var accumulateWidth = 0
    for (e <- flatten) {
      val width = e.getWidth
      if (width == -1) return -1
      accumulateWidth += width
    }
    accumulateWidth
  }

  override def asInput: this.type = {
    elements.foreach(_._2.asInput);
    this
  }


  override def asOutput: this.type = {
    elements.foreach(_._2.asOutput);
    this
  }


  override def flatten: Seq[BaseType] = {
    elements.map(_._2.flatten).foldLeft(List[BaseType]())(_ ++ _)
  }


  override def assignFromBits(bits: Bits): Unit = {
    var offset = 0
    for (e <- flatten) {
      val width = e.getWidth
      e.assignFromBits(bits(offset, width bit))
      offset = offset + width
    }
  }
}
