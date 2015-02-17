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


import scala.collection.mutable.ArrayBuffer


import importMe._

abstract class MultiData extends Data {

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
      else ret = ret ## e.toBits
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
    for ((eName, e) <- flatten) {
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


  override def flatten: ArrayBuffer[(String, BaseType)] = {
    val ret = ArrayBuffer[(String, BaseType)]()
    for ((name, data) <- elements) {
      ret ++= data.flatten
    }
    ret
  }


  override def fromBits(bits: Bits): Unit = {
    var offset = 0
    for ((n, e) <- flatten.reverse) {
      val width = e.getWidth
      e.fromBits(bits(offset, width bit))
      offset = offset + width
    }
  }
}
