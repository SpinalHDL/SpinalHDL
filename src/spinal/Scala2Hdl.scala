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

import scala.collection.mutable

object SpinalMain {
  def apply[T <: Component](gen : => T): T = {

    val backend = new VhdlBackend()


    val topLevel = backend.elaborate(() => gen)
    return topLevel;
  }
}

class Scala2Hdl[T <: Component](topLevel: T) {

/*
  var components = mutable.MutableList[Component]()
  addComponent(topLevel)
  components = components.sortWith(_.level > _.level)
  if(topLevel.instanceName == null) topLevel.instanceName = "toplevel"
  components.foreach(_.nameKinds())
  components.foreach(c => println(c.name))



  def addComponent(c : Component): Unit ={
    components += c
    c.kinds.foreach(addComponent(_))
  }

*/




  def printHierarchy() {
    printHierarchy(topLevel)
  }
  def printHierarchy(c: Component) {
    println(" ".*(c.level) + c.getName())
    for (kind <- c.kinds) {
      printHierarchy(kind)
    }
  }
}


