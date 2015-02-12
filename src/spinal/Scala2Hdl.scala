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

object SpinalVhdl{
  def apply[T <: Component](gen : => T) = {
    new SpinalVhdl(gen)
  }
}

class SpinalVhdl[T <: Component](gen : => T){
  val backend = new VhdlBackend
  val tbGen = new VhdlTestBenchBackend()
  def elaborate ={
    val report = backend.elaborate(() => gen)
    tbGen.elaborate(backend,report.topLevel)
    report
  }


  def setLibrary(name : String): this.type ={
    backend.library = name
    this
  }
  def setSpinalPackage(name : String): this.type ={
    backend.packageName = name
    this
  }
  def setEnumPackage(name : String): this.type ={
    backend.enumPackageName = name
    this
  }
  def setOutputFile(name : String): this.type ={
    backend.outputFile = name
    this
  }
  def setTbOutputFile(name : String): this.type ={
    tbGen.outputFile = name
    this
  }
  def setTbName(name : String): this.type ={
    tbGen.tbName = name
    this
  }
}


class SpinalVhdlTb[T <: Component](gen : => T) {

}

object SpinalMain {
  def apply[T <: Component](gen : => T): BackendReport[T] = {

    val backend = new VhdlBackend
    val topLevel = backend.elaborate(() => gen)

   /* for(backend <- backends){
      graphAdapter.reservedKeyWords ++= backend.getReservedKeyword
    }

    val topLevel = graphAdapter.elaborate(() => gen)
    for(backend <- backends){
      backend.elaborate(graphAdapter)
    }*/
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


