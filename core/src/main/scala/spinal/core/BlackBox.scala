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

/**
 * Created by PIC18F on 24.01.2015.
 */


class Generic {
  var flattenCache: ArrayBuffer[Any] = null


  def genNames: Unit ={
    Misc.reflect(this, (name, obj) => {
      obj match{
        case obj : Nameable =>
          obj.setWeakName(name)
        case _ =>
      }
    })
  }

  def flatten = {
    if (flattenCache == null) {
      flattenCache = ArrayBuffer[Any]()
      Misc.reflect(this, (name, obj) => {
        obj match{
          case obj : Data =>
            flattenCache ++= obj.flatten
          case _ =>
            flattenCache += Tuple2(name, obj)
        }
      })
    }
    flattenCache
  }
}

object uLogic extends SpinalTag


abstract class BlackBox extends Component with SpinalTagReady {


  //def generic: Generic// = new Generic{}
  def getGeneric : Generic = {
    try{
      val clazz = this.getClass
      val m = clazz.getMethod("generic")
      val generic = m.invoke(this).asInstanceOf[Generic]
      return generic
    } catch{
      case _ : Throwable => new Generic
    }
  }

  //
  def mapClockDomain(clockDomain: ClockDomain = ClockDomain.current, clock: Bool = null, reset: Bool = null, enable: Bool = null): Unit = {
    Component.push(parent)
    if (clockDomain.hasClockEnable && enable == null) SpinalError(s"Clock domain has clock enable, but blackbox is not compatible $this")
    if (enable != null) {
      pulledDataCache += (clockDomain.clockEnable -> enable)
      enable := ClockDomain.current.readClockEnableWire
    }

    if (reset != null) {
      if (!clockDomain.hasReset) SpinalError(s"Clock domain has no reset, but blackbox need it $this")
      pulledDataCache += (clockDomain.reset -> reset)
      reset := ClockDomain.current.readResetWire
    }
    pulledDataCache += (clockDomain.clock -> clock)
    clock := ClockDomain.current.readClockWire

    Component.pop(parent)
  }


  def mapCurrentClockDomain(clock: Bool, reset: Bool = null, enable: Bool = null): Unit = {
    mapClockDomain(ClockDomain.current, clock, reset, enable)
  }


  override def nameElements(): Unit = {
    val io = reflectIo
    if(io != null){
      io.setWeakName("")
    }
    super.nameElements()
  }

  override def isInBlackBoxTree: Boolean = true

  def setBlackBoxName(name : String) : this.type ={
    this.definitionName = name
    this
  }

  def isUsingULogic = this.hasTag(uLogic)
  def remplaceStdLogicByStdULogic = this.addTag(uLogic)
}


abstract class BlackBoxULogic extends BlackBox{
  remplaceStdLogicByStdULogic
}