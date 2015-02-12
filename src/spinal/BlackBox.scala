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

/**
 * Created by PIC18F on 24.01.2015.
 */


abstract class BlackBox extends Component {
  def generic: Bundle

  def use(clockDomain: ClockDomain, clockIn: Bool, resetIn: Bool = null, clockEnableIn: Bool = null): Unit = {
    Component.push(parent)
    if (clockDomain.hasClockEnable && clockEnableIn == null) SpinalError(s"Clock domain has clock enable, but blackbox is not compatible $this")
    if (clockEnableIn != null) {
      pulledDataCache += (clockDomain.clockEnable -> clockEnableIn)
      clockEnableIn := ClockDomain.current.readClockEnable
    }

    if (resetIn != null) {
      if (!clockDomain.hasReset) SpinalError(s"Clock domain has no reset, but blackbox need it $this")
      pulledDataCache += (clockDomain.reset -> resetIn)
      resetIn := ClockDomain.current.readReset
    }
    pulledDataCache += (clockDomain.clock -> clockIn)
    clockIn := ClockDomain.current.readClock

    Component.pop(parent)
  }


  def useCurrentClockDomain(clockIn: Bool, resetIn: Bool = null, clockEnableIn: Bool = null): Unit = {
    use(ClockDomain.current, clockIn, resetIn, clockEnableIn)
  }

  override def isInBlackBoxTree: Boolean = true
}
