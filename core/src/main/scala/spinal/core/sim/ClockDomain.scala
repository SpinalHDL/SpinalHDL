/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core.sim

import spinal.core.{Bool, ClockDomain, EdgeKind, HIGH, LOW, Polarity, TimeNumber}
import spinal.core.sim._
import spinal.sim.{SimCallSchedule}

/**
  * Execute a reset sequence
  */
object DoReset {

  def apply(reset: Bool, duration: Long, activeLevel: Polarity): Unit = {

    reset #= (activeLevel match {
      case HIGH => true
      case LOW => false
    })

    sleep(duration)

    reset #= (activeLevel match {
      case HIGH => false
      case LOW  => true
    })
  }

}

/**
  * Generate a clock
  */
object DoClock {
  def apply(clk: Bool, period: Long): Unit = {
    assert(period >= 2)

    var value = clk.toBoolean
    val clkProxy = clk.simProxy()

    def t: Unit = {
      value = !value
      clkProxy #= value
      delayed(period >> 1)(t)
    }

    t
  }
}

/**
  * Fork the DoClock
  */
object ForkClock {
  def apply(clk: Bool, period: Long): Unit = fork(DoClock(clk, period))
}


/**
  * Print the simulation speed
  */
object SimSpeedPrinter {

  def apply(cd: ClockDomain, printPeriod: Double): Unit = {
    var cycleCounter = 0l
    var lastTime = System.nanoTime()

    cd.onActiveEdges{
      cycleCounter += 1

      if((cycleCounter & 8191) == 0){
        val currentTime = System.nanoTime()
        val deltaTime = (currentTime - lastTime)*1e-9
        if(deltaTime > printPeriod) {
          println(f"[Info] Simulation speed : ${cycleCounter / deltaTime * 1e-3}%4.0f kcycles/s")
          lastTime = currentTime
          cycleCounter = 0
        }
      }
    }
  }
}


/**
  * Create a Timeout for the simulation
  */
object SimTimeout {

  def apply(duration: Long): Unit = delayed(duration) {
    simFailure(s"Timeout trigger after $duration units of time")
  }

  def apply(duration: TimeNumber): Unit = delayed(duration) {
    simFailure(s"Timeout trigger after $duration units of time")
  }
}

object SimStatics{
  val onSamplings = new Object
}
