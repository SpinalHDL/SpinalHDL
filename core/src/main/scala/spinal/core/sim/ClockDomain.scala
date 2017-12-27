package spinal.core.sim

import spinal.core.{Bool, ClockDomain, EdgeKind, HIGH, LOW, Polarity}
import spinal.core.sim._

object DoReset {
  def apply(reset : Bool, duration : Long, activeLevel: Polarity): Unit@suspendable = {
    reset #= (activeLevel match {
      case HIGH => true
      case LOW => false
    })
    sleep(duration)
    reset #= (activeLevel match {
      case HIGH => false
      case LOW => true
    })
  }
}

object DoClock {
  def apply(clk : Bool, period : Long): Unit@suspendable = {
    var value = clk.toBoolean
    while(true){
      value = !value
      clk #= value
      sleep(period >> 1)
    }
  }
}

object ForkClock {
  def apply(clk : Bool, period : Long): Unit = fork(DoClock(clk, period))
}


object SimSpeedPrinter{
  def apply(cd : ClockDomain, printPeriod : Double): Unit = fork{
    var cycleCounter = 0l
    var lastTime = System.nanoTime()
    while(true){
      cd.waitActiveEdge()
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