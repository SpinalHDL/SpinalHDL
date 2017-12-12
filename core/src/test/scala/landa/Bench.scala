package landa

import spinal.core.SimManagedApi.suspendable
;

object Bench {
  def apply(factor : Double)(testbench: => Unit): Unit = {
    var retry = 0
    while (retry < 3) {
      val startAt = System.nanoTime
      testbench
      val endAt = System.nanoTime
      System.out.println((endAt - startAt) * 1e-6 + " ms " + 1.0/((endAt - startAt) * 1e-9)*factor + " hz" )
      retry += 1;
    }
  }
}


object BenchSim {
  def apply(factor : Double)(testbench: => Unit@suspendable ): Unit@suspendable = {
    var retry = 0
    while (retry < 3) {
      val startAt = System.nanoTime
      testbench
      val endAt = System.nanoTime
      System.out.println((endAt - startAt) * 1e-6 + " ms " + 1.0/((endAt - startAt) * 1e-9)*factor + " hz" )
      retry += 1;
    }
  }
}
