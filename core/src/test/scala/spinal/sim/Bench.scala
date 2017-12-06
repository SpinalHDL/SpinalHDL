package spinal.sim

object Bench {
  def apply(testbench: => Unit): Unit = {
    var retry = 0
    while (retry < 2) {
      val startAt = System.nanoTime
      testbench
      val endAt = System.nanoTime
      System.out.println((endAt - startAt) * 1e-6 + " ms")
      retry += 1;
    }
  }
}
