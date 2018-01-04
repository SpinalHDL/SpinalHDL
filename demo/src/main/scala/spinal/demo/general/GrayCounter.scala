package spinal.demo.general
import spinal.core._

object GrayCounter {
  def apply(n: Int, enable: Bool): UInt = {
    val gray = RegInit(U(0, n bit))
    val even = RegInit(True)
    val word = Cat(True, gray(n-3 downto  0), even)
    when(enable) {
      var found = False
      for (i <- 0 until n) {
        when(word(i) && !found) {
          gray(i) := !gray(i)
          found \= True
        }
      }
      even := !even
    }
    return gray
  }
}
