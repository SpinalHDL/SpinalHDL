package spinal.lib

import spinal.core._

object GrayCounter {
  def apply(width: Int, enable: Bool): UInt = {
    val gray = RegInit(U(0, width bit))
    val even = RegInit(True)
    val word = Cat(True, gray(width - 3 downto  0), even)
    when(enable) {
      var found = False
      for (i <- 0 until width) {
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
