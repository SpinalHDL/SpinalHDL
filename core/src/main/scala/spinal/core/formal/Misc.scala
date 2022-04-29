package spinal.core.formal

import spinal.core.Component

object FormalDut{
  def apply[T <: Component](dut : T) = {
    val c = Component.current
    if(c != null) {
      c.withAutoPull()
      c.setFormalTester()
    }
    dut.asFormalDut()
  }
}

