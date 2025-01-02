package spinal.core.formal

import spinal.core.{Area, Bool, Component, assert, assume}

object FormalDut{
  def apply[T <: Component](dut : T) = {
    val c = Component.current
    if(c != null) {
      c.withAutoPull()
      c.setFormalTester()
    }

    dut match {
      case withAsserts: WithFormalAsserts => withAsserts.formalAsserts()
      case _ => {}
    }

    dut.asFormalDut()
  }
}

trait WithFormalAsserts {
  def formalAsserts(implicit useAssumes : Boolean = false): Area
  def formalAssumes(): Unit = formalAsserts(true)
  def assertOrAssume(cond : Bool, msg : Any*)(implicit useAssumes : Boolean): Unit = {
    if(useAssumes) {
      assume(cond)
    } else {
      assert(cond, Seq(msg:_*))
    }
  }
  def withFormalAsserts() : this.type = {
    formalAsserts()
    this
  }
}
