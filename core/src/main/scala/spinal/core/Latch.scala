package spinal.core

object Latch {

  def apply[T <: Data](dataType: HardType[T]): T = {
    dataType().addTag(noLatchCheck)
  }
}

object LatchWhen {

  def apply[T <: Data](next: T, cond: Bool): T = {
    val latch = Latch(next).setCompositeName(next, "latchWhen", true)

    when(cond) {
      latch := next
    }
    latch
  }
}
