package spinal.core

object Latch {

  def apply[T <: Data](dataType: HardType[T]): T = {
    dataType.apply().addTag(noLatchCheck)
  }
}

object LatchWhen {

  def apply[T <: Data](next: T, cond: Bool): T = {
    val latch = next.clone()
      .setCompositeName(next, "latchWhen", true)
      .addTag(noLatchCheck)
      .asInstanceOf[T]

    when(cond) {
      latch := next
    }
    latch
  }
}