package spinal.core

/**
  * Trait used to set the direction of a data
  */
trait IODirection extends BaseTypeFactory {

  def applyIt[T <: Data](data: T): T
  def apply[T <: Data](data: T): T = applyIt(data)
  def apply[T <: Data](data: HardType[T]): T = applyIt(data())
  def apply[T <: Data](datas: T*): Unit = datas.foreach(applyIt(_))
  def apply(senum: SpinalEnum) = applyIt(senum.craft())
  def cloneOf[T <: Data](that: T): T = applyIt(spinal.core.cloneOf(that))

  def Bool(u: Unit = ()): Bool = applyIt(spinal.core.Bool())
  override def Bits(u: Unit = ()) = applyIt(super.Bits())
  override def UInt(u: Unit = ()) = applyIt(super.UInt())
  override def SInt(u: Unit = ()) = applyIt(super.SInt())
  override def Vec[T <: Data](elements: TraversableOnce[T], dataType : HardType[T] = null): Vec[T] = applyIt(super.Vec(elements, dataType))

  override def postTypeFactory[T <: Data](that: T): T = applyIt(that)
}

/** Set a data to input */
object in extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asInput()
}

/** Set a data to output */
object out extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asOutput()
}

/** Set a data to inout */
object inout extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asInOut()
}

/** Set a data to in if the data is not null */
object inWithNull extends IODirection {
  override def applyIt[T <: Data](data: T): T = if(data != null) data.asInput() else data
}

/** Set a data to out if the data is not null */
object outWithNull extends IODirection {
  override def applyIt[T <: Data](data: T): T = if(data != null) data.asOutput() else data
}