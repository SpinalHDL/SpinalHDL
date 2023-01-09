package spinal.core

/** Declare ports
  *
  * A port is some Data with a direction, which can be `in`, `out` or `inout`.
  *
  * There are 4 available syntaxes, which are all equivalent:
  *
  * {{{
  * val braces = in(Vec(Bool, 5))
  *
  * val short = in Vec (Bool, 5)
  *
  * val spaceful = in port Vec(Bool, 5)
  *
  * val variadic = Vec(Bool, 5)
  * in(variadic)
  * }}}
  *
  * The "braces" syntax is short and generic, but it uses braces.
  *
  * The "short" syntax is short, but it is formatted with a space between the
  * type and its parameters, and it can be used only with:
  *
  *   - `Bool`
  *   - `Bits`
  *   - `UInt`
  *   - `SInt`
  *   - `Vec`
  *
  * The "spaceful" syntax is generic and beatiful, but more verbose.
  *
  * The "variadic" syntax can be used with any number of ports, but can be used
  * only if the ports types are already declared.
  *
  * @see [[in]] [[out]] [[inout]]
  */
sealed trait IODirection extends BaseTypeFactory {

  // This function should be protected, not public
  // Override it to define how to apply port specification on a non-null Data
  @deprecated("Use apply or port instead: `val b = in Bool ()` or `val rgb = out port Rgb`")
  def applyIt[T <: Data](data: T): T

  /** Declare a port without braces, spaceful syntax
    *
    * See [[IODirection]] for other syntax.
    */
  def port[T <: Data](data: T): T =
    if (data != null) applyIt(data)
    else data

  /** Declare a port without braces, spaceful syntax
    *
    * See [[IODirection]] for other syntax.
    */
  def port[T <: Data](data: HardType[T]): T = port(data())

  /** Declare a [[SpinalEnum]] port without braces, spaceful syntax
    *
    * See [[IODirection]] for other syntax.
    */
  def port(senum: SpinalEnum): SpinalEnumCraft[senum.type] = port(senum.craft())

  /** Declare a port with braces
    *
    * See [[IODirection]] for other syntaxes.
    */
  def apply[T <: Data](data: T): T = port(data)

  /** Declare a port with braces
    *
    * See [[IODirection]] for other syntaxes.
    */
  def apply[T <: Data](data: HardType[T]): T = port(data())

  /** Declare a [[SpinalEnum]] port with braces
    *
    * See [[IODirection]] for other syntaxes.
    */
  def apply(senum: SpinalEnum): SpinalEnumCraft[senum.type] = port(senum.craft())

  /** Declare existing Data as ports, "variadic" syntax */
  def apply[T <: Data](datas: T*): Unit = datas.foreach(port(_))

  /** Declare port with same type as that */
  def cloneOf[T <: Data](that: T): T = port(spinal.core.cloneOf(that))

  /** Declare a port without braces, short syntax
    *
    * See [[IODirection]] for other syntaxes.
    */
  override def Bool(u: Unit = ()): Bool = port(super.Bool())

  override def Bits(u: Unit = ()): Bits = port(super.Bits())

  override def UInt(u: Unit = ()): UInt = port(super.UInt())

  override def SInt(u: Unit = ()): SInt = port(super.SInt())

  override def Vec[T <: Data](elements: TraversableOnce[T], dataType: HardType[T] = null): Vec[T] =
    port(super.Vec(elements, dataType))

  override def postTypeFactory[T <: Data](that: T): T = port(that)
}

/** Declare an input port
  *
  * See [[IODirection]] for syntax help.
  */
object in extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asInput()
}

/** Declare an output port
  *
  * See [[IODirection]] for syntax help.
  */
object out extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asOutput()
}

/** Declare an inout port
  *
  * See [[IODirection]] for syntax help.
  */
object inout extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asInOut()
}

@deprecated("Use apply or port instead: 'val b = in(maybeNull)' or 'val rgb = in port maybeNull'")
object inWithNull extends IODirection {
  override def applyIt[T <: Data](data: T): T = if (data != null) data.asInput() else data
}

@deprecated("Use apply or port instead: 'val b = out(maybeNull)' or 'val rgb = out port maybeNull'")
object outWithNull extends IODirection {
  override def applyIt[T <: Data](data: T): T = if (data != null) data.asOutput() else data
}
