package spinal.lib

import spinal.core.{Data, HardType, IConnectable, IODirection}
import scala.collection.mutable.ArrayBuffer
import spinal.core

/** Master/slave interface */
trait IMasterSlave {

  /** Are port directions set for a Master interface? */
  final def isMasterInterface: Boolean = _isMasterInterface == Some(true)

  /** Are port directions set for a Master interface? */
  final def isSlaveInterface: Boolean = _isMasterInterface == Some(false)

  private var _isMasterInterface: Option[Boolean] = None

  /** Convert into master */
  final def intoMaster(): this.type = {
    setAsMaster()
    this
  }

  /** Convert into slave */
  final def intoSlave(): this.type = {
    setAsSlave()
    this
  }

  /** Set as master interface */
  final def setAsMaster(): Unit = {
    asMaster()
    _isMasterInterface = Some(true)
  }

  /** Set a slave interface */
  final def setAsSlave(): Unit = {
    asSlave()
    _isMasterInterface = Some(false)
  }

  /** Override it to define port directions for a master interface.
    *
    * @deprecated This method must be overriden but not called. Calling this
    * method is not correct. Call `setAsMaster()` or `intoMaster()` instead.
    *
    * This method is named `asXxx` but it does not return `Xxx`.
    *
    * This method does not update `isMasterInterface` and `isSlaveInterface`.
    */
  def asMaster(): Unit

  /** Override it to define port directions for a master interface.
    *
    * If not overriden, defaults to the opposite port directions of `asMaster()`.
    *
    * @deprecated This method can be overriden but not called. Calling this
    * method is not correct. Call `setAsSlave()` or `intoSlave()` instead.
    *
    * This method is named `asXxx` but it does not return `Xxx`.
    *
    * This method does not update `isMasterInterface` and `isSlaveInterface`.
    */
  def asSlave(): Unit = intoMaster().asInstanceOf[Data].flip()
}

/** Something which can create master/slave interfaces */
trait MSFactory {

  /** Called on IMasterSlave creation (eg: to apply master/slave-ness) */
  def postApply(interface: IMasterSlave): Unit = {}
}

/** Declare a port as `master` or `slave`
  *
  * There are 4 available syntaxes, which are all equivalent:
  *
  * {{{
  * val braces = master(Flow(Bool))
  *
  * val short = master Flow (Bool)
  *
  * val spaceful = master port Flow(Bool)
  *
  * val variadic = Flow(Bool)
  * master(variadic)
  * }}}
  *
  * The "braces" syntax is short and generic, but it uses braces.
  *
  * The "short" syntax is short, but it is formatted with a space between the
  * type and its parameters, and it can be used only with:
  *
  *   - `Flow`
  *   - `Stream`
  *
  * The "spaceful" syntax is generic and beautiful, but more verbose.
  *
  * The "variadic" syntax can be used with any number of interfaces, but can
  * be used only if the interfaces are already declared.
  *
  * @see [[master]] [[slave]]
  */
sealed trait MS {

  /** Override it to define how to apply port specification on a non-null IMasterSlave */
  protected def applyIt[T <: IMasterSlave](i: T): T

  /** Declare a port without braces, spaceful syntax
    *
    * See [[MS]] for other syntax.
    */
  def port[T <: IMasterSlave](i: T): T =
    if (i != null) applyIt(i)
    else i

  /** Declare a port without braces, spaceful syntax
    *
    * See [[MS]] for other syntax.
    */
  def port[T <: Data with IMasterSlave](i: HardType[T]): T = port(i())

  /** Declare a port with braces
    *
    * See [[MS]] for other syntaxes.
    */
  def apply[T <: IMasterSlave](i: T): T = port(i)

  def apply[T <: Data with IMasterSlave](data: HardType[T]): T = apply(data())

  /** Declare existing interfaces as ports, variadic syntax */
  def apply(is: IMasterSlave*): Unit = is.foreach(port(_))

  object Flow extends FlowFactory {
    override def postApply(interface: IMasterSlave): Unit = {
      super.postApply(interface)
      port(interface)
    }
  }

  object Stream extends StreamFactory {
    override def postApply(interface: IMasterSlave): Unit = {
      super.postApply(interface)
      port(interface)
    }
  }
}

/** Declare a master port
  *
  * See [[MS]] for syntax help.
  */
object master extends MS {
  override def applyIt[T <: IMasterSlave](i: T) = i.intoMaster()
}

/** Declare a slave port
  *
  * See [[MS]] for syntax help.
  */
object slave extends MS {
  def applyIt[T <: IMasterSlave](i: T) = i.intoSlave()
}

@deprecated("Use apply or port instead: 'val b = slave(maybeNull)' or 'val rgb = slave port maybeNull'")
object slaveWithNull extends MS {
  override def applyIt[T <: IMasterSlave](that: T): T = if (that != null) slave(that) else that
}

@deprecated("Use apply or port instead: 'val b = master(maybeNull)' or 'val rgb = master port maybeNull'")
object masterWithNull extends MS {
  override def applyIt[T <: IMasterSlave](that: T): T = if (that != null) master(that) else that
}
trait IMasterSlaveDirDeclare extends IMasterSlave {

  /** Define the direction of ports during declaration. asMaster does not need to be manually derived.
    * For example:
    * ```scala
    * case class TestInterface() extends Bundle with IMasterSlaveDirDeclare {
    *    val testOut = out port Bool()
    *    val testIn = in port Bool()
    *    val testMaster = master port Stream(Bool())
    *    val testSlave = slave port Stream(Bool())
    *  }
    *  ```
    *
    * @return
    */
  var directions = ArrayBuffer.empty[() => Unit]
  trait PortRedirect[Base] {
    def port[T <: Base](port: T): T
    def apply[T <: Base](t: T) = port(t)
  }
  def out = new PortRedirect[Data] {
    def port[T <: Data](port: T) = {
      directions += (() => spinal.core.out(port))
      port
    }
  }
  def in = new PortRedirect[Data] {
    def port[T <: Data](port: T) = {
      directions += (() => spinal.core.in(port))
      port
    }
  }
  def inout = new PortRedirect[Data] {
    def port[T <: Data](port: T) = {
      directions += (() => spinal.core.inout(port))
      port
    }

  }
  def slave = new PortRedirect[IMasterSlave] {
    def port[T <: IMasterSlave](port: T) = {
      directions += (() => spinal.lib.slave(port))
      port
    }

  }
  def master = new PortRedirect[IMasterSlave] {
    def port[T <: IMasterSlave](port: T) = {
      directions += (() => spinal.lib.master(port))
      port
    }

  }

  override final def asMaster(): Unit = directions.foreach(_())
}
