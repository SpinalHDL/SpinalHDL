package spinal.lib.misc.pipeline

import scala.collection.mutable
import spinal.core._
import spinal.lib._

object Node{
  def apply() : Node = new Node
}

trait NodeApi {
  def getNode : Node
  private val _n = getNode
  import _n._

  def valid : Bool = getNode.valid
  def ready : Bool = getNode.ready
  def isValid = valid
  def isReady = ready

  // True when the current transaction is successfuly moving forward (isReady && !isRemoved). Useful to validate state changes
  def isFiring = {
    if (status.fire.isEmpty) status.fire = Some(ContextSwapper.outsideCondScope(Bool().setCompositeName(getNode, "isFiring")))
    status.fire.get
  }

  // True when the current node is being cleaned up
  def isRemoved = {
    if (status.remove.isEmpty) status.remove = Some(ContextSwapper.outsideCondScope(Bool().setCompositeName(getNode, "isRemoved")))
    status.remove.get
  }

  // True when it is the last cycle that the current transaction is present on this node. Useful to "reset" some states
  def isMoving = {
    if (status.moving.isEmpty) status.moving = Some(ContextSwapper.outsideCondScope(Bool().setCompositeName(getNode, "isMoving")))
    status.moving.get
  }

  def setAlwaysValid(): Unit = {
    valid := True
    valid.freeze()
    alwaysValid = true
  }

  def setAlwaysReady(): Unit = {
    ready := True
    ready.freeze()
    alwaysReady = true
  }


  def apply(key: NamedTypeKey): Data = {
    keyToData.getOrElseUpdate(key, ContextSwapper.outsideCondScope {
      val ret = key.tpe()
      Misc.nameThat(getNode, ret, key, "")
      ret
    })
  }

  def apply[T <: Data](key: SignalKey[T]): T = apply(NamedTypeKey(key.asInstanceOf[SignalKey[Data]], null)).asInstanceOf[T]

  def apply[T <: Data](key: SignalKey[T], subKey: Any): T = apply(NamedTypeKey(key.asInstanceOf[SignalKey[Data]], subKey)).asInstanceOf[T]

  //Allows converting a list of key into values. ex : node(1 to 2)(MY_STAGEABLE)
  def apply(subKey: Seq[Any]) = new OffsetApi(subKey)

  class OffsetApi(subKeys: Seq[Any]) {
    def apply[T <: Data](that: SignalKey[T]): Seq[T] = {
      subKeys.map(subKey => getNode.apply(that, subKey))
    }
  }

  def insert[T <: Data](that: T): SignalKey[T] = {
    val s = SignalKey(cloneOf(that))
    this (s) := that
    s
  }

  def arbitrateFrom[T <: Data](that: Stream[T]): Unit = {
    valid := that.valid
    that.ready := ready
  }

  def arbitrateFrom[T <: Data](that: Flow[T]): Unit = {
    valid := that.valid
  }

  def driveFrom[T <: Data](that: Stream[T])(con: (Node, T) => Unit): Unit = {
    arbitrateFrom(that)
    con(getNode, that.payload)
  }

  def driveFrom[T <: Data](that: Flow[T])(con: (Node, T) => Unit): Unit = {
    arbitrateFrom(that)
    con(getNode, that.payload)
  }

  def arbitrateTo[T <: Data](that: Stream[T]): Unit = {
    that.valid := valid
    ready := that.ready
  }

  def arbitrateTo[T <: Data](that: Flow[T]): Unit = {
    that.valid := valid
    setAlwaysReady()
  }

  def driveTo[T <: Data](that: Stream[T])(con: (T, Node) => Unit): Unit = {
    arbitrateTo(that)
    con(that.payload, getNode)
  }

  def driveTo[T <: Data](that: Flow[T])(con: (T, Node) => Unit): Unit = {
    arbitrateTo(that)
    con(that.payload, getNode)
  }



//  implicit def stageablePiped[T <: Data](stageable: Stageable[T])(implicit key : StageableOffset = StageableOffsetNone) = Stage.this(stageable, key.value)
  implicit def stageablePiped2[T <: Data](stageable: SignalKey[T]) = this(stageable)

}

class Node() extends Area with NodeApi{
  override def getNode: Node = this

  override val valid = Bool()
  override val ready = Bool()

  val keyToData = mutable.LinkedHashMap[NamedTypeKey, Data]()

  val fromUp = new FromUp()
  val fromDown = new FromDown()

  var up: Connector = null
  var down: Connector = null

  var alwaysValid = false
  var alwaysReady = false

  val ctrl = new {
    def nameRemoveSeed(): Unit = removeSeed.foreach(_.setCompositeName(Node.this, "removeSeed"))
    var removeSeed = Option.empty[Bool]
  }

  val status = new {
    var fire = Option.empty[Bool]
    var moving = Option.empty[Bool]
    var remove = Option.empty[Bool]
  }

  def build(): Unit = {
    status.fire.foreach(_ := isValid && isReady && !isRemoved)
    status.moving.foreach(_ := isValid && (isReady || isRemoved))
    status.remove.foreach(_ := ctrl.removeSeed.getOrElse(False))
  }

  class Area extends spinal.core.Area with NodeApi {
    override def getNode: Node = Node.this
  }
}
