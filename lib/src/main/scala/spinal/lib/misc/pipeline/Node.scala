package spinal.lib.misc.pipeline

import scala.collection.Seq
import scala.collection.mutable
import spinal.core._
import spinal.lib._

object Node{
  def apply() : Node = new Node

  class OffsetApi(subKeys: Seq[Any], node: Node) {
    def apply[T <: Data](that: Payload[T]): Seq[T] = {
      subKeys.map(subKey => node.apply(that, subKey))
    }
  }
}

trait NodeBaseApi {
  def valid : Bool
  def ready : Bool
  def cancel: Bool

  def isValid = valid
  def isReady = ready
  def isCancel = cancel

  def isFiring : Bool   // True when the current transaction is successfuly moving forward (isReady && !isRemoved). Useful to validate state changes
  def isMoving : Bool   // True when it is the last cycle that the current transaction is present on this node. Useful to "reset" some states
  def isCanceling: Bool // True when the current node is being cleaned up



  def apply(key: NamedTypeKey): Data
  def apply[T <: Data](key: Payload[T]): T
  def apply[T <: Data](key: Payload[T], subKey: Any): T = {
    apply(NamedTypeKey(key.asInstanceOf[Payload[Data]], subKey)).asInstanceOf[T]
  }
  def apply(subKey: Seq[Any]) : Node.OffsetApi  //Allows converting a list of key into values. ex : node(1 to 2)(MY_STAGEABLE)
  def insert[T <: Data](that: T): Payload[T] = {
    val s = Payload(cloneOf(that))
    this (s) := that
    s
  }

  implicit def stageablePiped2[T <: Data](stageable: Payload[T]) : T = this(stageable)

  class BundlePimper[T <: Bundle](pimped : T){
    def :=(that: T): Unit = pimped := that
  }
  implicit def bundlePimper[T <: Bundle](stageable: Payload[T]) = new  BundlePimper[T](this(stageable))
}

trait NodeApi extends NodeBaseApi {
  def getNode : Node
  private val _n = getNode
  import _n._

  def defaultKey : Any = null

  def valid : Bool = getNode.valid
  def ready : Bool = getNode.ready


  override def isCancel: Bool = cancel

  // True when the current transaction is successfuly moving forward (isReady && !isRemoved). Useful to validate state changes
  def isFiring : Bool = {
    if (status.isFiring.isEmpty) status.isFiring = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "isFiring")))
    status.isFiring.get
  }

  // True when it is the last cycle that the current transaction is present on this node. Useful to "reset" some states
  def isMoving : Bool = {
    if (status.isMoving.isEmpty) status.isMoving = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "isMoving")))
    status.isMoving.get
  }

  // True when the current node is being cleaned up
  def isCanceling: Bool = {
    if (status.isCanceling.isEmpty) status.isCanceling = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "isCanceling")))
    status.isCanceling.get
  }

  def cancel: Bool = {
    if (status.cancel.isEmpty) status.cancel = Some(ContextSwapper.outsideCondScopeData(Bool())) //Unamed as it come from ctrl.cancel anyway
    status.cancel.get
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
    keyToData.get(key) match {
      case Some(x) => x
      case None => {
        val made = ContextSwapper.outsideCondScopeData(key.tpe())
        //So, that's a bit complicated, because it need to survive a Fiber blocking key.tpe()
        keyToData.get(key) match {
          case Some(x) => x
          case None => {
            keyToData(key) = made
            Misc.nameThat(getNode, made, key, "")
            made
          }
        }
      }
    }
  }

  def apply[T <: Data](key: Payload[T]): T = apply(NamedTypeKey(key.asInstanceOf[Payload[Data]], defaultKey)).asInstanceOf[T]
  def apply(subKey: Seq[Any]) : Node.OffsetApi = new Node.OffsetApi(subKey, getNode)


  def arbitrateFrom[T <: Data](that: Stream[T]): Unit = {
    valid := that.valid
    that.ready := ready || cancel
    ctrl.forgetOneSupported = true
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
}

class Node() extends Area with NodeApi{
  override def getNode: Node = this

  override val valid = Bool()
  override val ready = Bool()

  val keyToData = mutable.LinkedHashMap[NamedTypeKey, Data]()

  val fromUp = new FromUp()
  val fromDown = new FromDown()

  var up: Link = null
  var down: Link = null

  var alwaysValid = false
  var alwaysReady = false

  val ctrl = new {
    var forgetOne = Option.empty[Bool]
    var forgetOneSupported = false
    def forgetOneCreate(value: Option[Bool] = Some(Bool())): Unit = forgetOne = value.map(_.setCompositeName(Node.this, "forgetOne"))

    var cancel = Option.empty[Bool]
    def cancelCreate(value: Option[Bool] = Some(Bool())): Unit = cancel = value.map(_.setCompositeName(Node.this, "cancel"))
  }

  val status = new {
    var isFiring = Option.empty[Bool]
    var isMoving = Option.empty[Bool]
    var isCanceling = Option.empty[Bool]
    var cancel = Option.empty[Bool]
  }

  def build(): Unit = {
    if(!ctrl.forgetOneSupported && ctrl.forgetOne.nonEmpty){
      SpinalError(s"${this.getName()} doesn't support ctrl.forgetOne")
    }

    ctrl.cancel match {
      case Some(cancel) => {
        status.isFiring.foreach(_ := isValid && isReady && !cancel)
        status.isMoving.foreach(_ := isValid && (isReady || cancel))
      }
      case None => { //To avoid hasCancelRequest usages
        status.isFiring.foreach(_ := isValid && isReady)
        status.isMoving.foreach(_ := isValid && isReady)
      }
    }

    status.cancel.foreach(_ := ctrl.cancel.getOrElse(False))
    status.isCanceling.foreach(_ := status.cancel.map(isValid && _).getOrElse(False))
  }

  class Area(override val defaultKey : Any = null) extends spinal.core.Area with NodeApi {
    override def getNode: Node = Node.this
  }
}
