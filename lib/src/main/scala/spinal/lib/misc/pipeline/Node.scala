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

  def isValid : Bool
  def isReady : Bool
  def isCancel : Bool

  /** `True` when the current transaction is successfully moving forward (`isReady && !isRemoved`). Useful to validate state changes. */
  def isFiring : Bool
  /** `True` when it is the last cycle that the current transaction is present on this node. Useful to "reset" some states. */
  def isMoving : Bool
  /** `True` when the current node is being cleaned up. */
  def isCanceling: Bool



  def apply(key: NamedTypeKey): Data

  /** Return the hardware signal for this [[Payload]] key at the point of this [[Node]] in the pipeline.*/
  def apply[T <: Data](key: Payload[T]): T
  
  /** Return the hardware signal for this ([[Payload]], subKey) key at the point of this [[Node]] in the pipeline.
    * 
    * This eases the construction of multi-lane hardware. For instance, when you have a 
    * multi-issue CPU pipeline, you can use the lane `Int` id as secondary key.
    */
  def apply[T <: Data](key: Payload[T], subKey: Any): T = {
    apply(NamedTypeKey(key.asInstanceOf[Payload[Data]], subKey)).asInstanceOf[T]
  }

  /** Allows converting a list of key into values. ex : node(1 to 2)(MY_STAGEABLE) */
  def apply(subKey: Seq[Any]) : Node.OffsetApi
  
  /** Return a new [[Payload]] which is connected to the given `Data` hardware signal starting from this Node in the pipeline. */
  def insert[T <: Data](that: T): Payload[T] = {
    val s = Payload(cloneOf(that))
    this (s) := that
    s
  }

  implicit def stageablePiped2[T <: Data](stageable: Payload[T]) : T = this(stageable)

  class BundlePimper[T <: Bundle](pimped : T) {
    def :=(that: T): Unit = pimped := that
  }
  implicit def bundlePimper[T <: Bundle](stageable: Payload[T]) = new  BundlePimper[T](this(stageable))
}

trait NodeApi extends NodeBaseApi {
  def getNode : Node
  private val _n = getNode
  import _n._

  def defaultKey : Any = null

  /** The signal which specifies if a transaction is present on the node.
    * 
    * It is driven by the upstream. Once asserted, it must only be de-asserted the cycle after which
    * either both [[valid]] and [[ready]] or [[cancel]] are high. [[valid]] must not depend on [[ready]].
    * 
    * Created on demand, thus it's important to use [[isValid]] to get the signal value.
    * @see [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Libraries/Pipeline/introduction.html#node Node documentation]]
    */
  def valid : Bool = getNode.valid

  /** The signal which specifies if the node’s transaction can proceed downstream.
    *
    * It is driven by the downstream to create backpressure. The signal has no meaning when there
    * is no transaction ([[valid]] being deasserted).
    * 
    * Created on demand, thus it's important to use [[isReady]] to get the signal value.
    * @see [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Libraries/Pipeline/introduction.html#node Node documentation]]
    */
  def ready : Bool = getNode.ready

  /** The signal which specifies if the node’s transaction in being canceled from the pipeline.
    * 
    * It is driven by the downstream. The signal has no meaning when there is no transaction
    * ([[valid]] being deasserted).
    * 
    * Created on demand, thus it's important to use [[isReady]] to get the signal value.
    * @see [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Libraries/Pipeline/introduction.html#node Node documentation]]
    */
  def cancel : Bool = getNode.cancel

  /** Read-only accessor of [[valid]] */
  def isValid: Bool = {
    if (status.isValid.isEmpty) status.isValid = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "isValid")))
    status.isValid.get
  }

  /** Read-only accessor of [[ready]] */
  def isReady: Bool = {
    if (status.isReady.isEmpty) status.isReady = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "isReady")))
    status.isReady.get
  }

  /** `True` when the current transaction is successfully moving forward (`isReady && !isRemoved`).
    * 
    * Useful to validate state changes.
    */ 
  def isFiring : Bool = {
    if (status.isFiring.isEmpty) status.isFiring = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "isFiring")))
    status.isFiring.get
  }

  /** True when it is the last cycle that the current transaction is present on this node. 
    * 
    * More precisely, `True` when the node transaction will not be present anymore on the node 
    * (starting from the next cycle), either because downstream is ready to take the transaction, or 
    * because the transaction is canceled from the pipeline. (`valid && (ready || cancel)`).
    * 
    * Useful to “reset” states.
    */
  def isMoving : Bool = {
    if (status.isMoving.isEmpty) status.isMoving = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "isMoving")))
    status.isMoving.get
  }

  /**
    * `True` when the node transaction is being cleaned up.
    * 
    * Meaning that it will not appear anywhere in the pipeline in future cycles.
    * It is equivalent to `isValid && isCancel`.
    */
  def isCanceling: Bool = {
    if (status.isCanceling.isEmpty) status.isCanceling = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "isCanceling")))
    status.isCanceling.get
  }

  /** Read-only accessor of [[cancel]] */
  def isCancel: Bool = {
    if (status.isCancel.isEmpty) status.isCancel = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "isCancel")))
    status.isCancel.get
  }

  def apply(key: NamedTypeKey): Data = {
    keyToData.get(key) match {
      case Some(x) => x
      case None => {
        val made = ContextSwapper.outsideCondScopeData(key.tpe())
        // So, that's a bit complicated, because it need to survive a Fiber blocking key.tpe()
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
    that.ready := isReady || isCancel
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
    that.valid := isValid
    ready := that.ready
  }

  def arbitrateTo[T <: Data](that: Flow[T]): Unit = {
    that.valid := isValid
  }

  def driveTo[T <: Data](that: Stream[T])(con: (T, Node) => Unit): Unit = {
    arbitrateTo(that)
    con(that.payload, getNode)
  }

  def driveTo[T <: Data](that: Flow[T])(con: (T, Node) => Unit): Unit = {
    arbitrateTo(that)
    con(that.payload, getNode)
  }

  def toStream[T <: Data](con: (Node) => T): Stream[T] = {
    val newPayload = con(getNode)
    val that = Stream(cloneOf(newPayload))
    that.payload := newPayload 
    arbitrateTo(that)
    that
  }
  
  def toFlow[T <: Data](con: (Node) => T): Flow[T] = {
    val newPayload = con(getNode)
    val that = Flow(cloneOf(newPayload))
    that.payload := newPayload 
    arbitrateTo(that)
    that
  }
}

class Node() extends Area with NodeApi {
  override def getNode: Node = this

  override def valid = {
    if (ctrl.valid.isEmpty) ctrl.valid = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "valid")))
    ctrl.valid.get
  }
  override def ready = {
    if (ctrl.ready.isEmpty) ctrl.ready = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "ready")))
    ctrl.ready.get
  }
  override def cancel = {
    if (ctrl.cancel.isEmpty) ctrl.cancel = Some(ContextSwapper.outsideCondScopeData(Bool().setCompositeName(getNode, "cancel")))
    ctrl.cancel.get
  }

  val keyToData = mutable.LinkedHashMap[NamedTypeKey, Data]()

  val fromUp = new FromUp()
  val fromDown = new FromDown()

  var up: Link = null
  var down: Link = null

  val ctrl = new {
    var forgetOne = Option.empty[Bool]
    var forgetOneSupported = false
    def forgetOneCreate(value: Option[Bool] = Some(Bool())): Unit = forgetOne = value.map(_.setCompositeName(Node.this, "forgetOne"))

    var valid = Option.empty[Bool]
    var ready = Option.empty[Bool]
    var cancel = Option.empty[Bool]
  }

  val status = new {
    var isValid = Option.empty[Bool]
    var isReady = Option.empty[Bool]
    var isCancel = Option.empty[Bool]
    var isFiring = Option.empty[Bool]
    var isMoving = Option.empty[Bool]
    var isCanceling = Option.empty[Bool]
  }

  def build(): Unit = {
    if(!ctrl.forgetOneSupported && ctrl.forgetOne.nonEmpty) {
      SpinalError(s"${this.getName()} doesn't support ctrl.forgetOne")
    }

    ctrl.cancel match {
      case Some(cancel) => {
        status.isFiring.foreach(_ := isValid && isReady && !isCancel)
        status.isMoving.foreach(_ := isValid && (isReady || isCancel))
      }
      case None => { // To avoid hasCancelRequest usages
        status.isFiring.foreach(_ := isValid && isReady)
        status.isMoving.foreach(_ := isValid && isReady)
      }
    }

    status.isValid.foreach(_ := ctrl.valid.getOrElse(True))
    status.isReady.foreach(_ := ctrl.ready.getOrElse(True))
    status.isCancel.foreach(_ := ctrl.cancel.getOrElse(False))
    status.isCanceling.foreach(_ := status.isCancel.map(isValid && _).getOrElse(False))
  }

  class Area(override val defaultKey : Any = null) extends spinal.core.Area with NodeApi {
    override def getNode: Node = Node.this
  }
}


class NodeMirror(node : Node) extends NodeApi {
  def getNode: Node = node
}