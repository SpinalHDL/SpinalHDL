package spinal.lib.misc.pipeline

import scala.collection.mutable
import spinal.core._
import spinal.lib._

object Node{
  def apply() : Node = new Node
}

class Node() extends Area {
  val valid = Bool()
  val ready = Bool()

  val ctrl = new {
    def nameRemoveSeed(): Unit = removeSeed.foreach(_.setCompositeName(Node.this, "removeSeed"))
    var removeSeed = Option.empty[Bool]
  }

  def isValid = valid
  def isReady = ready
  val isFireing = valid && ready

  val keyToData = mutable.LinkedHashMap[StageableKey, Data]()

  val fromUp = new FromUp()
  val fromDown = new FromDown()

  var up: Connector = null
  var down: Connector = null

  var alwaysValid = false
  var alwaysReady = false

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


  def apply(key: StageableKey): Data = {
    keyToData.getOrElseUpdate(key, ContextSwapper.outsideCondScope {
      val ret = key.stageable()
      Misc.nameThat(this, ret, key, "")
      ret
    })
  }

  def apply[T <: Data](key: Stageable[T]): T = apply(StageableKey(key.asInstanceOf[Stageable[Data]], null)).asInstanceOf[T]
  def apply[T <: Data](key: Stageable[T], subKey: Any): T = apply(StageableKey(key.asInstanceOf[Stageable[Data]], subKey)).asInstanceOf[T]

  //Allows converting a list of key into values. ex : node(1 to 2)(MY_STAGEABLE)
  def apply(subKey: Seq[Any]) = new OffsetApi(subKey)
  class OffsetApi(subKeys: Seq[Any]) {
    def apply[T <: Data](that: Stageable[T]): Seq[T] = {
      subKeys.map(subKey => Node.this.apply(that, subKey))
    }
  }

  def insert[T <: Data](that: T): Stageable[T] = {
    val s = Stageable(cloneOf(that))
    this(s) := that
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
    con(this, that.payload)
  }

  def driveFrom[T <: Data](that: Flow[T])(con: (Node, T) => Unit): Unit = {
    arbitrateFrom(that)
    con(this, that.payload)
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
    con(that.payload, this)
  }

  def driveTo[T <: Data](that: Flow[T])(con: (T, Node) => Unit): Unit = {
    arbitrateTo(that)
    con(that.payload, this)
  }
}
