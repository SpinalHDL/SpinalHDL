package spinal.lib.misc.pipeline

import scala.collection.mutable
import spinal.core._
import spinal.lib._

class Node() extends Area {
  val valid = Bool()
  val ready = Bool()

  val ctrl = new {
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

  def nameThat(target: Nameable, key: StageableKey, postfix: String): Unit = {
    target.setLambdaName(this.isNamed && key.stageable.isNamed) {
      val stageName = this.getName
      val stageSlices = stageName.split('_')
      val postfixName = key.toString + postfix
      val postfixSlices = postfixName.split('_')
      var i = 0
      val iEnd = stageSlices.length min postfixSlices.length
      while (i != iEnd && stageSlices(i) == postfixSlices(i)) i += 1
      stageName + "_" + postfixSlices.drop(i).mkString("_")
    }
  }

  def apply(key: StageableKey): Data = {
    keyToData.getOrElseUpdate(key, ContextSwapper.outsideCondScope {
      val ret = key.stageable()
      nameThat(ret, key, "")
      ret
    })
  }

  def apply[T <: Data](key: Stageable[T]): T = {
    apply(StageableKey(key.asInstanceOf[Stageable[Data]], null)).asInstanceOf[T]
  }

  def insert[T <: Data](that: T): Stageable[T] = {
    val s = Stageable(cloneOf(that))
    this(s) := that
    s
  }

  def driveFrom[T <: Data](stream: Stream[T])(con: (Node, T) => Unit): Unit = {
    valid := stream.valid
    stream.ready := ready
    con(this, stream.payload)
  }

  def toStream[T <: Data](stream: Stream[T])(con: (T, Node) => Unit): Unit = {
    stream.valid := valid
    ready := stream.ready
    con(stream.payload, this)
  }
}
