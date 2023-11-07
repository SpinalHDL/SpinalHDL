package spinal.lib.misc.pipeline

import spinal.core._
import spinal.lib._
import spinal.idslplugin.Location
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

object CtrlConnector {
  def apply(up : Node, down : Node) = new CtrlConnector(up, down)
  def apply() = {
    val c = new CtrlConnector(new Node(), new Node())
    c.up.setCompositeName(c, "up")
    c.down.setCompositeName(c, "down")
    c
  }
}

class CtrlConnector(val up : Node, val down : Node) extends Connector {
  down.up = this
  up.down = this

  def nameFromLocation[T <: Data](that: T, prefix: String)(implicit loc: Location): T = {
    that.setCompositeName(this, prefix + "_" + loc.file + "_l" + loc.line, Nameable.REMOVABLE)
  }

  val requests = new{
    val halts = ArrayBuffer[Bool]()
    val duplicates = ArrayBuffer[Bool]()
    val terminates = ArrayBuffer[Bool]()
    val removeSeeds = ArrayBuffer[Bool]()
  }

  def apply[T <: Data](that: Stageable[T]): T = down(that)
  def apply[T <: Data](that: Stageable[T], subKey: Any): T = down(that, subKey)
  def apply(subKeys: Seq[Any]) = down(subKeys)



  def insert[T <: Data](that: T): Stageable[T] = down.insert(that)

  def bypass[T <: Data](that: Stageable[T]): T =  bypass(that, null)
  def bypass[T <: Data](that: Stageable[T], subKey : Any): T =  bypass(StageableKey(that.asInstanceOf[Stageable[Data]], subKey)).asInstanceOf[T]
  def bypass[T <: Data](that: StageableKey): Data = bypasses.getOrElseUpdate(that, ContextSwapper.outsideCondScope {
    val ret = that.stageable()
    Misc.nameThat(this, ret, that, "bypass")
    ret := up(that)
    ret
  })

  def haltWhen(cond: Bool)      (implicit loc: Location): Unit = requests.halts += nameFromLocation(CombInit(cond), "haltRequest")
  def duplicateWhen(cond: Bool) (implicit loc: Location): Unit = requests.duplicates += nameFromLocation(CombInit(cond), "duplicateRequest")
  def terminateWhen(cond: Bool) (implicit loc: Location): Unit = requests.terminates += nameFromLocation(CombInit(cond), "terminateRequest")
  def removeSeedWhen(cond: Bool)(implicit loc: Location): Unit = requests.removeSeeds += nameFromLocation(CombInit(cond), "removeSeedRequest")
  def throwWhen(cond : Bool)    (implicit loc: Location) : Unit = {
    val flag = nameFromLocation(CombInit(cond), "throwWhen")
    requests.terminates += flag
    requests.removeSeeds += flag
  }

  def haltIt()      (implicit loc: Location) : Unit = haltWhen(ConditionalContext.isTrue)
  def duplicateIt() (implicit loc: Location) : Unit = duplicateWhen(ConditionalContext.isTrue)
  def terminateIt() (implicit loc: Location) : Unit = terminateWhen(ConditionalContext.isTrue)
  def removeSeedIt()(implicit loc: Location) : Unit = removeSeedWhen(ConditionalContext.isTrue)
  def throwIt()     (implicit loc: Location) : Unit = throwWhen(ConditionalContext.isTrue)

  val bypasses = mutable.LinkedHashMap[StageableKey, Data]()
  val keyToData = mutable.LinkedHashMap[StageableKey, Data]()

  def apply(key: StageableKey): Data = {
    keyToData.getOrElseUpdate(key, ContextSwapper.outsideCondScope {
      key.stageable()
    })
  }

  override def ups: Seq[Node] = List(up)
  override def downs: Seq[Node] = List(down)

  def or(a : Option[Bool], b : Seq[Bool]) : Option[Bool] = {
    val l = a.toList ++ b.toList
    if(l.isEmpty) return Option.empty[Bool]
    Some(l.orR)
  }

  override def propagateDown(): Unit = propagateDownAll()
  override def propagateUp(): Unit = {
    propagateUpAll()
    up.ctrl.removeSeed = or(down.ctrl.removeSeed, requests.removeSeeds)
    up.ctrl.nameRemoveSeed()
  }

  override def build(): Unit = {
    down.valid := up.valid
    up.ready := down.ready
    if(requests.halts.nonEmpty) when(requests.halts.orR){
      down.valid := False
      up.ready := False
    }
    if(requests.duplicates.nonEmpty) when(requests.duplicates.orR){
      up.ready := False
    }
    if(requests.terminates.nonEmpty) when(requests.terminates.orR){
      down.valid := False
    }
    val matches = down.fromUp.payload.intersect(up.fromDown.payload)
    for (m <- matches) {
      bypasses.get(m) match {
        case Some(x) => down(m) := x
        case None => down(m) := up(m)
      }
    }
  }
}
