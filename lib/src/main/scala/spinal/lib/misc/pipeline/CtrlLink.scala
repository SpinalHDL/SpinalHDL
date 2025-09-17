package spinal.lib.misc.pipeline

import spinal.core._
import spinal.lib._
import spinal.idslplugin.Location
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.collection.Seq

object CtrlLink {
  def apply(up : Node, down : Node) = new CtrlLink(up, down)
  def apply() = {
    val c = new CtrlLink(new Node(), new Node())
    c.up.setCompositeName(c, "up")
    c.down.setCompositeName(c, "down")
    c
  }
}

trait CtrlApi {
  def getCtrl: CtrlLink
  private val _c = getCtrl
  import _c._

  def defaultKey : Any = null

  def up : NodeApi = _c.up
  def down : NodeApi = _c.down

  def isValid = up.isValid
  def isReady = down.isReady

  /** Same as `Link.down(Payload)` */
  def apply[T <: Data](that: Payload[T]): T = down(that, defaultKey)

  /** Same as `Link.down(Payload, subKey)` */
  def apply[T <: Data](that: Payload[T], subKey: Any): T = down(that, subKey)
  def apply(subKeys: Seq[Any]) : Node.OffsetApi = down(subKeys)

  /** Same as `Link.down.insert(Data)` */
  def insert[T <: Data](that: T): Payload[T] = down.insert(that)

  /** Allows to conditionally override a [[Payload]] value between `link.up` -> `link.down`.
   * 
   * This can be used to fix data hazard in CPU pipelines for instance.
   */
  def bypass[T <: Data](that: Payload[T]): T =  bypass(that, defaultKey)

  /** Allows to conditionally override a ([[Payload]], subKey) value between `link.up` -> `link.down`.
   * 
   * This can be used to fix data hazard in CPU pipelines for instance.
   */
  def bypass[T <: Data](that: Payload[T], subKey : Any): T =  bypass(NamedTypeKey(that.asInstanceOf[Payload[Data]], subKey)).asInstanceOf[T]
  def bypass[T <: Data](that: NamedTypeKey): Data = {
    val preserve = DslScopeStack.get != getCtrl.parentScope
    bypasses.getOrElseUpdate(that, ContextSwapper.outsideCondScope {
      val ret = that.tpe()
      Misc.nameThat(_c, ret, that, "_bypass")
      down(that) := ret
      if(preserve) ret := up(that)
      ret
    })
  }

  /** Block the current transaction when `True` (clear `up.ready` and `down.valid`) */
  def haltWhen(cond: Bool)      (implicit loc: Location): Bool = requests.halts addRet nameFromLocation(CombInit(cond), "haltRequest")

  /** Duplicate the current transaction when `True` (clear `up.ready`) */
  def duplicateWhen(cond: Bool) (implicit loc: Location): Bool = requests.duplicates addRet nameFromLocation(CombInit(cond), "duplicateRequest")

  /** Hide the current transaction from downstream when `True` (clear `down.valid`) */
  def terminateWhen(cond: Bool) (implicit loc: Location): Bool = requests.terminates addRet nameFromLocation(CombInit(cond), "terminateRequest")

  /** Request the upstream to forget its current transaction  when `True`(but doesn’t clear the `down.valid`) */
  def forgetOneWhen(cond: Bool)(implicit loc: Location): Bool = requests.forgetsOne addRet nameFromLocation(CombInit(cond), "forgetsSingleRequest")

  /** Ignore the downstream ready when `True` (set `up.ready`) */
  def ignoreReadyWhen(cond: Bool)(implicit loc: Location): Bool = requests.ignoresReady addRet nameFromLocation(CombInit(cond), "ignoreReadyRequest")
  
  /** Cancel the current transaction from the pipeline when `True`.
   * 
   * It clear `down.valid` and make the transaction driver forget its current state.
   */
  def throwWhen(cond : Bool, usingReady : Boolean = false)    (implicit loc: Location) : Unit = {
    val flag = nameFromLocation(CombInit(cond), "throwWhen")
    requests.terminates += flag
    requests.cancels += flag
    usingReady match {
      case false => requests.forgetsOne += flag
      case true =>  requests.ignoresReady += flag
    }
  }

  /** Same as [[haltWhen()]] but for use in `when` block */
  def haltIt()      (implicit loc: Location) : Unit = haltWhen(ConditionalContext.isTrue)
  /** Same as [[duplicateWhen()]] but for use in `when` block */
  def duplicateIt() (implicit loc: Location) : Unit = duplicateWhen(ConditionalContext.isTrue)
  /** Same as [[terminateWhen()]] but for use in `when` block */
  def terminateIt() (implicit loc: Location) : Unit = terminateWhen(ConditionalContext.isTrue)
  /** Same as [[throwWhen()]] but for use in `when` block */
  def throwIt(usingReady : Boolean = false)(implicit loc: Location): Unit = throwWhen(ConditionalContext.isTrue, usingReady = usingReady)
  /** Same as [[forgetOneWhen()]] but for use in `when` block */
  def forgetOneNow()(implicit loc: Location): Unit = forgetOneWhen(ConditionalContext.isTrue)
  /** Same as [[ignoreReadyWhen()]] but for use in `when` block */
  def ignoreReadyNow()(implicit loc: Location): Unit = ignoreReadyWhen(ConditionalContext.isTrue)

  def forkStream[T <: Data](forceSpawn : Option[Bool] = Option.empty[Bool]): Stream[NoData] = {
    val ret = Stream(NoData())
    val fired = RegInit(False) setCompositeName(ret, "fired")
    val firedComb = CombInit(fired)
    ret.valid := isValid && !firedComb
    haltWhen(!firedComb && !ret.ready)
    if (forceSpawn.nonEmpty) when(forceSpawn.get) {
      fired := False
      firedComb := False
    }
    fired setWhen (ret.fire) clearWhen (up.isMoving)
    ret
  }

  implicit def stageablePiped2[T <: Data](stageable: Payload[T]): T = this (stageable)

  class BundlePimper[T <: Bundle](pimped: T) {
    def :=(that: T): Unit = pimped := that
  }

  implicit def bundlePimper[T <: Bundle](stageable: Payload[T]) = new BundlePimper[T](this (stageable))
}

/** A kind of special [[Link]] that connects two nodes with optional flow control / bypass logic.
  * 
  * Its API should be flexible enough to implement a CPU stage with it.
  * 
  * It as an [[up]] and a [[down]] node.
  * 
  * @see [[https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Libraries/Pipeline/introduction.html#ctrllink CtrlLink documentation]]
  */
class CtrlLink(override val up : Node, override val down : Node) extends Link with CtrlApi {
  down.up = this
  up.down = this

  override def getCtrl: CtrlLink = this

  def nameFromLocation[T <: Data](that: T, prefix: String)(implicit loc: Location): T = {
    that.setCompositeName(this, prefix + "_" + loc.fileSymbol + "_l" + loc.line, Nameable.REMOVABLE)
  }

  val bypasses = mutable.LinkedHashMap[NamedTypeKey, Data]()
  val requests = new{
    val halts = ArrayBuffer[Bool]()
    val duplicates = ArrayBuffer[Bool]()
    val terminates = ArrayBuffer[Bool]()
    val ignoresReady = ArrayBuffer[Bool]()
    val forgetsOne = ArrayBuffer[Bool]()
    val cancels = ArrayBuffer[Bool]()

    def impactValid = halts.nonEmpty || terminates.nonEmpty
    def impactReady = halts.nonEmpty || duplicates.nonEmpty
  }

  override def ups: Seq[Node] = List(up)
  override def downs: Seq[Node] = List(down)

  def or(a : Option[Bool], b : Seq[Bool]) : Option[Bool] = {
    val l = a.toList ++ b.toList
    if(l.isEmpty) return Option.empty[Bool]
    Some(l.orR)
  }

  override def propagateDown(): Unit = {
    propagateDownAll()
    if(up.ctrl.valid.nonEmpty || requests.impactValid) down.valid
    down.ctrl.forgetOneSupported = true
  }

  override def propagateUp(): Unit = {
    propagateUpAll()
    up.ctrl.forgetOneCreate(or(down.ctrl.forgetOne, requests.forgetsOne))
    or(down.ctrl.cancel, requests.cancels).map(up.cancel := _)

    if (down.ctrl.ready.nonEmpty || requests.impactReady) {
      up.ready
    }
  }

  override def build(): Unit = {
    if(down.ctrl.valid.nonEmpty) down.valid := up.valid
    if(up.ctrl.ready.nonEmpty) {
      up.ready := down.isReady
    }
    if(requests.halts.nonEmpty) when(requests.halts.orR) {
      down.valid := False
      up.ready := False
    }
    if(requests.duplicates.nonEmpty) when(requests.duplicates.orR) {
      up.ready := False
    }
    if(requests.terminates.nonEmpty) when(requests.terminates.orR) {
      down.valid := False
    }
    if (up.ctrl.ready.nonEmpty) {
      if (requests.ignoresReady.nonEmpty) when(requests.ignoresReady.orR) {
        up.ready := True
      }
    }
    val matches = down.fromUp.payload.intersect(up.fromDown.payload)
    for (m <- matches) {
      bypasses.get(m) match {
        case Some(x) =>
        case None => down(m) := up(m)
      }
    }
  }

  class Area(override val defaultKey : Any = null)  extends spinal.core.Area with CtrlApi {
    override def getCtrl: CtrlLink = CtrlLink.this
  }
}


class CtrlLinkMirror(from : CtrlLink) extends spinal.core.Area with CtrlApi {
  override def getCtrl: CtrlLink = from
}