package spinal.lib.pipeline

import spinal.lib._
import spinal.core._
import spinal.idslplugin.Location

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Stage(implicit _pip: Pipeline = null)  extends Area {
  if(_pip != null) {
    _pip.addStage(this)
  }

  def chainConnect(connection: ConnectionLogic): Unit ={
    _pip.connect(_pip.stagesSet.takeWhile(_ != this).last, this)(connection)
  }

  def this(connection: ConnectionLogic)(implicit _pip: Pipeline)  {
    this()
    chainConnect(connection)
  }


  def driveFrom[T <: Data](stream : Stream[T]): Unit ={
    valid := stream.valid
    stream.ready := isReady
  }

  def driveFrom(stage : Stage, cond : Bool, values : List[Stageable[_ <: Data]], syncronous : Boolean = true) = new Composite(this, "driveFrom"){
    isValid := stage.isValid && cond
    val doHalt = isValid && cond && !isReady
    stage.haltIt(doHalt)

    val fired = !syncronous generate new Area{
      val done = RegInit(False) setWhen(isFireing) clearWhen(isChanging)
      when(done){
        doHalt := False
        isValid := False
      }
    }

    for(value <- values){
      self(value).assignFrom(stage(value))
    }
  }

  def forkStream(cond : Bool = True)(implicit loc: Location) : Stream[NoData] = {
    val ret = Stream(NoData())
    val fired = RegInit(False) setWhen(ret.fire) clearWhen(isChanging) setCompositeName(ret, "fired")
    ret.valid := isValid && !fired && cond
    haltIt(!fired && !ret.ready && cond)
    ret
  }


  def forkFlow(cond: Bool = True)(implicit loc: Location): Flow[NoData] = {
    val ret = Flow(NoData())
    val fired = RegInit(False) setWhen (ret.fire) clearWhen (isChanging) setCompositeName(ret, "fired")
    ret.valid := isValid && !fired && cond
    ret
  }


  def toStream()(implicit loc: Location): Stream[NoData] = {
    val ret = Stream(NoData())
    ret.valid := isValid && !isRemoved
    haltIt(!ret.ready)
    ret
  }


  val internals = new {
    val input = new {
      val valid = Bool()
      var ready : Bool = null
    }

    val output = new Area {
      val valid = Bool()
      var ready : Bool = null
    }

    //    val will = new {
    //      var haltAsk, removeAsk, flushAsk = false
    //      var haltBe, removeBe, flushBe = false
    //    }

    val arbitration = new {
      var isRemoved : Bool = null
      var isFlushed : Bool = null
      var isThrown : Bool = null
      var isForked : Bool = null
      var isFlushingNext : Bool = null
      var isFlushingRoot : Bool = null
      var isHalted : Bool = null
      var isHaltedByOthers : Bool = null
      var propagateReady = false
    }

    val request = new {
      val halts = ArrayBuffer[Bool]()
      val throws = ArrayBuffer[Bool]()
      val throwsRoot = ArrayBuffer[Bool]()
      val forks = ArrayBuffer[Bool]()
      val spawns = ArrayBuffer[Bool]()
      val flush = ArrayBuffer[Bool]()
      val flushRoot = ArrayBuffer[Bool]()
      val flushNext = ArrayBuffer[Bool]()
    }



    val stageableToData = mutable.LinkedHashMap[StageableKey, Data]()
    val stageableOverloadedToData = mutable.LinkedHashMap[StageableKey, Data]()
    val stageableResultingToData = mutable.LinkedHashMap[StageableKey, Data]()
    val stageableTerminal = mutable.LinkedHashSet[StageableKey]()

    def allRequiredStageables = stageableToData ++ stageableResultingToData

    def outputOf(key : StageableKey) = stageableOverloadedToData.get(key) match {
      case Some(x) => x
      case None => stageableToData(key)
    }
  }

  def nameFromLocation[T <: Data](that : T, prefix : String)(implicit loc: Location) : T ={
    that.setCompositeName(this, prefix + "_" + loc.file + "_l" + loc.line, Nameable.REMOVABLE)
  }

  implicit def stageablePiped[T <: Data](stageable: Stageable[T])(implicit key : StageableOffset = StageableOffsetNone) = Stage.this(stageable, key.value)
  implicit def stageablePiped2[T <: Data](stageable: Stageable[T]) = new {
    def of(key : Any) = Stage.this.apply(stageable, key)
//    def := (value : T)(implicit key : StageableOffset = StageableOffsetNone) =  Stage.this(stageable, key.value) := value
  }
  implicit def stageablePiped3[T <: Data](key: Tuple2[Stageable[T], Any]) = Stage.this(key._1, key._2)

  implicit def stageablePipedVec[T <: Data](stageable: Stageable[Vec[T]])(implicit key : StageableOffset = StageableOffsetNone) : Vec[T] = Stage.this(stageable, key.value)
  implicit def stageablePipedVec2[T <: Data](stageable: Stageable[Vec[T]]) = new {
    def of(key : Any) = Stage.this.apply(stageable, key)
  }
  implicit def stageablePipedVec3[T <: Data](key: Tuple2[Stageable[Vec[T]], Any]) = Stage.this(key._1, key._2)

  //  implicit def stageablePiped2[T <: Data](stageable: Stageable[T]) = new DataPimper(Stage.this(stageable))
  def haltIt()(implicit loc: Location) : Unit = haltIt(ConditionalContext.isTrue)
  def throwIt()(implicit loc: Location) : Unit = throwIt(ConditionalContext.isTrue)
  def forkIt()(implicit loc: Location) : Unit = forkIt(ConditionalContext.isTrue)
  def spawnIt()(implicit loc: Location) : Unit = spawnIt(ConditionalContext.isTrue)
  def flushIt() : Unit = flushIt(ConditionalContext.isTrue)
  def flushNext() : Unit = flushNext(ConditionalContext.isTrue)
  def haltIt(cond : Bool)(implicit loc: Location) : Unit = internals.request.halts += nameFromLocation(CombInit(cond), "haltRequest")
  def haltWhen(cond : Bool)(implicit loc: Location) : Unit = haltIt(cond)
  def throwIt(cond : Bool, root : Boolean = true)(implicit loc: Location) : Unit = {
    internals.request.throws += nameFromLocation(CombInit(cond), "throwRequest")
    if(root) internals.request.throwsRoot += cond
  }
  def forkIt(cond : Bool)(implicit loc: Location) : Unit = internals.request.forks += nameFromLocation(CombInit(cond), "forkRequest")
  def spawnIt(cond : Bool)(implicit loc: Location) : Unit = internals.request.spawns += nameFromLocation(CombInit(cond), "spawnRequest")

  //Not being root will not clear the output valid of the stage, which can be quite usefull
  def flushIt(cond : Bool, root : Boolean = true) : Unit = {
    internals.request.flush += cond
    if(root) internals.request.flushRoot += cond
  }
  def flushNext(cond : Bool) : Unit =  internals.request.flushNext += cond
  def removeIt(): Unit = ???
  def isValid: Bool = internals.input.valid
  def isFireing: Bool = signalCache(this -> "isFireing")(isValid && isReady).setCompositeName(this, "isFireing")
  def isFirstCycle: Bool = {
    val wait = RegInit(False) setWhen(isValid) clearWhen(isReady || isFlushed)
    val ret = isValid && !wait
    signalCache(this -> "isFirstCycle")(ret).setCompositeName(this, "isFirstCycle")
  }



  def isStuck: Bool = isValid && !isReady
  def isChanging:Bool = isReady || isRemoved
  def isRemoved : Bool = {
    if(internals.arbitration.isRemoved == null) internals.arbitration.isRemoved = ContextSwapper.outsideCondScopeData(Bool())
    internals.arbitration.isRemoved
  }
  def isFlushed : Bool = {
    if(internals.arbitration.isFlushed == null) internals.arbitration.isFlushed = ContextSwapper.outsideCondScopeData(Bool())
    internals.arbitration.isFlushed
  }
  //So, not realy well named, as it just check if a transaction is moving to the next stage (output.valid && output.ready)
  def isForked : Bool = {
    if(internals.arbitration.isForked == null) internals.arbitration.isForked = ContextSwapper.outsideCondScopeData(Bool())
    internals.arbitration.isForked
  }
  def isFlushingNext : Bool = {
    if(internals.arbitration.isFlushingNext == null) internals.arbitration.isFlushingNext = ContextSwapper.outsideCondScopeData(Bool())
    internals.arbitration.isFlushingNext
  }
  def isFlushingRoot : Bool = {
    if(internals.arbitration.isFlushingRoot == null) internals.arbitration.isFlushingRoot = ContextSwapper.outsideCondScopeData(Bool())
    internals.arbitration.isFlushingRoot
  }
  def isReady : Bool = {
    if(internals.input.ready == null) internals.input.ready = ContextSwapper.outsideCondScopeData(Bool())
    internals.input.ready
  }
  def isSelfRemoved : Bool = isFlushingRoot
  def isThrown : Bool = {
    if(internals.arbitration.isThrown == null) internals.arbitration.isThrown = ContextSwapper.outsideCondScopeData(Bool())
    internals.arbitration.isThrown
  }

  def valid = internals.input.valid



  def apply(key : StageableKey) : Data = {
    internals.stageableToData.getOrElseUpdate(key, ContextSwapper.outsideCondScopeData{
      key.stageable()//.setCompositeName(this, s"${key}")
    })
  }
  def overloaded(key : StageableKey) : Data = {
    internals.stageableOverloadedToData.getOrElseUpdate(key, ContextSwapper.outsideCondScopeData{
      key.stageable()//.setCompositeName(this, s"${key}_overloaded")
    })
  }
  def resulting(key : StageableKey) : Data = {
    internals.stageableResultingToData.getOrElseUpdate(key, ContextSwapper.outsideCondScopeData{
      key.stageable()//.setCompositeName(this, s"${key}_overloaded")
    })
  }
  def terminal(key : StageableKey) : StageableKey = {
    internals.stageableTerminal += key
    key
  }

  def apply[T <: Data](key : Stageable[T]) : T = {
    apply(StageableKey(key.asInstanceOf[Stageable[Data]], null)).asInstanceOf[T]
  }
  def apply[T <: Data](key : Stageable[T], key2 : Any) : T = {
    apply(StageableKey(key.asInstanceOf[Stageable[Data]], key2)).asInstanceOf[T]
  }

  def apply(keys2 : Seq[Any]) = new OffsetApi(keys2)
  class OffsetApi(keys2 : Seq[Any]){
    def apply[T <: Data](key : Stageable[T]) : Seq[T] = {
      keys2.map(key2 => Stage.this.apply(key, key2))
    }
  }

  def terminal[T <: Data](key : Stageable[T], key2 : Any) : StageableKey = {
    terminal(StageableKey(key.asInstanceOf[Stageable[Data]], key2))
  }
  def overloaded[T <: Data](key : Stageable[T]) : T = {
    overloaded(StageableKey(key.asInstanceOf[Stageable[Data]], null)).asInstanceOf[T]
  }
  def overloaded[T <: Data](key : Stageable[T], key2 : Any) : T = {
    overloaded(StageableKey(key.asInstanceOf[Stageable[Data]], key2)).asInstanceOf[T]
  }

  def resulting[T <: Data](key : Stageable[T]) : T = {
    resulting(StageableKey(key.asInstanceOf[Stageable[Data]], null)).asInstanceOf[T]
  }
  def resulting[T <: Data](key : Stageable[T], key2 : Any) : T = {
    resulting(StageableKey(key.asInstanceOf[Stageable[Data]], key2)).asInstanceOf[T]
  }


  def insert[T <: Data](that : T) : Stageable[T] = {
    val s = Stageable(cloneOf(that))
    this(s) := that
    s
  }

  def insert[T <: Data](that : Stageable[T])(implicit key : StageableOffset = StageableOffsetNone) : Stageable[T] = {
    val s = Stageable(that)
    this(s) := that
    s
  }

  //  def <<(that : Stage) = {
  //
  //    that
  //  }
  //
  //  def <-<(that : Stage) = {
  //    that
  //  }
  //
  ////  def >>(that : Stage) = {
  ////    that
  ////  }
  //  def >>(that : Stage) = new {
  //    def apply(x : Int) = {
  //
  //    }
  //  }
  //
  //  def >->(that : Stage) = {
  //    that
  //  }
}