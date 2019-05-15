package spinal.debugger


import net.liftweb.json
import net.liftweb.json.Extraction._
import net.liftweb.json.JsonAST._
import net.liftweb.json.Printer._
import net.liftweb.json.{DefaultFormats, Formats, TypeInfo}
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object LogicAnalyserBuilder {
  def apply() = new LogicAnalyserParameter
}

object LogicAnalyser {
  def waitTriggerHeader = 0x01
  def userTriggerHeader = 0x02
  def configsHeader = 0x0F

  def jsonSerDes = Seq(Probe , LogicAnalyserParameter,ExTrigger)
}


object Probe extends net.liftweb.json.Serializer[Probe] {

  def apply(baseType: BaseType): Probe = apply("", baseType)

  def apply(name: String, baseType: BaseType): Probe = {
    val ret = new Probe
    ret.name = name
    ret.baseType = baseType
    ret
  }



  private val Class = classOf[Probe]

  override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, json.JValue), Probe] = {
    case (TypeInfo(Class, _), json) =>
      val probe = new Probe
      probe.fromJson(json)
      //a.street = (json \ "street").extract[String]
      probe
  }

  override def serialize(implicit format: Formats): PartialFunction[Any, json.JValue] = {
    case x: Probe =>
      x.toJson
    //JObject(JField("street" JString(x.street) :: Nil)
  }
}

class Probe {
  var name = ""
  var scope = ArrayBuffer[String]()
  var kind = ""
  var width = -1
  var baseType: BaseType = null


  def postBackend: Unit = {
    if (kind == "") kind = baseType.getClass.getSimpleName
    if (name == "") name = baseType.getName()
    width = baseType.getBitsWidth
    // scope = (baseType.component.parents() ++ List(baseType.component)).map(_.getName()).reduceLeft(_ + " " + _)
    scope ++= (baseType.component.parents() ++ List(baseType.component)).map(_.getName())
  }

  implicit val formats = DefaultFormats ++ LogicAnalyser.jsonSerDes

  import net.liftweb.json.JsonDSL._

  def fromJson(json: JValue): Unit = {
    name = (json \ "name").extract[String]
    scope ++= (json \ "scope").children.map(_.extract[String])
    kind = (json \ "kind").extract[String]
    width = (json \ "width").extract[Int]
  }

  def toJson: JValue = {
    ("name" -> name) ~
      ("scope" -> scope) ~
      ("kind" -> kind) ~
      ("width" -> width)
  }
}
object ExTrigger extends net.liftweb.json.Serializer[ExTrigger] {

  def apply(trigger: Bool): ExTrigger = apply("", trigger)

  def apply(name: String, trigger: Bool): ExTrigger = {
    val ret = new ExTrigger
    ret.name = name
    ret.trigger = trigger
    ret
  }



  private val Class = classOf[ExTrigger]

  override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, json.JValue), ExTrigger] = {
    case (TypeInfo(Class, _), json) =>
      val exTrigger = new ExTrigger
      exTrigger.fromJson(json)
      exTrigger
  }

  override def serialize(implicit format: Formats): PartialFunction[Any, json.JValue] = {
    case x: ExTrigger =>
      x.toJson
  }
}

class ExTrigger {
  var name = ""
  var scope = ArrayBuffer[String]()
  var trigger: Bool = null


  def postBackend: Unit = {
    if (name == "") name = trigger.getName()
    scope ++= (trigger.component.parents() ++ List(trigger.component)).map(_.getName())
  }

  implicit val formats = DefaultFormats ++ LogicAnalyser.jsonSerDes

  import net.liftweb.json.JsonDSL._

  def fromJson(json: JValue): Unit = {
    name = (json \ "name").extract[String]
    scope ++= (json \ "scope").children.map(_.extract[String])
  }

  def toJson: JValue = {
    ("name" -> name) ~
      ("scope" -> scope)
  }
}


object LogicAnalyserParameter extends net.liftweb.json.Serializer[LogicAnalyserParameter] {
  private val Class = classOf[LogicAnalyserParameter]


  override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, json.JValue), LogicAnalyserParameter] = {
    case (TypeInfo(Class, _), json) =>
      val x = new LogicAnalyserParameter
      x.fromJson(json)
      x
  }

  override def serialize(implicit format: Formats): PartialFunction[Any, json.JValue] = {
    case x: LogicAnalyserParameter =>
      x.toJson
  }
}




class LogicAnalyserParameter {
  var memAddressWidth = 8
  val probes = ArrayBuffer[Probe]()
  var uid: Int = Random.nextInt()
  var zeroSampleLeftAfterTriggerWindow = 2
  val exTriggers = ArrayBuffer[ExTrigger]()


  def exTrigger(trigger : Bool): LogicAnalyserParameter = {
    exTriggers += ExTrigger(trigger)
    this
  }
  def probe (baseType : BaseType): LogicAnalyserParameter = {
    probes += Probe(baseType)
    this
  }
  def setSampleCount(sampleCount : Int) : LogicAnalyserParameter = {
    memAddressWidth = log2Up(sampleCount)
    this
  }


  def build = new LogicAnalyser(this)

  def postBackend: Unit = {
    probes.foreach(_.postBackend)
    exTriggers  .foreach(_.postBackend)
    implicit val formats = DefaultFormats ++ LogicAnalyser.jsonSerDes
    val json = toJson
    GlobalData.get.addJsonReport(prettyRender(json))
  }

  def memAddressCount = 1 << memAddressWidth


  implicit val formats = DefaultFormats ++ LogicAnalyser.jsonSerDes

  import net.liftweb.json.JsonDSL._


  def fromJson(json: JValue): Unit = {
    uid = (json \ "uid").extract[String].toInt
    memAddressWidth = (json \ "memAddressWidth").extract[Int]
    zeroSampleLeftAfterTriggerWindow = (json \ "zeroSampleLeftAfterTriggerWindow").extract[Int]
    probes ++= (json \ "probes").children.map(_.extract[Probe])
    exTriggers ++= (json \ "exTriggers").children.map(_.extract[ExTrigger])
  }

  def toJson: JValue = {
    ("clazz" -> "uidPeripheral") ~
      ("kind" -> "logicAnalyser") ~
      ("uid" -> uid.toString) ~
      ("memAddressWidth" -> memAddressWidth)~
      ("zeroSampleLeftAfterTriggerWindow" -> zeroSampleLeftAfterTriggerWindow)~
      ("probes" -> decompose(probes))~
    ("exTriggers" -> decompose(exTriggers))
  }
}



case class LogicAnalyserConfig(p: LogicAnalyserParameter) extends Bundle {
  val trigger = new Bundle {
    val delay = UInt(32 bit)
  }
  val logger = new Bundle {
    val samplesLeftAfterTrigger = UInt(p.memAddressWidth bit)
  }
}


class LogicAnalyser(p: LogicAnalyserParameter) extends Component {
  import LogicAnalyser._

  val fragmentWidth = 8
  val io = new Bundle {
    val slavePort = slave Flow Fragment(Bits(fragmentWidth bit))
    val masterPort = master Stream Fragment(Bits(fragmentWidth bit))
  }

  val probes = Cat(p.probes.map(_.baseType.pull()))
  val exTriggers = p.exTriggers.reverse.map(_.trigger.pull())


  val waitTrigger = io.slavePort filterHeader (waitTriggerHeader) toRegOf (Bool) init (False)
  val userTrigger = io.slavePort pulseOn (userTriggerHeader)
  val configs = io.slavePort filterHeader (configsHeader) toRegOf(new LogicAnalyserConfig(p), false)
  val passportEvent = io.slavePort eventOn (0xFF)

  val trigger = new Area {
    val aggregate = exTriggers.foldLeft(False)(_ || _) || userTrigger   //CounterFreeRun(1000) === U(999)
    when(waitTrigger && aggregate) {
      waitTrigger := False
    }
    val event = DelayEvent(aggregate && waitTrigger, configs.trigger.delay)
  }


  val logger = new LogicAnalyserLogger(p, probes)
  logger.io.configs := configs
  logger.io.trigger := trigger.event
  logger.io.probe := probes


  val passport = passportEvent.translateWith(S(p.uid, 32 bit)).fragmentTransaction(fragmentWidth)
  val logs = logger.io.log.toFragmentBits(fragmentWidth)


  io.masterPort << StreamFragmentArbiter(Bits(fragmentWidth bit))(Seq(
    passport,
    logs
  ))

  globalData.addPostBackendTask({
    p.postBackend
  })
}


object LogicAnalyserLoggerState extends SpinalEnum {
  val sWaitTrigger, sSample, sPush = newElement()
}

class LogicAnalyserLogger(p: LogicAnalyserParameter, probeType: Bits) extends Component {

  import LogicAnalyserLoggerState._

  val io = new Bundle {
    val configs = in(new LogicAnalyserConfig(p))

    val trigger = in Bool
    val probe = in cloneOf (probeType)

    val log = master Stream Fragment(probe)
  }

  val mem = Mem(probeType, 1 << p.memAddressWidth)
  val memWriteAddress = Reg(mem.addressType) randBoot
  val memReadAddress = Reg(mem.addressType)


  val state = RegInit(sWaitTrigger)
  val pushCounter = Reg(mem.addressType)


  val sampler = new Area {
    val preEnable = False
    val postEnable = False
    val counter = Reg(mem.addressType)

    when(postEnable) {
      counter := counter - 1
    } otherwise {
      counter := io.configs.logger.samplesLeftAfterTrigger
    }

    when(preEnable || postEnable) {
      mem(memWriteAddress) := io.probe
      memWriteAddress := memWriteAddress + 1
    }

    val done = counter === 0
  }


  val memReadCmd = Stream(mem.addressType)
  val memReadCmdIsLast = False

  memReadCmd.valid := False
  memReadCmd.payload := memReadAddress



  switch(state) {
    is(sWaitTrigger) {
      sampler.preEnable := True
      when(io.trigger) {
        state := sSample
        memReadAddress := memWriteAddress + io.configs.logger.samplesLeftAfterTrigger + 2
      }
    }
    is(sSample) {
      sampler.postEnable := True
      when(sampler.done) {
        state := sPush
        pushCounter := 0
      }
    }
    is(sPush) {
      memReadCmd.valid := True
      when(memReadCmd.ready) {
        memReadAddress := memReadAddress + 1
        pushCounter := pushCounter + 1
      }
      when(pushCounter === (1 << pushCounter.getWidth) - 1) {
        memReadCmdIsLast := True
        when(memReadCmd.ready) {
          state := sWaitTrigger
        }
      }
    }
  }

  val memReadPort = mem.streamReadSync(memReadCmd, memReadCmdIsLast)
  io.log.translateFrom(memReadPort)((to, from) => {
    to.last := from.linked
    to.fragment := from.value
  })

}