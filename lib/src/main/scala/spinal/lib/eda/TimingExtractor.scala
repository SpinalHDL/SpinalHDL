package spinal.lib.eda

import spinal.core
import spinal.core._
import spinal.core.internals.{AssertStatement, AssignmentStatement, BaseNode, Statement, SwitchStatement, WhenStatement}
import spinal.lib.{AnalysisUtils, FlowCCUnsafeByToggle, StreamCCByToggle, StreamFifoCC}

import java.io.{File, PrintWriter, Writer}
import scala.collection.mutable

trait TimingExtractorListener{
  def writeClock(clock: Bool, frequency : ClockDomain.ClockFrequency): Unit
  def writeFalsePath(target: Any, falsePathTag: crossClockFalsePath) : Unit
  def writeMaxDelay(target: Any, source : Any, tag: crossClockMaxDelay): Unit
  def writeInputDelay(bt: BaseType): Unit
  def writeOutputDelay(bt: BaseType): Unit
  def close(): Unit
}

object TimingExtractorPrinter extends TimingExtractorListener{
  def writeFalsePath(target: Any, tag: crossClockFalsePath) : Unit =
    println(s"$tag : ${tag.source} -> $target")
  def writeMaxDelay(target: Any, source : Any, tag: crossClockMaxDelay): Unit =
    println(s"$tag : $source -> $target")
  def close(): Unit = {}

  override def writeClock(clock: Bool, frequency: core.IClockDomainFrequency): Unit = println(s"Clock $clock at $frequency")
  override def writeInputDelay(bt: BaseType): Unit = println(s"input delay $bt")
  override def writeOutputDelay(bt: BaseType): Unit = println(s"output delay $bt")
}

class TimingExtractorSdc(path : File, marginFactor : Double) extends TimingExtractorListener{
  val o = new PrintWriter(path)
  def pathOf(that : Any) : String = that match{
    case bt : BaseType => bt.getRtlPath()
  }

  def freqToTimeSingle(cd : ClockDomain) = (cd.frequency match {
    case f : FixedFrequency => f.getValue.toTime.toBigDecimal * 1e9
    case _ => SpinalError("Unknown frequancy for " + cd.clock)
  })*marginFactor

  def writeFalsePath(target: Any, tag: crossClockFalsePath) : Unit = {
    def pathOf(that : Any) : String = that match{
      case bt : BaseType => bt.getRtlPath()
    }
    val sourceStr = tag.source.map(e => s"-from ${pathOf(e)}")
    val dst = tag.destType match {
      case TimingEndpointType.DATA => "DATA"
      case TimingEndpointType.RESET => "RESET"
      case TimingEndpointType.CLOCK_EN => ???
    }
    val targeteStr = pathOf(target) + "/" + dst
    o.println(f"set_false_path ${sourceStr.getOrElse("")} -to $targeteStr")
  }
  def writeMaxDelay(target: Any, source : Any, tag: crossClockMaxDelay): Unit = {
    val timeAny = tag.useTargetClock.mux(target, source)
    val cd = TimingExtractor.clockDomainOf(timeAny)
    val time = freqToTimeSingle(cd)
    assert(time > 0)
    val sourceStr = pathOf(source)
    val targeteStr = pathOf(target)
    o.println(f"set_max_delay $time%5.3f -from $sourceStr -to $targeteStr")
  }

  override def writeInputDelay(bt: BaseType): Unit = {
    bt.getTag(classOf[ClockDomainReportTag]) match {
      case Some(tag) => o.println(s"set_input_delay -clock ${pathOf(tag.clockDomain.clock)} -max ??? ${pathOf(bt)}")
      case _ => println(s"no input delay for $bt")
    }
  }
  override def writeOutputDelay(bt: BaseType): Unit = {
    bt.getTag(classOf[ClockDomainReportTag]) match {
      case Some(tag) => o.println(s"set_output_delay -clock ${pathOf(tag.clockDomain.clock)} -min ??? ${pathOf(bt)}")
      case _ => println(s"no output delay for $bt")
    }
  }


  override def writeClock(clock: Bool, frequency: core.IClockDomainFrequency): Unit = {
    val freqStr = frequency match{
      case f : FixedFrequency => f"${1e9/f.getValue.toDouble}%5.3f"
    }
    o.println(s"create_clock -period ${freqStr} ${pathOf(clock)}")
  }

  def close(): Unit = {
    o.flush()
    o.close()
  }
}

object TimingExtractor {
  def apply[C <: Component](report : SpinalReport[C], listener : TimingExtractorListener): Unit = apply(report.toplevel, listener)
  def apply(top : Component, listener : TimingExtractorListener): Unit = {
    val cds = mutable.LinkedHashSet[ClockDomain]()
    val clks = mutable.LinkedHashMap[Bool, mutable.LinkedHashSet[ClockDomain]]()
    top.walkComponents{ c =>
      c.dslBody.walkStatements{ s =>
        clockDomainOptionOf(s) match {
          case Some(cd) => cds += cd //clks.getOrElseUpdate(cd.clock, mutable.LinkedHashSet[ClockDomain]()) += cd
          case None =>
        }
      }
    }
    for(cd <- cds){
      val clock = AnalysisUtils.solveCombDriver(cd.clock)
      clks.getOrElseUpdate(clock, mutable.LinkedHashSet[ClockDomain]()) += cd
    }
    for((clock, cds) <- clks){
      val frequencies = mutable.LinkedHashSet[ClockDomain.ClockFrequency]()
      frequencies ++= cds.map(_.frequency)
      if(frequencies.size > 1) {
        val filtred = frequencies.filter(!_.isInstanceOf[ClockDomain.UnknownFrequency])
        frequencies.clear(); frequencies ++= filtred
      }
      assert(frequencies.size == 1)
      listener.writeClock(clock, frequencies.head)
    }
    top.walkComponents(cc => cc.dslBody.walkStatements{
      case s: SpinalTagReady => s.foreachTag{
        case tag: crossClockFalsePath =>
          crossClockFalsePath(s, tag, listener)
        case tag: crossClockMaxDelay =>
          crossClockMaxDelay(s, tag, listener)
        case _ =>
      }
      case _ =>
    })
    top.getAllIo.foreach {
      case bt if bt.isInput => listener.writeInputDelay(bt)
      case bt if bt.isOutput => listener.writeOutputDelay(bt)
    }
    listener.close()
  }

  def clockDomainOf(that : Any) : ClockDomain = that match{
    case bt : BaseType if bt.isReg => bt.clockDomain
    case bt : BaseType if bt.isComb => bt.getTag(classOf[ClockDomainTag]) match {
      case Some(tag) => tag.clockDomain
    }
    case p : MemReadSync => p.clockDomain
  }
  def clockDomainOptionOf(that : Any) : Option[ClockDomain] = that match{
    case s : AssignmentStatement => None
    case bt : BaseType if bt.isReg => Some(bt.clockDomain)
    case bt : BaseType if bt.isComb => bt.getTag(classOf[ClockDomainTag]) match {
      case Some(tag) => Some(tag.clockDomain)
      case _ => None
    }
    case bt : BaseType => None
    case s : WhenStatement => None
    case s : SwitchStatement => None
    case s : Mem[_] => None
    case p : MemReadSync => Some(p.clockDomain)
    case p : MemReadWrite => Some(p.clockDomain)
    case p : MemWrite => Some(p.clockDomain)
    case p : AssertStatement => Some(p.clockDomain)
  }

  def isCrossClock(target : Statement, source : Any): Boolean = {
    val cdTarget = clockDomainOf(target)
    val cdSource = clockDomainOf(source)
    if(cdTarget == null || cdSource == null) return true
    !ClockDomain.areSynchronous(cdTarget, cdSource)
  }

  def crossClockMaxDelay(target: Statement, tag: crossClockMaxDelay, listener : TimingExtractorListener): Unit = {
    var sources = mutable.LinkedHashSet[Any]()
    target match {
      case target : BaseType => AnalysisUtils.seekNonCombDrivers (target) (sources.add)
      case target : MemReadSync => AnalysisUtils.seekNonCombDrivers (target) (sources.add)
    }

    if (sources.isEmpty) println(s"??? no source found for $target while writeMaxDelay")
    for(source <- sources; if isCrossClock(target, source)) listener.writeMaxDelay(target, source, tag)
  }

  def crossClockFalsePath(target: Statement, falsePathTag: crossClockFalsePath, listener : TimingExtractorListener): Unit = {
    listener.writeFalsePath(target, falsePathTag)
//    val resetIsDriver = falsePathTag.destType == TimingEndpointType.RESET
//
//    var sources = mutable.LinkedHashSet[Any]()
//    target match {
//      case target : BaseType => {
//        falsePathTag.destType match {
//          case TimingEndpointType.DATA => AnalysisUtils.seekNonCombDrivers(target) (sources.add)
//          case TimingEndpointType.RESET => {
//            assert(target.isReg)
//            if(target.hasInit) {
//              if (target.clockDomain.reset != null) AnalysisUtils.seekNonCombDriversFromSelf(target.clockDomain.reset)(sources.add)
//              if (target.clockDomain.softReset != null) AnalysisUtils.seekNonCombDriversFromSelf(target.clockDomain.softReset)(sources.add)
//            }
//          }
//        }
//      }
//    }

//    if(sources.isEmpty) println(s"??? no source found for $target while writeFalsePath")
//    for(source <- sources; if isCrossClock(target, source)) listener.writeFalsePath(target, source, falsePathTag)
  }
}


object TimingExtractorDemo extends App {
  ClockDomain.crossClockBufferPushToPopResetGen.set(false)
  val report = SpinalVerilog(new Component{
    val cdA = ClockDomain.external("cdA")
    val cdB = ClockDomain.external("cdB")

    val flowCc = new FlowCCUnsafeByToggle(UInt(8 bits), cdA, cdB)
    val flowCcIo = flowCc.io.toIo

    val streamToggleCc = new StreamCCByToggle(UInt(8 bits), cdA, cdB)
    val streamToggleCcIo = streamToggleCc.io.toIo

    val streamFifoCc = new StreamFifoCC(UInt(8 bits), 128, cdA, cdB)
    val streamFifoCcIo = streamFifoCc.io.toIo
  })
  TimingExtractor(report, TimingExtractorPrinter)
}
