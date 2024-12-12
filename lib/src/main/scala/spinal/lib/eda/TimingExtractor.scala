package spinal.lib.eda

import spinal.core._
import spinal.core.internals.{BaseNode, Statement}
import spinal.lib.AnalysisUtils

import java.io.Writer
import scala.collection.mutable

trait TimingExtractorListener{
  def writeFalsePath(target: Any, source : Any, falsePathTag: crossClockFalsePath) : Unit
  def writeMaxDelay(target: Any, source : Any, tag: crossClockMaxDelay): Unit
}

object TimingExtractorPrinter extends TimingExtractorListener{
  def writeFalsePath(target: Any, source : Any, tag: crossClockFalsePath) : Unit =
    println(s"$tag : $source -> $target")
  def writeMaxDelay(target: Any, source : Any, tag: crossClockMaxDelay): Unit =
    println(s"$tag : $source -> $target")
}

object TimingExtractor {
  def apply[C <: Component](report : SpinalReport[C], listener : TimingExtractorListener): Unit = apply(report.toplevel, listener)
  def apply(top : Component, listener : TimingExtractorListener): Unit = {
    top.walkComponents(cc => cc.dslBody.walkStatements{
      case s: SpinalTagReady => s.foreachTag{
        case tag: crossClockFalsePath =>
          writeFalsePath(s, tag, listener)
        case tag: crossClockMaxDelay =>
          writeMaxDelay(s, tag, listener)
        case _ =>
      }
      case _ =>
    })
  }

  def writeMaxDelay(target: Statement, tag: crossClockMaxDelay, listener : TimingExtractorListener): Unit = {
    var sources = mutable.LinkedHashSet[Any]()
    target match {
      case target : BaseType => AnalysisUtils.seekNonCombDrivers (target) (sources.add)
    }

    if (sources.isEmpty) println(s"??? no source found for $target while writeMaxDelay")
    for(source <- sources) listener.writeMaxDelay(target, source, tag)
  }

  def writeFalsePath(target: Statement, falsePathTag: crossClockFalsePath, listener : TimingExtractorListener): Unit = {
    val resetIsDriver = falsePathTag.destType == TimingEndpointType.RESET

    var sources = mutable.LinkedHashSet[Any]()
    target match {
      case target : BaseType => {
        falsePathTag.destType match {
          case TimingEndpointType.DATA => AnalysisUtils.seekNonCombDrivers(target) (sources.add)
          case TimingEndpointType.RESET => {
            assert(target.isReg)
            if(target.hasInit) {
              if (target.clockDomain.reset != null) AnalysisUtils.seekNonCombDriversFromSelf(target.clockDomain.reset)(sources.add)
              if (target.clockDomain.softReset != null) AnalysisUtils.seekNonCombDriversFromSelf(target.clockDomain.softReset)(sources.add)
            }
          }
        }
      }
    }

    if(sources.isEmpty) println(s"??? no source found for $target while writeFalsePath")
    for(source <- sources) listener.writeFalsePath(target, source, falsePathTag)
  }
}

