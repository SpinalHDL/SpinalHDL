/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core


import spinal.core.internals._

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


//TODO
// undefined

trait SpinalMode
object VHDL    extends SpinalMode
object Verilog extends SpinalMode


case class DumpWaveConfig(depth: Int = 0, vcdPath: String = "wave.vcd")


/**
 * target device
 */
case class Device(vendor: String = "?", family: String = "?", name: String = "?")


trait MemBlackboxingPolicy {
  def translationInterest(topology: MemTopology): Boolean

  def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit

  def generateUnblackboxableError(topology: MemTopology, who: Any, message: String): Unit = {
    PendingError(s"${this.getClass} is not able to blackbox ${topology.mem}\n  write ports : ${topology.writes.size} \n  readAsync ports : ${topology.readsAsync.size} \n  readSync ports : ${topology.readsSync.size} \n  readRrite ports : ${topology.readWriteSync.size}\n  -> $message")
  }
}


object blackboxAllWhatsYouCan extends MemBlackboxingPolicy {
  override def translationInterest(topology: MemTopology): Boolean = true

  override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = {}
}


object blackboxAll extends MemBlackboxingPolicy {
  override def translationInterest(topology: MemTopology): Boolean = true

  override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = generateUnblackboxableError(topology, who, message)
}


object blackboxRequestedAndUninferable extends MemBlackboxingPolicy {

  override def translationInterest(topology: MemTopology): Boolean = {
    if(blackboxOnlyIfRequested.translationInterest(topology))                      return true
    if(topology.readsAsync.exists(_.readUnderWrite != writeFirst))                 return true
    if(topology.readsSync.exists(_.readUnderWrite != readFirst))                   return true
    if(topology.writeReadSameAddressSync.exists(_._2.readUnderWrite != readFirst)) return true
//    if(topology.readWriteSync.exists(_._2.readUnderWrite != readFirst)) return true
    return false
  }

  override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = generateUnblackboxableError(topology, who, message)
}


object blackboxOnlyIfRequested extends MemBlackboxingPolicy{
  override def translationInterest(topology: MemTopology): Boolean = {
    topology.mem.forceMemToBlackboxTranslation
  }

  override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = generateUnblackboxableError(topology, who, message)
}


/**
 * Spinal configuration for the generation of the RTL 
 */
case class SpinalConfig(
  mode                           : SpinalMode = null,
  debug                          : Boolean = false,
  keepAll                        : Boolean = false,
  defaultConfigForClockDomains   : ClockDomainConfig = ClockDomainConfig(),
  onlyStdLogicVectorAtTopLevelIo : Boolean = false,
  defaultClockDomainFrequency    : IClockDomainFrequency = UnknownFrequency(),
  targetDirectory                : String = ".",
  oneFilePerComponent            : Boolean = false,
  netlistFileName                : String = null,
  dumpWave                       : DumpWaveConfig = null,
  globalPrefix                   : String = "",
  anonymSignalPrefix             : String = null,
  device                         : Device = Device(),
  genVhdlPkg                     : Boolean = true,
  mergeAsyncProcess              : Boolean = true,
  asyncResetCombSensitivity      : Boolean = false,
  phasesInserters                : ArrayBuffer[(ArrayBuffer[Phase]) => Unit] = ArrayBuffer[(ArrayBuffer[Phase]) => Unit](),
  transformationPhases           : ArrayBuffer[Phase] = ArrayBuffer[Phase](),
  memBlackBoxers                 : ArrayBuffer[Phase] = ArrayBuffer[Phase](/*new PhaseMemBlackBoxerDefault(blackboxNothing)*/)
){

  def generate       [T <: Component](gen: => T): SpinalReport[T] = Spinal(this)(gen)
  def generateVhdl   [T <: Component](gen: => T): SpinalReport[T] = Spinal(this.copy(mode = VHDL))(gen)
  def generateVerilog[T <: Component](gen: => T): SpinalReport[T] = Spinal(this.copy(mode = Verilog))(gen)

  def apply[T <: Component](gen : => T): SpinalReport[T] = {
    if(memBlackBoxers.isEmpty)
      addStandardMemBlackboxing(blackboxOnlyIfRequested)
    Spinal(this)(gen)
  }

  def applyToGlobalData(globalData: GlobalData): Unit = {
    globalData.scalaLocatedEnable = debug
    globalData.commonClockConfig  = defaultConfigForClockDomains
  }

  def dumpWave(depth: Int = 0, vcdPath: String = "wave.vcd"): SpinalConfig = this.copy(dumpWave=DumpWaveConfig(depth,vcdPath))

  def addTransformationPhase(phase: Phase): SpinalConfig = {
    transformationPhases += phase
    this
  }

  def addStandardMemBlackboxing(policy: MemBlackboxingPolicy): this.type = {
    memBlackBoxers += new PhaseMemBlackBoxingDefault(policy)
    this
  }
}


object SpinalConfig{

  def shell[T <: Component](args: Seq[String]): SpinalConfig = {
    val parser = new scopt.OptionParser[SpinalConfig]("SpinalCore") {
      opt[Unit]("vhdl")                   action { (_, c) => c.copy(mode = VHDL)         } text("Select the VHDL mode")
      opt[Unit]("verilog")                action { (_, c) => c.copy(mode = Verilog)      } text("Select the Verilog mode")
      opt[Unit]('d', "debug")             action { (_, c) => c.copy(debug = true)        } text("Enter in debug mode directly")
      opt[String]('o', "targetDirectory") action { (v, c) => c.copy(targetDirectory = v) } text("Set the target directory")
    }

    parser.parse(args, SpinalConfig()) match {
      case Some(config) => config
      case None         => ???
    }
  }
}


/**
 * Spinal report give after the generation of the RTL
 */
class SpinalReport[T <: Component]() {
  var toplevel: T = null.asInstanceOf[T]
  val prunedSignals   = mutable.Set[BaseType]()
  val unusedSignals   = mutable.Set[BaseType]()
  var counterRegister = 0
  var rtlSourcesPaths = ArrayBuffer[String]()
  var toplevelName : String = null

  def printUnused() : this.type = {
    unusedSignals.foreach(bt => SpinalWarning(s"Unused wire detected : $bt"))
    this
  }

  def printPruned() : this.type = {
    prunedSignals.foreach(bt => SpinalWarning(s"Pruned wire detected : $bt"))
    this
  }

  def printPrunedIo() : this.type = {
    prunedSignals.filter(_.dir != null).foreach(bt => SpinalWarning(s"Pruned wire detected : $bt"))
    this
  }
}


object Spinal{
  def version = spinal.core.Info.version

  def apply[T <: Component](config: SpinalConfig)(gen: => T): SpinalReport[T] = {

    println({
      SpinalLog.tag("Runtime", Console.YELLOW)
    } + s" SpinalHDL v$version    git head : ${spinal.core.Info.gitHash}")


    val runtime = Runtime.getRuntime
    println({
      SpinalLog.tag("Runtime", Console.YELLOW)
    } + s" JVM max memory : ${f"${(runtime.maxMemory()).toFloat / 1048576f}%1.1f"}MiB")

    val curDate: Date = new Date()
    val dateFmt: SimpleDateFormat = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss")
    println({
      SpinalLog.tag("Runtime", Console.YELLOW)
    } + s" Current date : ${dateFmt.format(curDate)}")

    val report = config.mode match {
      case `VHDL`    => SpinalVhdlBoot(config)(gen)
      case `Verilog` => SpinalVerilogBoot(config)(gen)
    }

    println({SpinalLog.tag("Done", Console.GREEN)} + s" at ${f"${Driver.executionTime}%1.3f"}")
    report
  }
}


object SpinalVhdl {
  def apply[T <: Component](config: SpinalConfig)(gen: => T): SpinalReport[T] = Spinal(config.copy(mode = VHDL))(gen)
  def apply[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig(mode = VHDL).generate(gen)
}


object SpinalVerilog {
  def apply[T <: Component](config: SpinalConfig)(gen: => T): SpinalReport[T] = Spinal(config.copy(mode = Verilog))(gen)
  def apply[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig(mode = Verilog).generate(gen)
}

