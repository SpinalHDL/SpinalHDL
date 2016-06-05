/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal.core

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.mutable


trait SpinalMode
object VHDL extends SpinalMode


case class SpinalConfig(
  mode: SpinalMode = null,
  debug : Boolean = false,
  forceMemToBlackboxTranslation : Boolean = false,
  defaultConfigForClockDomains: ClockDomainConfig = ClockDomainConfig(),
  onlyStdLogicVectorAtTopLevelIo : Boolean = false,
  defaultClockDomainFrequency : IClockDomainFrequency = UnknownFrequency(),
  targetDirectory : String = "."
){
    def generate[T <: Component](gen : => T) : SpinalReport[T] = Spinal(this)(gen)
    def apply[T <: Component](gen : => T) : SpinalReport[T] = Spinal(this)(gen)
}

object SpinalConfig{
  def shell[T <: Component](args : Seq[String]): Unit = {
    val parser = new scopt.OptionParser[SpinalConfig]("SpinalCore") {
      head("Spinal core")
      opt[Unit]("vhdl") action { (_, c) => c.copy(mode = VHDL) }text("Set the VHDL mode")
      opt[Unit]('d', "debug") action { (_, c) => c.copy(debug = true) } text("Enable debug mode directly")
    }

    parser.parse(args, SpinalConfig()) match {
      case Some(config) => config
      case None => ???
    }
  }
}

class SpinalReport[T <: Component](val toplevel: T) {
  val prunedSignals = mutable.Set[BaseType]()

  def printPruned() : this.type = {
    prunedSignals.foreach(bt => SpinalWarning(s"Unused wire detected : $bt"))
    this
  }

  def printPrunedIo() : this.type = {
    prunedSignals.filter(_.dir != null).foreach(bt => SpinalWarning(s"Unused wire detected : $bt"))
    this
  }
}

object Spinal{
  def apply[T <: Component](config : SpinalConfig)(gen : => T) : SpinalReport[T]  = {
    val runtime = Runtime.getRuntime
    println({
      SpinalLog.tag("Runtime", Console.YELLOW)
    } + s" JVM max memory : ${f"${(runtime.maxMemory()).toFloat / 1048576f}%1.1f"}MiB")

    val curDate: Date = new Date()
    val dateFmt: SimpleDateFormat = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss")
    println({
      SpinalLog.tag("Runtime", Console.YELLOW)
    } + s" Current date : ${dateFmt.format(curDate)}")

    config.mode match {
      case `VHDL` => SpinalVhdlBoot(config)(gen)
    }
  }


  def apply[T <: Component](args : String*)(gen : => T) : Unit = seq(args)(gen)
  def seq[T <: Component](args : Seq[String])(gen : => T) : Unit = {

  }
}

object SpinalVhdl {
  def apply[T <: Component](config : SpinalConfig)(gen: => T) : SpinalReport[T] =
    Spinal(config.copy(mode=VHDL))(gen)

  def apply[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig(mode = VHDL).generate(gen)
}

