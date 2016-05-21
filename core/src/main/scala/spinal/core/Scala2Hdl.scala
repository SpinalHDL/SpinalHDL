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

object SpinalVhdl {

  val runtime = Runtime.getRuntime
  println({
    SpinalLog.tag("Runtime", Console.YELLOW)
  } + s" JVM max memory : ${f"${(runtime.maxMemory()).toFloat / 1048576f}%1.1f"}MiB")

  val curDate: Date = new Date()
  val dateFmt: SimpleDateFormat = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss")
  println({
    SpinalLog.tag("Runtime", Console.YELLOW)
  } + s" Current date : ${dateFmt.format(curDate)}")

  def apply[T <: Component](gen: => T,
                            defaultConfigForClockDomains: ClockDomainConfig = ClockDomainConfig(),
                            onlyStdLogicVectorAtTopLevelIo : Boolean = false): BackendReport[T] = {
    val builder = SpinalVhdlBuilder(gen)
    builder.setDefaultConfigForClockDomains(defaultConfigForClockDomains)
    if(onlyStdLogicVectorAtTopLevelIo) builder.onlyStdLogicVectorAtTopLevelIo()
    builder.elaborate()
  }

  //Depreciated
  def apply[T <: Component](gen: => T, config: SpinalVhdlBuilder[_] => Unit): BackendReport[T] = {
    val factory = new SpinalVhdlBuilder(gen)
    config(factory)
    factory.elaborate()
  }

}


object SpinalVhdlBuilder {
  def apply[T <: Component](gen: => T): SpinalVhdlBuilder[T] = new SpinalVhdlBuilder(gen)
}

class SpinalVhdlBuilder[T <: Component](gen: => T) {
  val backend = new VhdlBackend
  val tbGen = new VhdlTestBenchBackend()

  def elaborate() : BackendReport[T]= {
    try {
      val report = backend.elaborate(() => gen)
      tbGen.elaborate(backend, report.toplevel)
      println({SpinalLog.tag("Done", Console.GREEN)} + s" at ${f"${Driver.executionTime}%1.3f"}")
      return report
    } catch {
      case e: Throwable => {
        if(!GlobalData.get.scalaLocatedEnable){
          Thread.sleep(10);
          println("\n**********************************************************************************************")
          val errCnt = SpinalError.getErrorCount()
          SpinalWarning(s"Elaboration failed (${errCnt} error" + (if(errCnt > 1){s"s"} else {s""}) + s").\n" +
            s"          Spinal will restart with scala trace to help you to find the problem.")
          println("**********************************************************************************************\n")
          Thread.sleep(10);
          GlobalData.get.scalaLocatedEnable = true
          return elaborate()
        }else{
          Thread.sleep(10);
          println("\n**********************************************************************************************")
          val errCnt = SpinalError.getErrorCount()
          SpinalWarning(s"Elaboration failed (${errCnt} error" + (if(errCnt > 1){s"s"} else {s""}) + ").")
          println("**********************************************************************************************")
          Thread.sleep(10);
          throw e
        }
      }
    }
  }



  def setDefaultConfigForClockDomains(config: ClockDomainConfig): this.type = {
    backend.globalData.commonClockConfig = config
    this
  }

  def setDefaultClockFrequency(frequency : IClockDomainFrequency): this.type  ={
    backend.defaultClockDomainFrequency = frequency
    this
  }

  def setSpinalPackage(name: String): this.type = {
    backend.packageName = name
    this
  }

  def setEnumPackage(name: String): this.type = {
    backend.enumPackageName = name
    this
  }
  def onlyStdLogicVectorAtTopLevelIo() : this.type = {
    backend.onlyStdLogicVectorTopLevelIo = true
    this
  }
  def setOutputFile(path: String): this.type = {
    backend.outputFilePath = path
    this
  }


  def forceMemToBlackboxTranslation: this.type = {
    backend.forceMemToBlackboxTranslation = true
    this
  }

  def setTbOutputFile(name: String): this.type = {
    tbGen.outputFile = name
    this
  }

  def setTbName(name: String): this.type = {
    tbGen.tbName = name
    this
  }

  def nothing: this.type = {
    this
  }
}

