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

package spinal.tester.scalatest

import java.nio.charset.Charset

import org.scalatest.{BeforeAndAfterAll, ParallelTestExecution, FunSuite}
import spinal.core._

import scala.concurrent.Await
import scala.sys.process._

abstract class SpinalTesterCocotbBase extends FunSuite  {

  var withWaveform = false
  var spinalMustPass = true
  var cocotbMustPass = true

  def doTest: Unit ={
      try {
        backendConfig(SpinalConfig(mode = Verilog)).generate(createToplevel)
      } catch {
        case e: Throwable => {
          assert(!spinalMustPass,"Spinal has fail :(")
          return
        }
      }
      assert(spinalMustPass,"Spinal has not fail :(")

      doCmd(Seq(s"cd $pythonTestLocation", "rm -f results.xml", "make"))
      val pass = getCocotbPass()
      assert(!cocotbMustPass || pass,"Simulation fail")
      assert(cocotbMustPass || !pass,"Simulation has not fail :(")
      postTest
  }

  test(getName + "Verilog") {doTest}




  def doCmd(cmds : Seq[String]): Unit ={
    var out,err : String = null
        val io = new ProcessIO(
          stdin  => {
            for(cmd <- cmds)
              stdin.write((cmd + "\n").getBytes)
            stdin.close()
          },
          stdout => {
            out = scala.io.Source.fromInputStream(stdout).getLines.reduce(_ + "\n" + _)
            stdout.close()
          },
          stderr => {
            err = scala.io.Source.fromInputStream(stderr).getLines.reduce(_ + "\n" + _)
            stderr.close()
          })
        val proc = Process("sh").run(io)
    proc.exitValue()
    println(out)
    println(err)
  }

  def getCocotbPass() : Boolean = {
    import scala.io.Source
    for(line <- Source.fromFile(pythonTestLocation + "/results.xml").getLines()) {
      if (line.contains("failure")){
        return false
      }
    }
    return true
  }

  def postTest : Unit = {}

  def backendConfig(config: SpinalConfig) : SpinalConfig = config
  def getName: String = this.getClass.getName()
  def createToplevel: Component
  def pythonTestLocation : String
}
