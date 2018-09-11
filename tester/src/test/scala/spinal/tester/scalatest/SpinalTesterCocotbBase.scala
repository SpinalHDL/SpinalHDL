package spinal.tester.scalatest

import java.io.File
import java.nio.charset.Charset

import org.scalatest._
import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.sys.process._

abstract class SpinalTesterCocotbBase extends FunSuite /* with BeforeAndAfterAll with ParallelTestExecution*/ {

  var withWaveform = false
  var spinalMustPass = true
  var cocotbMustPass = true
  var genHdlSuccess = false
  var waveDepth = 99
  def noVhdl = false
  def noVerilog = false

  def genVhdl: Unit ={
    try {
      val waveFolder = sys.env.getOrElse("WAVES_DIR",".")
      backendConfig(SpinalConfig(mode = VHDL)).generate(createToplevel)
      genHdlSuccess = true
    } catch {
      case e: Throwable => {
        if(spinalMustPass)
          throw e
        return
      }
    }
    assert(spinalMustPass,"Spinal has not fail :(")
  }
  def waveFolder = sys.env.getOrElse("WAVES_DIR",".")

  def genVerilog: Unit ={
    try {

      backendConfig(SpinalConfig(mode = Verilog,dumpWave = if(withWaveform) DumpWaveConfig(depth = waveDepth,vcdPath = waveFolder + "/" + getName + "_verilog.vcd") else null)).generate(createToplevel)
      genHdlSuccess = true
    } catch {
      case e: Throwable => {
        if(spinalMustPass)
          throw e
        return
      }
    }
    assert(spinalMustPass,"Spinal has not fail :(")
  }


  def doTest(testPath : String, lang : Language): Unit ={
    assert(genHdlSuccess)
    val (langString, xmlPath) = lang match {
      case Language.VHDL => ("vhdl", testPath + "/sim_build/results.xml")
      case Language.VERILOG | Language.SYSTEM_VERILOG => ("verilog", testPath + "/results.xml")
    }
    doCmd(Seq(
      s"rm -f $xmlPath"
    ))
    val additionalArgs = ArrayBuffer[String]()
    if(withWaveform && lang == Language.VHDL){
      additionalArgs += s"SIM_ARGS=--vcd=${waveFolder + "/" + getName + "_vhdl.vcd"}"
    }
    if(withWaveform){
      additionalArgs += "RANDOM_SEED=1377424946"
    }
    val stdout = doCmd(Seq(
      s"cd $testPath",
      s"make TOPLEVEL_LANG=${langString} ${additionalArgs.mkString(" ")}"
    ))
//    val pass = getCocotbPass(xmlPath)
    val pass = stdout.contains("**                                 ERRORS : 0                                      **")

    assert(!cocotbMustPass || pass,"Simulation fail")
    assert(cocotbMustPass || !pass,"Simulation has not fail :(")
  }

  if(!noVhdl)
    test("genVhdl") {genVhdl}
  if(!noVerilog)
    test("genVerilog") {genVerilog}

  if(spinalMustPass) {
    val cocotbTests = ArrayBuffer[(String, String)]()
    if (pythonTestLocation != null) cocotbTests += ("cocotb" -> pythonTestLocation)
    cocotbTests ++= pythonTests
    for ((name, location) <- cocotbTests) {
      if(!noVhdl)
        test(name + "VHDL") {
          doTest(location, Language.VHDL)
        }
      if(!noVerilog)
        test(name + "Verilog") {
          doTest(location, Language.VERILOG)
        }
    }
  }

  if(postTest != null){
    test("postTests"){
      postTest()
    }
  }



  def doCmd(cmds : Seq[String]): String ={
    var out,err : String = null
    val io = new ProcessIO(
      stdin  => {
        for(cmd <- cmds)
          stdin.write((cmd + "\n").getBytes)
        stdin.close()
      },
      stdout => {
        out = scala.io.Source.fromInputStream(stdout).getLines.foldLeft("")(_ + "\n" + _)
        stdout.close()
      },
      stderr => {
        err = scala.io.Source.fromInputStream(stderr).getLines.foldLeft("")(_ + "\n" + _)
        stderr.close()
      })
    val proc = Process("sh").run(io)
    proc.exitValue()
    println(out)
    println(err)
    out
  }

  def getCocotbPass(location : String) : Boolean = {
    Thread.sleep(500)
    import scala.io.Source
    for(line <- Source.fromFile(location).getLines()) {
      if (line.contains("failure") || line.contains("skipped")){
        return false
      }
    }
    return true
  }

  def postTest : () => Unit = null

  def backendConfig(config: SpinalConfig) : SpinalConfig = config
  def getName: String = this.getClass.getName()
  def createToplevel: Component
  def pythonTestLocation : String = null
  def pythonTests : Seq[(String,String)] = Nil
}
