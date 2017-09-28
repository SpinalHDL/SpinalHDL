package spinal.tester.scalatest

import java.io.File
import java.nio.charset.Charset

import org.scalatest._
import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.sys.process._

abstract class SpinalTesterCocotbBase /*extends FunSuite *//* with BeforeAndAfterAll with ParallelTestExecution*/ {

  var withWaveform = false
  var spinalMustPass = true
  var cocotbMustPass = true
  var genHdlSuccess = false
  def genHdl: Unit ={
    try {
      val waveFolder = sys.env.getOrElse("WAVES_DIR",".")
      backendConfig(SpinalConfig(mode = Verilog,dumpWave = DumpWaveConfig(depth = 1,vcdPath = waveFolder + "/" + getName + ".vcd"))).generate(createToplevel)
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

  def doTest(testPath : String): Unit ={
    assert(genHdlSuccess)
    doCmd(Seq(
      s"rm -f $testPath/results.xml"
    ))
    doCmd(Seq(
      s"cd $testPath",
      "make"
    ))
    val pass = getCocotbPass(testPath)
    assert(!cocotbMustPass || pass,"Simulation fail")
    assert(cocotbMustPass || !pass,"Simulation has not fail :(")
  }

  //test("genVerilog") {genHdl}
  //  genHdl
  if(spinalMustPass) {
    val cocotbTests = ArrayBuffer[(String, String)]()
    if (pythonTestLocation != null) cocotbTests += ("cocotb" -> pythonTestLocation)
    cocotbTests ++= pythonTests
    for ((name, location) <- cocotbTests) {
//      test(name + "Verilog") {
//        doTest(location)
//      }
    }
  }

  if(postTest != null){
//    test("postTests"){
//      postTest()
//    }
  }



  def doCmd(cmds : Seq[String]): Unit ={
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
  }

  def getCocotbPass(location : String) : Boolean = {
    Thread.sleep(500)
    import scala.io.Source
    val resultPath = location + "/results.xml"
    for(line <- Source.fromFile(resultPath).getLines()) {
      if (line.contains("failure") || line.contains("skipped")){
        return false
      }
    }
    return true
  }

  def postTest: () => Unit = null

  def backendConfig(config: SpinalConfig) : SpinalConfig = config
  def getName: String = this.getClass.getName()
  def createToplevel: Component
  def pythonTestLocation : String = null
  def pythonTests : Seq[(String,String)] = Nil
}
