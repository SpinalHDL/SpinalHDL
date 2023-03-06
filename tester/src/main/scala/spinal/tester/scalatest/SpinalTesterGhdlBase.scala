package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._

import java.io.File
import scala.sys.process._

abstract class SpinalTesterGhdlBase extends AnyFunSuite  {
  def workspaceRoot = "./simWorkspace"
  def workspaceFile = new File(workspaceRoot)
  var withWaveform = false
  var elaborateMustFail = false
  var checkHDLMustFail = false
  var simulateMustFail = false

  test(getName) {
    testIt
    postTest
  }

  def testIt: Unit = {
    if(!elaborateMustFail) elaborate else elaborateWithFail
    if(!checkHDLMustFail) checkHDL else checkHDLWithFail
    if(!simulateMustFail) simulateHDL else simulateHDLWithFail
  }

  def postTest : Unit = {}

  def getLibraryName = "lib_" + getName
  def elaborate: Unit = {
    backendConfig(SpinalConfig(targetDirectory = workspaceRoot)).generateVhdl(createToplevel)
  }

  def backendConfig(config: SpinalConfig) : SpinalConfig = {
    config
  }

  def elaborateWithFail: Unit = {
    try {
      elaborate
      assert(false, "Spinal elaboration has not fail :(")
    } catch {
      case e: Throwable => return;
    }
  }

  def checkHDL(mustSucceed: Boolean): Unit = {
    val tester = new File(s"tester/src/test/resources/${getName}_tb.vhd").getAbsoluteFile
    val comp = s"ghdl -a --ieee=synopsys --work=$getLibraryName --workdir=. $getName.vhd $tester"
    println("GHDL compilation " + comp)
    assert((Process(comp, workspaceFile).! == 0) == mustSucceed, if (mustSucceed) "compilation fail" else "Compilation did not fail :(")

    val elab = s"ghdl -e --ieee=synopsys --work=$getLibraryName --workdir=. ${getName}_tb"
    println("GHDL elaborate " + elab)
    assert((Process(elab, workspaceFile).! == 0) == mustSucceed, if (mustSucceed) "compilation fail" else "Compilation did not fail :(")
  }

  def checkHDL: Unit = checkHDL(true)
  def checkHDLWithFail: Unit = checkHDL(false)

  def simulateHDL : Unit= simulateHDL(true)
  def simulateHDLWithFail : Unit= simulateHDL(false)

  def simulateHDL(mustSucceed : Boolean): Unit = {
    val run = s"ghdl -r --ieee=synopsys --work=$getLibraryName --workdir=. ${getName}_tb --ieee-asserts=disable ${if (!withWaveform) "" else s" --vcd=$getName.vcd"}"
    println("GHDL run " + run)
    val ret = Process(run, new File(workspaceRoot)).!
    assert(!mustSucceed || ret == 0,"Simulation fail")
    assert(mustSucceed || ret != 0,"Simulation did not fail :(")
  }

  def getName: String
  def createToplevel: Component
}
