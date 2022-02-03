package spinal.sim

import spinal.sim.vpi.SharedMemIface

import java.io.{File, PrintWriter}
import scala.sys.process.Process


class VcsBackendConfig extends VpiBackendConfig {
  var vcsPath: String = null
  var elaborationFlags: String = ""
}

class VcsBackend(config: VcsBackendConfig) extends VpiBackend(config) {
  var vcsPath = config.vcsPath
  val elaborationFlags = config.elaborationFlags

  val availableFormats = Array(WaveFormat.VCD, WaveFormat.FSDB,
    WaveFormat.DEFAULT, WaveFormat.NONE)

  val format = if(availableFormats contains waveFormat){
    waveFormat
  } else {
    println("Wave format " + waveFormat + " not supported by GHDL")
    WaveFormat.NONE
  }

  if(!(Array(WaveFormat.DEFAULT, WaveFormat.NONE) contains format)) {
    if(format == WaveFormat.FSDB){
      runFlags += "  -debug_acc+pp+fsdb " + wavePath
    } else {
      runFlags += " --" + format.ext + "=" + wavePath
    }
  }

  def compileVPI()  = {}

  def analyzeRTL()  = {
    val simWaveSource =
      s"""
         |module __simulation_def;
         |initial begin
         |    $$fsdbDumpfile("$wavePath") ;
         |    $$fsdbDumpvars(0, $toplevelName);
         |    $$fsdbDumpflush;
         |end
         |endmodule""".stripMargin
    val simulationDefSourceFile = new PrintWriter(new File(s"${workspacePath}/rtl/" ++
      "__simulation_def.v"))
    simulationDefSourceFile.write(simWaveSource)
    simulationDefSourceFile.close

    val vhdl_files = rtlSourcesPaths.filter{ s =>
      s.endsWith(".vhd") ||
      s.endsWith(".vhdl")
    }.mkString("\\\n")

    val verilog_files = rtlSourcesPaths.filter{s =>
      s.endsWith(".v") ||
      s.endsWith(".sv") ||
      s.endsWith(".vh") ||
      s.endsWith(".svh")
    }.mkString("\\\n")

    val incdirs = rtlSourcesPaths.filter{s =>
      s.startsWith("+incdir+")
    }.mkString("\\\n")

    val fsdbswitch = if(format == WaveFormat.FSDB) "-debug_acc+pp+fsdb" else ""
    val vloganFlag = List(
      "-full64 +v2k -sverilog -v2005",
      "-timescale=1ns/100ps",
      fsdbswitch,
      "+libext+.v+.vp+.vh+.dat+.sv+.svh",
      "-Xcheck_p1800_2009=char",
      "-Mlib=csrc",
      "+notimingcheck",
      "+nospecify",
      "-work xil_defaultlib",
      s"${incdirs}",
      s"./rtl/__simulation_def.v",
      s"${verilog_files}"
    )

    val vhdlanFlag = List(
      "-full64",
      fsdbswitch,
      "-Mlib=csrc",
      "-work xil_defaultlib",
      s"${incdirs}",
      s"${vhdl_files}"
    )

    val vcsElaborateFlag = List(
      "-full64",
      "-debug_access+all",
      "-t ps -licqueue",
      "-l elaborate.log  ",
      "xil_defaultlib.glbl",
      "+error+100 +cli+3 ",
      "-o simv"
    )

    val vhdlan_compile = if(vhdl_files.isEmpty) "" else s"vhdlan ${vhdlanFlag.mkString(" ")}"
    val vlogan_compile = s"vlogan ${vloganFlag.mkString(" ")}"
    val vcs_elaborator = s"vcs    ${vcsElaborateFlag.mkString(" ")}"

    val vcsFullScript4Debug =
      s"""
        |${vhdlan_compile}
        |${vlogan_compile}
        |${vcs_elaborator}
        |./simv
        |""".stripMargin

    val verilatorScriptFile = new PrintWriter(new File(workspacePath + "/vcsFullScript.sh"))
    verilatorScriptFile.write(vcsFullScript4Debug)
    verilatorScriptFile.close

    val workDir = new File(workspacePath)
    if(!vhdlan_compile.isEmpty){
      assert(Process(vhdlan_compile, workDir).!(new Logger()) == 0, s"Compile VHDL files failed")
    }
    assert(Process(vlogan_compile, workDir).!(new Logger()) == 0, s"Compile Verilog/SV files failed")
    assert(Process(vcs_elaborator, workDir).!(new Logger()) == 0, s"Vcs Elaboration failed")
  }

  def runSimulation(sharedMemIface: SharedMemIface) : Thread = {
    val thread = new Thread(new Runnable{
      val iface = sharedMemIface
      def run(): Unit = {
        val retCode = Process("./simv").! (new Logger())
        if (retCode != 0) {
          iface.set_crashed(retCode)
          println(s"Simulation of $toplevelName failed")
        }
      }
    })
    thread.setDaemon(true)
    thread.start()
    thread
  }

  override def isBufferedWrite: Boolean = true
}