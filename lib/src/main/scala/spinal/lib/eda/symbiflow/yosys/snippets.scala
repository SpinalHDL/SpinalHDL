package spinal.lib.eda.symbiflow

import java.nio.file.{Path, Paths}
import spinal.core._

import org.apache.commons.io.FilenameUtils

object YosysSnippet {
  def loadSystemVerilog[T <: Component](report: SpinalReport[T], opt: String*): Yosys = {
    val command = Yosys()
    report.rtlSourcesPaths.foreach(x => command.addInputFile(Paths.get(x), "verilog", ("-sv" +: opt): _*))
    command + setTop(report.toplevelName)
  }

  def loadVerilog[T <: Component](report: SpinalReport[T], opt: String*): Yosys = {
    val command = Yosys()
    report.rtlSourcesPaths.foreach(x => command.addInputFile(Paths.get(x), "verilog", opt: _*))
    command + setTop(report.toplevelName)
  }

  def setTop(top: String): Yosys = {
    val command = Yosys()
    command.addCommand("prep", "-top", top)
    command
  }

  def load_verilog(opt: String*): Yosys = {
    val command = Yosys()
    val verilogFiles = command.getAllPrerequisiteFromExtension(".*.v")
    verilogFiles.foreach(command.addInputFile(_, "verilog", opt:_*))
    command
  }

  def export(file: String, opt: String*): Yosys = {
    val ret = Yosys()
    ret.addOutputFile(Paths.get(file), FilenameUtils.getExtension(file), opt:_*)
    ret
  }

  def model(mode: String,
            multiclock: Boolean = false,
            memoryMap: Boolean = false): Yosys = {
    val modelGen = Yosys()
    if (memoryMap) modelGen.addCommand("memory_map")
    else modelGen.addCommand("memory_nordff")
    modelGen.addCommand("memory_nordff")
    if (multiclock) {
      modelGen.addCommand("clk2fflogic")
    } else {
      modelGen.addCommand("async2sync")
      modelGen.addCommand("chformal", "-assume", "-early")
    }
    if (mode == Mode.bmc || mode == Mode.prove)
      modelGen.addCommand("chformal", "-live", "-fair", "-cover", "-remove")
    if (mode == Mode.cover)
      modelGen.addCommand("chformal", "-live", "-fair", "-remove")
    if (mode == Mode.live) {
      modelGen.addCommand("chformal", "-assert2assume")
      modelGen.addCommand("chformal", "-cover", "-remove")
    }
    modelGen.addCommand("opt_clean")
    modelGen.addCommand("setundef", "-anyseq")
    modelGen.addCommand("opt", "-keepdc", "-fast")
    modelGen.addCommand("check")
    modelGen.addCommand("hierarchy", "-simcheck")
    modelGen
  }

  def synthesize(target: String): Yosys = {
    val command = Yosys()
    command.addCommand("synth_" + target)
    command
  }

  def svFormal[T <: Component](report: SpinalReport[T],
                             mode: String = Mode.bmc,
                             multiclock: Boolean = false,
                             memoryMap: Boolean = false,
                             workDir: String = ".") = {
    loadSystemVerilog(report,"-formal") +
    model(mode, multiclock, memoryMap) +
    export(report.toplevelName + ".smt2")
  }
}