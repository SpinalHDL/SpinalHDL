package spinal.lib.eda.symbiflow

import spinal.lib.eda.common._
import java.io.File
import spinal.core._
import scala.collection._
import java.nio.file.{Path, Paths}

object SymbiFlow {
  def icestorm[T <: Component]( report: SpinalReport[T],
                                target: String = "ice40",
                                workDir: String = "makeWorkplace") = {
    InputFile(report) |>
    (
      YosysSnippet.loadSystemVerilog(report) +
      YosysSnippet.synthesize(target) +
      YosysSnippet.export("synthesis.json")
    ).outputFolder(Paths.get(workDir,"synthesis")) |>
    NextPNR_ice40().outputFolder(Paths.get(workDir,"pnr"))
  }

  def svFormal[T <: Component](report: SpinalReport[T],
                                 mode: String = Mode.bmc,
                                 multiclock: Boolean = false,
                                 memoryMap: Boolean = false,
                                 workDir: String = "makeWorkplace") = {
    InputFile(report) |>
    YosysSnippet.svFormal(
      report,
      mode,
      multiclock,
      memoryMap).outputFolder(Paths.get(workDir,"modelgen")) |>
    FormalCommand().outputFolder(Paths.get(workDir,"formal"))
  }
}
