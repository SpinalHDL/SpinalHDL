package spinal.lib.eda.symbiflow

import spinal.lib.eda.common._
import java.io.File
import spinal.core._
import scala.collection._
import java.nio.file.{Path, Paths}

object SymbiFlow {
  def syntesize[T <: Component]( report: SpinalReport[T],
                                pcf: Path,
                                target: String = "ice40") = {
    val syntesis = InputFile(report) |>
      (
        YosysSnippet.loadSystemVerilog(report) +
        YosysSnippet.synthesize(target) +
        YosysSnippet.export("synthesis.json")
      ).outputFolder(Paths.get("synthesis"))
    val constrain = InputFile(pcf)

    List(syntesis, constrain) |> NextPNR_ice40().outputFolder(Paths.get("pnr"))
  }

  def svFormal[T <: Component]( report: SpinalReport[T],
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
