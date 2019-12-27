package spinal.lib.eda.symbyflow

import spinal.lib.eda.common._
import java.io.File
import spinal.core._
import scala.collection._
import java.nio.file.{Path, Paths}

object YosysFlow {
  def ice40Flow[T <: Component](report: SpinalReport[T],
                                workDir: String = "makeWorkplace") = {
      InputFile(report) |>(Yosys.load_SystemVerilog(report) + Yosys.synthesize("ice40")).outputFolder(Paths.get(workDir,"syntesys")) |>
      NextPNR_ice40().outputFolder(Paths.get(workDir,"pnr"))
  }

  def formalFlow[T <: Component](report: SpinalReport[T],
                                 mode: String = Mode.bmc,
                                 multiclock: Boolean = false,
                                 memoryMap: Boolean = false,
                                 workDir: String = "makeWorkplace") = {
    InputFile(report) |>
    Yosys.formal(report,
                mode,
                multiclock,
                memoryMap).outputFolder(Paths.get(workDir,"modelgen")) |>
    FormalCommand().outputFolder(Paths.get(workDir,"formal"))
  }
}
