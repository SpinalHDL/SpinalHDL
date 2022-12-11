package spinal.tester

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._

import java.io.File
import org.apache.commons.io.FileUtils

class RepeatabilitySuite extends AnyFunSuite {
  var checkOutputHashCounter = 0
  def checkOutputHash(gen: => Component): Unit = {
    checkOutputHashCounter = checkOutputHashCounter + 1
    var ref = ""
    for (i <- 0 until 8) {
      val report =
        SpinalConfig(defaultClockDomainFrequency = FixedFrequency(50 MHz))
          .generateVerilog(
            gen.setDefinitionName(s"checkOutputHash_${checkOutputHashCounter}")
          )
      FileUtils.copyFile(
        new File(report.generatedSourcesPaths.head),
        new File(report.generatedSourcesPaths.head + "_" + i + ".v")
      )

      import sys.process._
      val hash = s"md5sum ${report.generatedSourcesPaths.head}".!!
      if (i == 0) ref = hash
      else assert(ref == hash)
    }
  }
}
