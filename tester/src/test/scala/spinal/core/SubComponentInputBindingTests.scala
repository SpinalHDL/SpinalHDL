package spinal.core

import org.apache.commons.io.FileUtils
import spinal.tester.SpinalAnyFunSuite
import spinal.core._
import java.nio.file.Files

class SubComponentInputBindingTests extends SpinalAnyFunSuite {
  class Child extends Component {
    val io = new Bundle {
      val direct = in Bool()
      val directUnread = in Bool()
      val literalUnused = in Bool()
      val literalRead = in Bool()
      val expression = in Bool()
      val result = out Bool()
    }
    io.result := io.direct ^ io.directUnread ^ io.literalUnused ^ io.literalRead ^ io.expression
  }

  class Parent extends Component {
    val io = new Bundle {
      val source = in Bool()
      val directRead = out Bool()
      val literalRead = out Bool()
      val result = out Bool()
    }
    val child = new Child
    child.io.direct := io.source
    child.io.directUnread := io.source
    child.io.literalUnused := True
    child.io.literalRead := False
    child.io.expression := !io.source
    io.directRead := child.io.direct
    io.literalRead := child.io.literalRead
    io.result := child.io.result
  }

  test("direct and literal child input bindings survive parent reads") {
    for (mode <- Seq(Verilog, VHDL)) {
      val directory = Files.createTempDirectory("spinal-p2-binding-")
      try {
        val rtl = SpinalConfig(mode = mode, targetDirectory = directory.toString).generate(new Parent).getRtlString()
        if (mode == Verilog) {
          Seq(
            raw"\.io_direct\s+\(io_source\s*\)",
            raw"\.io_directUnread\s+\(io_source\s*\)",
            raw"\.io_literalUnused\s+\(1'b1\s*\)",
            raw"\.io_literalRead\s+\(child_1_io_literalRead\s*\)",
            raw"\.io_expression\s+\(child_1_io_expression\s*\)"
          ).foreach(pattern => assert(pattern.r.findFirstIn(rtl).nonEmpty, s"Missing $pattern in Verilog RTL"))
        } else {
          Seq(
            "io_direct => io_source",
            "io_directUnread => io_source",
            "io_literalUnused => pkg_toStdLogic(true)",
            "io_literalRead => child_1_io_literalRead",
            "io_expression => child_1_io_expression"
          ).foreach(binding => assert(rtl.contains(binding), s"Missing $binding in VHDL RTL"))
        }
      } finally {
        FileUtils.deleteDirectory(directory.toFile)
      }
    }
  }
}
