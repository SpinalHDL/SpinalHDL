package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.eda.TimingExtractor
import spinal.lib.eda.xilinx.TimingExtractorXdc

import java.io.{ByteArrayOutputStream, PrintStream}
import scala.language.postfixOps
import scala.io.Source

class TimingExtractorXdcTester extends SpinalAnyFunSuite {
  case class Test() extends Component {
    val io = new Bundle {
      val i = in port Bool()
      val o = out port Bool()
      val o2 = out port Bool()

      val is = slave port Stream(Bits(2 bit))
      val os = master port Stream(Bits(2 bit))

      val ib = in port Bits(3 bit)
      val ob = out port Bits(3 bit)

      val iv = in port Vec(Bits(2 bit), 2)
      val ov = out port Vec(Bits(2 bit), 2)
    }
    val otherCD = ClockDomain.external("someDomain")

    // BufferCC with false path that specified source
    io.o := BufferCC(io.i, inputAttributes = List(crossClockFalsePath(source = Some(io.i))))

    // BufferCC with false path that didn't specify source
    io.o2 := BufferCC(io.i, inputAttributes = List(crossClockFalsePath()))

    // generates 3 max delays:
    // - input       -> buffer push
    // - buffer push -> buffer pop
    // - buffer pop  -> output
    // generates 2 false paths on reset in source and destination clock domain
    io.os <> io.is.ccToggle(ClockDomain.current, otherCD)

    // if an input directly drives the start of a cross-clock path, a clock-domain tag must be added
    val cdt = ClockDomainTag(clockDomain)

    otherCD {
      io.ob := BufferCC(
        io.ib addTag cdt,
        inputAttributes = List(crossClockMaxDelay(2, useTargetClock = true))
      )

      io.ov := BufferCC(
        io.iv,// addTag cdt,
        inputAttributes = List(crossClockMaxDelay(2, useTargetClock = true))
      )
    }
  }

  test("Test constraint generation for Vivado") {
    val report = SpinalVerilog(Test())
    val extractor = new TimingExtractorXdc

    // capture stdout to check warning messages
    val stdoutBuffer = new ByteArrayOutputStream()
    val stdoutStream = new PrintStream(stdoutBuffer)

    Console.withOut(stdoutStream) {
      TimingExtractor(report, extractor)
    }
    extractor.close()
    stdoutStream.close()

    def checkFile(path: String, expectedContent: String): Unit = {
      val src = Source.fromFile(path)
      val got = src.getLines.mkString(System.lineSeparator())
      assert(got == expectedContent)
      src.close()
    }

    assert(stdoutBuffer.toString ==
      """
        |!!! No valid source found for (toplevel/io_i_buffercc_1/buffers_0 :  Bool)
        |    while writing a false path constraint, omitting the -from selector.  To include
        |    the selector, you need to specify source= in the crossClockFalsePath tag.
        |
        |!!! No valid source found for (toplevel/io_iv_buffercc/buffers_0_0 :  Bits[2 bits])
        |    while writing a max delay constraint, skipping.  If the target does not have
        |    a non-combinatorial driver in the netlist (register or memory), you need to
        |    mark the driver with a ClockDomainTag.
        |
        |!!! No valid source found for (toplevel/io_iv_buffercc/buffers_0_1 :  Bits[2 bits])
        |    while writing a max delay constraint, skipping.  If the target does not have
        |    a non-combinatorial driver in the netlist (register or memory), you need to
        |    mark the driver with a ClockDomainTag.
        |""".stripMargin)
    checkFile(extractor.realFilename,
      """
        |# False path: (toplevel/io_i : in Bool) -> (toplevel/io_i_buffercc/buffers_0 :  Bool)
        |set pin [get_pins -hier -filter {NAME =~ "*/io_i"}]
        |set net [get_nets -segments -of_objects $pin]
        |set source_pins [get_pins -of_objects $net -filter {IS_LEAF && DIRECTION == OUT}]
        |set source [get_cells -of_objects $source_pins]
        |set_false_path -from $source -to [get_pins -hier -regexp -filter {NAME =~ ".*/io_i_buffercc/buffers_0_reg.*/D"}]
        |
        |# False path: [no source specified] -> (toplevel/io_i_buffercc_1/buffers_0 :  Bool)
        |set_false_path -to [get_pins -hier -regexp -filter {NAME =~ ".*/io_i_buffercc_1/buffers_0_reg.*/D"}]
        |
        |# Max delay path: (toplevel/io_is_ccToggle/pushArea_data :  Bits[2 bits]) -> (toplevel/io_is_ccToggle/popArea_stream_rData :  Bits[2 bits])
        |set clk_pin [get_pins -hier -filter {NAME =~ "*/io_is_ccToggle/pushArea_data_reg*/C"}]
        |set clk [get_clocks -include_generated_clocks -of $clk_pin]
        |set src_clk_period [get_property -min PERIOD $clk]
        |set clk_pin [get_pins -hier -filter {NAME =~ "*/io_is_ccToggle/popArea_stream_rData_reg*/C"}]
        |set clk [get_clocks -include_generated_clocks -of $clk_pin]
        |set dst_clk_period [get_property -min PERIOD $clk]
        |set src  [get_cells -hier -filter {NAME =~ "*/io_is_ccToggle/pushArea_data_reg*"}]
        |set dest [get_pins  -hier -filter {NAME =~ "*/io_is_ccToggle/popArea_stream_rData_reg*/D"}]
        |set_max_delay -from $src -to $dest [expr {1 * $dst_clk_period}] -datapath_only
        |set_bus_skew  -from $src -to $dest $dst_clk_period
        |
        |# Max delay path: (toplevel/io_is_ccToggle/popArea_hit :  Bool) -> (toplevel/io_is_ccToggle/outHitSignal_buffercc/buffers_0 :  Bool)
        |set clk_pin [get_pins -hier -filter {NAME =~ "*/io_is_ccToggle/popArea_hit_reg*/C"}]
        |set clk [get_clocks -include_generated_clocks -of $clk_pin]
        |set src_clk_period [get_property -min PERIOD $clk]
        |set clk_pin [get_pins -hier -filter {NAME =~ "*/io_is_ccToggle/outHitSignal_buffercc/buffers_0_reg*/C"}]
        |set clk [get_clocks -include_generated_clocks -of $clk_pin]
        |set dst_clk_period [get_property -min PERIOD $clk]
        |set src  [get_cells -hier -filter {NAME =~ "*/io_is_ccToggle/popArea_hit_reg*"}]
        |set dest [get_pins  -hier -filter {NAME =~ "*/io_is_ccToggle/outHitSignal_buffercc/buffers_0_reg*/D"}]
        |set_max_delay -from $src -to $dest [expr {1 * $dst_clk_period}] -datapath_only
        |set_bus_skew  -from $src -to $dest $dst_clk_period
        |
        |# False path: (reset :  Bool) -> (toplevel/io_is_ccToggle/reset_asyncAssertSyncDeassert_buffercc/buffers_0 :  Bool)
        |set pin [get_pins -hier -filter {NAME =~ "*/reset"}]
        |set net [get_nets -segments -of_objects $pin]
        |set source_pins [get_pins -of_objects $net -filter {IS_LEAF && DIRECTION == OUT}]
        |set source [get_cells -of_objects $source_pins]
        |set_false_path -quiet -from $source -to [get_pins -hier -regexp -filter {NAME =~ ".*/io_is_ccToggle/reset_asyncAssertSyncDeassert_buffercc/buffers_0_reg.*/(PRE|CLR|S|R)"}]
        |
        |# False path: (reset :  Bool) -> (toplevel/io_is_ccToggle/reset_asyncAssertSyncDeassert_buffercc/buffers_1 :  Bool)
        |set pin [get_pins -hier -filter {NAME =~ "*/reset"}]
        |set net [get_nets -segments -of_objects $pin]
        |set source_pins [get_pins -of_objects $net -filter {IS_LEAF && DIRECTION == OUT}]
        |set source [get_cells -of_objects $source_pins]
        |set_false_path -quiet -from $source -to [get_pins -hier -regexp -filter {NAME =~ ".*/io_is_ccToggle/reset_asyncAssertSyncDeassert_buffercc/buffers_1_reg.*/(PRE|CLR|S|R)"}]
        |
        |# Max delay path: (toplevel/io_is_ccToggle/pushArea_target :  Bool) -> (toplevel/io_is_ccToggle/pushArea_target_buffercc/buffers_0 :  Bool)
        |set clk_pin [get_pins -hier -filter {NAME =~ "*/io_is_ccToggle/pushArea_target_reg*/C"}]
        |set clk [get_clocks -include_generated_clocks -of $clk_pin]
        |set src_clk_period [get_property -min PERIOD $clk]
        |set clk_pin [get_pins -hier -filter {NAME =~ "*/io_is_ccToggle/pushArea_target_buffercc/buffers_0_reg*/C"}]
        |set clk [get_clocks -include_generated_clocks -of $clk_pin]
        |set dst_clk_period [get_property -min PERIOD $clk]
        |set src  [get_cells -hier -filter {NAME =~ "*/io_is_ccToggle/pushArea_target_reg*"}]
        |set dest [get_pins  -hier -filter {NAME =~ "*/io_is_ccToggle/pushArea_target_buffercc/buffers_0_reg*/D"}]
        |set_max_delay -from $src -to $dest [expr {1 * $dst_clk_period}] -datapath_only
        |set_bus_skew  -from $src -to $dest $dst_clk_period
        |
        |# Max delay path: (toplevel/io_ib : in Bits[3 bits]) -> (toplevel/io_ib_buffercc/buffers_0 :  Bits[3 bits])
        |set clk_pin [get_pins -hier -filter {NAME =~ "*/io_ib_reg*/C"}]
        |set clk [get_clocks -include_generated_clocks -of $clk_pin]
        |set src_clk_period [get_property -min PERIOD $clk]
        |set clk_pin [get_pins -hier -filter {NAME =~ "*/io_ib_buffercc/buffers_0_reg*/C"}]
        |set clk [get_clocks -include_generated_clocks -of $clk_pin]
        |set dst_clk_period [get_property -min PERIOD $clk]
        |set src  [get_cells -hier -filter {NAME =~ "*/io_ib_reg*"}]
        |set dest [get_pins  -hier -filter {NAME =~ "*/io_ib_buffercc/buffers_0_reg*/D"}]
        |set_max_delay -from $src -to $dest [expr {2 * $dst_clk_period}] -datapath_only
        |set_bus_skew  -from $src -to $dest $dst_clk_period""".stripMargin)
    checkFile(extractor.oocFilename,
      """
        |# Clock definition: clk
        |create_clock -period 1 -name clk [get_ports clk]
        |
        |# Clock definition: someDomain_clk
        |create_clock -period 1 -name someDomain_clk [get_ports someDomain_clk]""".stripMargin)
  }
}
