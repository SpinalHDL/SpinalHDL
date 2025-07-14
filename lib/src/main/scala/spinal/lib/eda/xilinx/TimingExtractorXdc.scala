package spinal.lib.eda.xilinx

import spinal.core._
import spinal.core.ClockDomain.FixedFrequency
import spinal.lib.eda.{TimingExtractor, TimingExtractorListener}

import java.io.{File, PrintWriter}
import scala.language.postfixOps

/** Writes timing constraints for Vivado.  Produces two .xdc constraint files:
  *  - a normal XDC file to use when the toplevel is included inside another project.  Includes
  *    all false path and max delay constraints
  *  - an "out-of-context" (OOC) XDC file to use when the toplevel is packaged as an IP (e.g. to use in block design).
  *    Includes all clock and input/output delay commands
  */
class TimingExtractorXdc extends TimingExtractorListener {
  val component = GlobalData.get.phaseContext.topLevel.definitionName
  val outDir = GlobalData.get.phaseContext.config.targetDirectory
  val realFilename = s"$outDir/$component.xdc"
  val writer = new PrintWriter(new File(realFilename))

  val oocFilename = s"$outDir/${component}_ooc.xdc"
  val oocWriter = new PrintWriter(new File(oocFilename))

  def findDriverCell(s: String, destVar: String = "source"): String =
    s"""set pin [get_pins -hier -filter {NAME =~ "*/$s"}]
       |set net [get_nets -segments -of_objects $$pin]
       |set source_pins [get_pins -of_objects $$net -filter {IS_LEAF && DIRECTION == OUT}]
       |set $destVar [get_cells -of_objects $$source_pins]
       |""".stripMargin

  def findClockPeriod(signal: Any, destVar: String = "clk_period"): String = {
    s"""set clk_pin [get_pins -hier -filter {NAME =~ "*/${pathOf(signal)}_reg*/C"}]
       |set clk [get_clocks -include_generated_clocks -of $$clk_pin]
       |set $destVar [get_property -min PERIOD $$clk]""".stripMargin
  }

  def pathOf(that: Any): String = that match {
    case bt: BaseType => bt.getRtlPath()
  }

  // see https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_false_path
  def writeFalsePath(target: Any, falsePathTag: crossClockFalsePath): Unit = {
    val resetIsDriver = falsePathTag.destType == TimingEndpointType.RESET

    if (falsePathTag.source.isEmpty) {
      println(
        s"""
           |!!! No valid source found for $target
           |    while writing a false path constraint, omitting the -from selector.  To include
           |    the selector, you need to specify source= in the crossClockFalsePath tag.""".stripMargin)
    }
    val sourceLocator = falsePathTag.source.map { s =>
      findDriverCell(s.getName())
    }.getOrElse("")
    val sourceOption = falsePathTag.source.map { _ => " -from $source" } getOrElse ""
    val quiet = if (resetIsDriver) " -quiet" else ""
    val pinName = if (resetIsDriver) "(PRE|CLR|S|R)" else "D"

    writer.write(
      s"""
         |# False path: ${falsePathTag.source.getOrElse("[no source specified]")} -> $target
$sourceLocator|set_false_path$quiet$sourceOption -to [get_pins -hier -regexp -filter {NAME =~ ".*/${pathOf(target)}_reg.*/$pinName"}]
         |""".stripMargin)
  }

  // see https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_max_delay
  // and https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_bus_skew
  def writeMaxDelay(target: Any, source: Any, tag: crossClockMaxDelay): Unit = {
    // XXX: we cannot rely on the clock domain names since the toplevel does not have keep_hierarchy;
    //      must locate the clocks from the clock pins (*_reg/C) of the nets
    val maxDelay = f"expr {${tag.cycles} * $$${if (tag.useTargetClock) "dst_clk_period" else "src_clk_period"}}"
    writer.write(
      s"""
         |# Max delay path: $source -> $target
         |${findClockPeriod(source, "src_clk_period")}
         |${findClockPeriod(target, "dst_clk_period")}
         |set src  [get_cells -hier -filter {NAME =~ "*/${pathOf(source)}_reg*"}]
         |set dest [get_pins  -hier -filter {NAME =~ "*/${pathOf(target)}_reg*/D"}]
         |set_max_delay -from $$src -to $$dest [$maxDelay] -datapath_only
         |set_bus_skew  -from $$src -to $$dest $$dst_clk_period
         |""".stripMargin)
  }

  def writeClock(clock: Bool, frequency: ClockDomain.ClockFrequency): Unit = {
    val name = clock.getName()
    val freqNanos = frequency match {
      case FixedFrequency(freq) => freq.toTime / (1 ns)
      case _ => BigDecimal(1)
    }
    oocWriter.write(
      s"""
         |# Clock definition: $name
         |create_clock -period $freqNanos -name $name [get_ports $name]
         |""".stripMargin)
  }

  def writeInputDelay(bt: BaseType): Unit = {
    // println(s"TimingExtractorXdc.writeInputDelay not implemented yet")
  }

  def writeOutputDelay(bt: BaseType): Unit = {
    // println(s"TimingExtractorXdc.writeOutputDelay not implemented yet")
  }

  def close(): Unit = {
    writer.close()
    oocWriter.close()
  }
}
