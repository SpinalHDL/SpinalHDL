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

  def findClockPeriod(clock: Bool, destVar: String = "clk_period"): String = {
    s"""set clk_net [get_nets -hier -filter {NAME =~ "*/${clock.getName()}"}]
       |set clk [get_clocks -include_generated_clocks -of $$clk_net]
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
    val sourceCD = TimingExtractor.clockDomainOf(source)
    val targetCD = TimingExtractor.clockDomainOf(target)
    val maxDelay = f"expr {${tag.cycles} * $$${if (tag.useTargetClock) "dst_clk_period" else "src_clk_period"}}"
    writer.write(
      s"""
         |# Max delay path: $source -> $target
         |${findClockPeriod(sourceCD.clock, "src_clk_period")}
         |${findClockPeriod(targetCD.clock, "dst_clk_period")}
         |set source [get_cells -hier -filter {NAME =~ "*/${pathOf(source)}_reg*"}]
         |set_max_delay -from $$source -to [get_pins -hier -filter {NAME =~ "*/${pathOf(target)}_reg*/D"}] [$maxDelay] -datapath_only
         |set_bus_skew -from $$source -to [get_pins -hier -filter {NAME =~ "*/${pathOf(target)}_reg*/D"}] $$dst_clk_period
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

case class Test() extends Component {

  import spinal.lib._

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

object Test extends App {
  TimingExtractor(SpinalVerilog(Test()), new TimingExtractorXdc)
}
