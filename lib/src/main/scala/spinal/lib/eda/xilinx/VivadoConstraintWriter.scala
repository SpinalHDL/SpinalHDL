package spinal.lib.eda.xilinx

import spinal.core.ClockDomain.FixedFrequency
import spinal.core.internals._
import spinal.core.{crossClockMaxDelay, _}
import spinal.lib.AnalysisUtils

import java.io.{File, PrintWriter, Writer}
import scala.collection.mutable
import scala.language.postfixOps

object VivadoConstraintWriter {
  def apply[T <: Component](
                             report: SpinalReport[T],
                             filename: String = null
                           ): Unit = {
    val c = report.toplevel
    val realFilename = Option(filename).getOrElse(
      f"${GlobalData.get.phaseContext.config.targetDirectory}/${c.getClass.getSimpleName}.xdc"
    )
    val writer = new PrintWriter(new File(realFilename))
    c.walkComponents(cc => cc.dslBody.walkStatements(doWalkStatements(_, writer)))
    writer.close()

    val oocFilename = realFilename.split('.').tails.collect {
      case Array(secondLast, _) => secondLast + "_ooc"
      case Array(other, _*) => other
    }.mkString(".")
    val oocWriter = new PrintWriter(new File(oocFilename))

    val clockDomainNames = mutable.LinkedHashSet[String]()
    AnalysisUtils.foreachToplevelIoCd(c) {
      case (p: Bool, List(cd)) if p.isInput =>
        if (!clockDomainNames.contains(cd.toString)) {
          writeClockDef(cd, oocWriter)
          clockDomainNames.add(cd.toString)
        }
      case _ =>
    }
    oocWriter.close()
  }

  def doWalkStatements(s: Statement, writer: Writer): Unit = {
    s match {
      case da: DataAssignmentStatement =>
        da.target match {
          case str: SpinalTagReady if str.hasTag(classOf[crossClockFalsePath]) =>
            writeFalsePath(da, writer, str.getTag(classOf[crossClockFalsePath]).get)
          case str: SpinalTagReady if str.hasTag(classOf[crossClockMaxDelay]) =>
            writeMaxDelay(da, str.getTag(classOf[crossClockMaxDelay]).get, writer)
          case _ =>
        }
      case _ =>
    }
  }

  def findDriverCell(s: String, destVar: String = "source"): String =
    s"""set pin [get_pins -hier -filter {NAME =~ */$s}]
       |set net [get_nets -segments -of_objects $$pin]
       |set source_pins [get_pins -of_objects $$net -filter {IS_LEAF && DIRECTION == OUT}]
       |set $destVar [get_cells -of_objects $$source_pins]""".stripMargin

  def findClockPeriod(cd: ClockDomain, compName: String, destVar: String = "clk_period"): String =
    s"""set clk_net [get_nets -hier -filter {NAME =~ */$compName/${cd.toString}}]
       |set clk [get_clocks -include_generated_clocks -of $$clk_net]
       |set $destVar [get_property -min PERIOD $$clk]""".stripMargin

  // see https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_false_path
  def writeFalsePath(s: DataAssignmentStatement, writer: Writer, falsePathTag: crossClockFalsePath): Unit = {
    val resetIsDriver = falsePathTag.destType == TimingEndpointType.RESET
    var source = s.source.asInstanceOf[BaseType]
    if (!resetIsDriver) {
      AnalysisUtils.seekNonCombDrivers(source) { case b: BaseType =>
        source = b
      }
    }
    val sourceLocator = if (source.isReg && !resetIsDriver) {
      // source is register inside spinal design
      f"set source [get_cells -hier -filter {NAME =~ */${source.getRtlPath()}_reg*}]"
    } else {
      findDriverCell(falsePathTag.source.get.getName())
    }
    val quiet = if (resetIsDriver) " -quiet" else ""
    val target = s.target.asInstanceOf[BaseType].getRtlPath()
    val pinName = if (resetIsDriver) "(PRE|CLR|S|R)" else "D"
    writer.write(
      s"""
         |# CDC constaints for ${source.getRtlPath()} -> ${target} in ${s.component.getPath()}
         |$sourceLocator
         |set_false_path$quiet -from $$source -to [get_pins -hier -regexp -filter {NAME =~ ".*/${target}_reg.*/$pinName"}]
         |""".stripMargin)

  }

  // see https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_max_delay
  // and https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_bus_skew
  def writeMaxDelay(s: DataAssignmentStatement, tag: crossClockMaxDelay, writer: Writer): Unit = {
    var source = s.source.asInstanceOf[BaseType]
    AnalysisUtils.seekNonCombDrivers(source) { case b: BaseType =>
      source = b
    }
    val sourceCD = source.getTag(classOf[ClockDomainTag]).map(_.clockDomain).getOrElse(source.clockDomain)
    val target = s.target.asInstanceOf[BaseType]
    val targetCD = target.getTag(classOf[ClockDomainTag]).map(_.clockDomain).getOrElse(target.clockDomain)
    val maxDelay = f"expr {${tag.cycles} * $$${if (tag.useTargetClock) "dst_clk_period" else "src_clk_period"}}"
    writer.write(
      s"""
         |# CDC constraints for ${source.getRtlPath()} -> ${target.getRtlPath()} in ${s.component.getPath()}
         |${findClockPeriod(sourceCD, s.component.getName(), "src_clk_period")}
         |${findClockPeriod(targetCD, s.component.getName(), "dst_clk_period")}
         |set source [get_cells -hier -filter {NAME =~ */${source.getRtlPath()}_reg*}]
         |set_max_delay -from $$source -to [get_pins -hier -filter {NAME =~ */${target.getRtlPath()}_reg*/D}] [$maxDelay] -datapath_only
         |set_bus_skew -from $$source -to [get_pins -hier -filter {NAME =~ */${target.getRtlPath()}_reg*/D}] $$dst_clk_period
         |""".stripMargin)
  }

  def writeClockDef(cd: ClockDomain, writer: Writer): Unit = {
    val name = cd.toString
    val freqNanos = cd.frequency match {
      case FixedFrequency(freq) => freq.toTime / (1 ns)
      case _ => BigDecimal(1)
    }
    writer.write(
      s"""
         |# Clock definition for $name
         |create_clock -period $freqNanos -name $name [get_ports $name]
         |""".stripMargin)
  }

  def fullPath(bt: BaseType) = (if (bt.component != null) bt.component.getPath() + "/" else "") + bt.getDisplayName()
}

case class Test() extends Component {

  import spinal.lib._

  val io = new Bundle {
    val i = in port Bool()
    val o = out port Bool()

    val is = slave port Stream(Bits(2 bit))
    val os = master port Stream(Bits(2 bit))

    val ib = in port Bits(3 bit)
    val ob = out port Bits(3 bit)

    val iv = in port Vec(Bits(2 bit), 2)
    val ov = out port Vec(Bits(2 bit), 2)
  }
  val otherCD = ClockDomain.external("someDomain")

  io.o := BufferCC(io.i, inputAttributes = List(crossClockFalsePath()))
  io.os <> io.is.ccToggle(ClockDomain.current, otherCD)

  otherCD {
    io.ob := BufferCC(
      io.ib,
      inputAttributes = List(new crossClockMaxDelay(2, useTargetClock = true))
    )

    io.ov := BufferCC(
      io.iv,
      inputAttributes = List(new crossClockMaxDelay(2, useTargetClock = true))
    )
  }
}

object Test extends App {
  VivadoConstraintWriter(SpinalVerilog(Test()))
}
