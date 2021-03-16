package spinal.lib.eda.altera

import java.io.File
import java.nio.file.Paths
import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.lib.DoCmd.doCmd
import spinal.lib.eda.bench.Report

import scala.sys.process._
/**
 * Created by PIC32F_USER on 18/05/2016.
 */
object QuartusFlow {
  def getFMax(staReportPath : String) : Double = {
    import scala.io.Source
    var fmaxLineCounter = -1
    var fMax = Double.PositiveInfinity
    val fMaxReg = """[-+]?(\d*[.])?\d+""".r
    for(line <- Source.fromFile(staReportPath).getLines()) {
      if (line.contains("; Fmax")) fmaxLineCounter = 2
      fmaxLineCounter match {
        case -1 =>
        case 0 => {
          fmaxLineCounter = -1
          val nums = fMaxReg.findAllIn(line)
          val lineFMax = nums.map(_.toDouble).reduce(Math.min(_, _))
          fMax = Math.min(fMax, lineFMax)
        }
        case _ => fmaxLineCounter -= 1
      }
    }
    return fMax*1e6
  }

  def getArea(reportPath : String): String = {
    import scala.io.Source
    val report = Source.fromFile(reportPath).getLines.mkString
    val intFind = "(\\d+,?)+".r
    val foundCycloneV = "Logic utilization \\(in ALMs\\)[ ]*;[ ]*(\\d+,?)+".r.findFirstIn(report)
    try {
      if (foundCycloneV.isDefined)
        intFind.findFirstIn(foundCycloneV.get).get + " ALMs"
      else
        intFind.findFirstIn("Total combinational functions[ ]*;[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " LUT " +
          intFind.findFirstIn("Dedicated logic registers[ ]*;[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " FF "
    } catch {
      case _: Exception => "Unknown device family?"
    }
  }


  def apply(quartusPath : String,workspacePath : String,toplevelPath : String,family : String,device : String,frequencyTarget : HertzNumber = null,processorCount : Int = 1) : Report = {
    val projectName = toplevelPath.split("/").last.split("[.]").head

    val targetPeriod = (if(frequencyTarget != null) frequencyTarget else 400 MHz).toTime

    val workspacePathFile = new File(workspacePath)
    FileUtils.deleteDirectory(workspacePathFile)
    workspacePathFile.mkdir()
    FileUtils.copyFileToDirectory(new File(toplevelPath), workspacePathFile)

    doCmd(s"""${Paths.get(quartusPath,"quartus_map")} ${Paths.get(workspacePath,projectName)} --family="$family" --part=$device --source=${Paths.get(workspacePath,toplevelPath)}""")
    doCmd(s"""${Paths.get(quartusPath,"quartus_fit")} ${Paths.get(workspacePath,projectName)} --parallel=$processorCount""") // --fmax=${(if(frequencyTarget != null) frequencyTarget else 400 MHz).toBigDecimal*1e-6}mhz
    doCmd(s"${Paths.get(quartusPath,"quartus_sta")} ${Paths.get(workspacePath,projectName)}")

    new Report{
      override def getFMax(): Double =  (QuartusFlow.getFMax(s"${Paths.get(workspacePath,s"$projectName.sta.rpt")}"))
      override def getArea(): String =  (QuartusFlow.getArea(s"${Paths.get(workspacePath,s"$projectName.flow.rpt")}"))
    }
  }

  def main(args: Array[String]) {
    val report = QuartusFlow(
      quartusPath="/eda/intelFPGA_lite/17.0/quartus/bin/",
      workspacePath="/home/spinalvm/tmp",
      toplevelPath="TopLevel.vhd",
      family="Cyclone V",
      device="5CSEMA5F31C6",
      frequencyTarget = 1 MHz
     )
    println(report)
  }
}


object QuartusTest {
  def main(args: Array[String]) {


  }
}

object QuartusProject

class QuartusProject(quartusPath: String, workspacePath: String) {
  private def search(file: File, name: String): String = {
    try {
      if (file.isFile) {
        if (file.getName.endsWith(name))
          return file.getAbsoluteFile.toString
      } else
        for (f <- file.listFiles()) {
          val ret = search(f, name)
          if (ret != null)
            return ret
        }
    } catch {
      case _: Exception => println("Invalid path!")
    }
    null
  }

  val qpfPath: String = search(new File(workspacePath), ".qpf")
  val cdfPath: String = search(new File(workspacePath), ".cdf")

  def report(): Unit = {
    val area = QuartusFlow.getArea(search(new File(workspacePath), ".flow.rpt"))
    val fMax = QuartusFlow.getFMax(search(new File(workspacePath), ".sta.rpt"))
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    printf("Area: %s\n\rFMax: %f MHz\n\r", area, fMax/1e6)
  }

  def compile(): QuartusProject = {
    if (qpfPath != null)
      doCmd(s"""${Paths.get(quartusPath,"quartus_sh")} --flow compile $qpfPath""")
    report()
    this
  }

  def program(): Unit = {
    if (cdfPath != null)
      doCmd(s"""${Paths.get(quartusPath,"quartus_pgm")} $cdfPath""")
    report()
  }
}
