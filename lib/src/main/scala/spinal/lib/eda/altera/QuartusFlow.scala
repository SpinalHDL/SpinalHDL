package spinal.lib.eda.altera

import java.io.File
import java.nio.file.Paths

import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.lib.eda.bench.Report

import scala.sys.process._
/**
 * Created by PIC32F_USER on 18/05/2016.
 */
object QuartusFlow {
  def doCmd(cmd : String): Unit ={
    println(cmd)
    if(isWindows)
      Process("cmd /C " + cmd) !
    else
      Process(cmd) !
  }

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

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

  def getArea(reportPath : String, family : String): String ={
    import scala.io.Source
    val report = Source.fromFile(reportPath).getLines.mkString
    val intFind = "(\\d+,?)+".r
    val leArea = try {
      family match {
        case "Cyclone V" => intFind.findFirstIn("Logic utilization \\(in ALMs\\)[ ]*;[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " ALMs"
        case "Cyclone IV" | "Cyclone II" =>
          intFind.findFirstIn("Total combinational functions[ ]*;[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " LUT " +
          intFind.findFirstIn("Dedicated logic registers[ ]*;[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " FF "
      }
    }catch{
      case e : Exception => "???"
    }
    return leArea
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
      override def getArea(): String =  (QuartusFlow.getArea(s"${Paths.get(workspacePath,s"$projectName.flow.rpt")}", family))
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
