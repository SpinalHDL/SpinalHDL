package spinal.lib.eda.altera

import spinal.lib.eda.bench.Report

import scala.sys.process._
/**
 * Created by PIC32F_USER on 18/05/2016.
 */
object QuartusFlow {
  def doCmd(cmd : String): Unit ={
    println(cmd)
    Process("cmd /C " + cmd) !
  }

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


  def apply(quartusPath : String,workspacePath : String,toplevelPath : String,family : String,device : String,fmax : Double = 0,processorCount : Int = 1) : Report = {
    val projectName = toplevelPath.split("/").last.split("[.]").head
    val correctedWorkspacePath = workspacePath.replace("/","\\")

    doCmd(s"rmdir /S /Q $correctedWorkspacePath")
    doCmd(s"mkdir $correctedWorkspacePath")
    doCmd(s"copy $toplevelPath $correctedWorkspacePath")

    doCmd(s"""$quartusPath/quartus_map $workspacePath/$projectName --family="$family" --part=$device --source=$workspacePath/$toplevelPath""")
    doCmd(s"""$quartusPath/quartus_fit $workspacePath/$projectName --parallel=$processorCount""")
    doCmd(s"$quartusPath/quartus_sta $workspacePath/$projectName")

    new Report{
      override def getFMax(): Double =  (QuartusFlow.getFMax(s"$correctedWorkspacePath/$projectName.sta.rpt"))
      override def getArea(): String =  (QuartusFlow.getArea(s"$correctedWorkspacePath/$projectName.flow.rpt", family))
    }
  }

  def main(args: Array[String]) {
    val report = QuartusFlow(
      quartusPath="D:/altera_lite/15.1/quartus/bin64",
      workspacePath="E:/tmp/test1",
      toplevelPath="TopLevel.vhd",
      family="Cyclone V",
      device="5CSEMA5F31C6"
     )
    println(report)
  }
}


object QuartusTest {
  def main(args: Array[String]) {


  }
}
