package spinal.lib.eda.quartus


import java.nio.file.Files

import scala.sys.process._
import java.io.File
/**
 * Created by PIC32F_USER on 18/05/2016.
 */
object QuartusFlow {
  def doCmd(cmd : String): Unit ={
    println(cmd)
    Process("cmd /C " + cmd) !
  }

  def apply(quartusPath : String,workspacePath : String,toplevelPath : String,family : String,device : String,fmax : Double,processorCount : Int = 1) : Unit = {
    val projectName = toplevelPath.split("/").last.split("[.]").head
    val correctedWorkspacePath = workspacePath.replace("/","\\")
    
    doCmd(s"rmdir /S /Q $correctedWorkspacePath")
    doCmd(s"mkdir $correctedWorkspacePath")
    doCmd(s"copy $toplevelPath $correctedWorkspacePath")

    doCmd(s"""$quartusPath/quartus_map $workspacePath/$projectName --family="$family" --part=$device --source=$workspacePath/$toplevelPath""")
    doCmd(s"""$quartusPath/quartus_fit $workspacePath/$projectName --parallel=$processorCount""")
    doCmd(s"$quartusPath/quartus_sta $workspacePath/$projectName")
  }

  def main(args: Array[String]) {
    QuartusFlow(
      quartusPath="D:/altera_lite/15.1/quartus/bin64",
      workspacePath="E:/tmp/test1",
      toplevelPath="TopLevel.vhd",
      family="Cyclone V",
      device="5CSEMA5F31C6",
      fmax=200e6
     )
  }
}

