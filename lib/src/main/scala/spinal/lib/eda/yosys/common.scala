package spinal.lib.eda.yosys

import java.io._
import java.io.PrintWriter
import scala.sys.process._
import scala.collection._

trait Executable {
  val logFile: String = "verbose.log"
  val logPath: String = "logs"
  val workDir: String
  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")
  def runComand: String = this.toString

  def run(): Int = {
    val wrkDir = new File(workDir)
    wrkDir.mkdir
    val lgDir = new File(wrkDir, logPath)
    lgDir.mkdir
    val lgFile = new File(lgDir, logFile)
    val str = new PrintWriter(lgFile)

    str.println("WORDIR: " + wrkDir.toString())
    str.println("COMMAND: " + runComand)
    val log = new ProcessLogger {
      override def err(s: => String): Unit = {
        str.println(s)
        stderr.println(s)
      }
      override def out(s: => String): Unit = {
        str.println(s)
        //stdout.println(s)
      }
      override def buffer[T](f: => T) = f
    }
    val ret: Int = if (isWindows)
                Process("cmd /C " + this.runComand, wrkDir) ! (log)
              else
                Process(this.runComand, wrkDir) ! (log)
    str.close()
    ret
  }

}

trait Makable{
  val prerequisite = new mutable.MutableList[Makable]()
  def isPhony: Boolean = false
  def isFile : Boolean = false
  def target: String

  def addPrerequisite(pre: Makable*) = prerequisite ++= pre
  def makeComand: String = this.toString

  def makejob: String = {
    val phony = if(isPhony) ".PHONY: " + target + "\n"
                else        ""
    val targets = target + " : "
    val prerequisites = prerequisite.map(x => x.target).mkString(" ")
    val ret = if(!isFile) phony+targets + prerequisites + "\n\t" + makeComand
              else ""
    ret
  }
  def getPrerequisiteFromName(str: String) : String = {
    val ret = prerequisite.map(_.target).find(_.matches(str))
    assert(!ret.isEmpty, s"${str} not found in ${prerequisite.map(_.target)}")
    ret.get
  }

  def |>(pre: Makable): pre.type = {
    pre.prerequisite += this
    pre
  }

  // def <|(pre: Makable*): this.type = {
  //   this.prerequisite ++= pre
  //   this
  // }

  /**
    * recursive function to generate the makefile
    */
  def makefile: String = {
    val preJob = prerequisite.map(z => z.makefile).filter(_.nonEmpty) //generate recursivelly a list of job string, filter empty string generated from Makable without dependency (e.g. input Files)
    preJob += makejob                                                 //add the job definition for this job, this is necessary for the recursion
    preJob.mkString("","\n\n","")
  }
}

trait MakablePhony extends Makable{
  override def isPhony : Boolean = true
}

trait MakableFile extends Makable{
  override def isFile : Boolean = true
}

case class InputFile(file: String,workDir: String=".") extends MakableFile{
  def target : String = new File(workDir,file).toString
}

object testMake {
  case class Fi(target: String) extends Makable{
    override def toString: String = target
  }
  case class Comm(target: String) extends Makable{
    override def toString: String = s"penis.${target}"
  }
  def main(args: Array[String]): Unit = {
    //val x = IceProg() <| IcePack() <| IceBram() <| InputFile("test.asc")
    val x = InputFile("test.asc") |> IceBram() |> IcePack()
    val y = x |> IceProg().name("test")
    val z = x |> IceProg().name("test2")
    // println(x.makefile)
    // println("--------------")
    // println(y.makefile)
    // println("--------------")
    println("--------------")

    println(CreateMakefile(x,y,z))
  }
}

object CreateMakefile{
  def apply[T <: Makable](op: T*): String = {
    val nodes = (op.flatMap(_.prerequisite) ++ op).distinct
    val jobStrigs = nodes.map(_.makejob).filter(_.nonEmpty)
    val ret = jobStrigs.mkString("\n\n")
    ret
  }
}

trait PassFail extends Makable{
  override def makeComand: String = "rm PASS FAIL;" //+ super.makeComand + "&& $(date) > PASS || $(date) > FAIL"
  override def target = "PASS"
  def makejob: String = {
    val phony = if(isPhony) ".PHONY: " + target + "\n"
                else        ""
    val targets = target + " : "
    val prerequisites = prerequisite.map(x => x.target).mkString(" ")
    val ret = if(!isFile) phony+targets + prerequisites + "\n\t" + makeComand
              else ""
    ret
  } z
}
