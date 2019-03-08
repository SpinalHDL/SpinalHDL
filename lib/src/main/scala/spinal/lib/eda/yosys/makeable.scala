package spinal.lib.eda.yosys

import java.io._
import java.io.PrintWriter
import scala.sys.process._
import scala.collection._
import spinal.core._

trait Makeable extends WorkDir{
  val prerequisite = new mutable.MutableList[Makeable]()
  def needs: Seq[String] = Seq[String]()
  def target: Seq[String]

  def makeComand: String = this.toString

  def addPrerequisite(pre: Makeable*) = prerequisite ++= pre
  def getPrerequisiteString: String = prerequisite.flatMap(pre => needs.map(pre.getTargetFromName(_))).mkString(" ")
  def getTargetString: String = target.map(getPath(_)).mkString(" ")
  def getCommandString: String = makeComand

  def getPrerequisiteFromName(str: String) : String = {
    val pre: mutable.MutableList[String] = prerequisite.flatMap(_.target)
    val ret = pre.find(_.matches(str))
    assert(!ret.isEmpty, s"""Prerequisite ${str} not found in ${this.getClass.getSimpleName}:${pre.mkString("[",",","]")}""")
    getPath(ret.get).toString
  }

  def getTargetFromName(str: String) : String = {
    val ret = target.find(_.matches(str))
    assert(!ret.isEmpty, s"""Target ${str} not found in ${this.getClass.getSimpleName}:${target.mkString("[",",","]")}""")
    getPath(ret.get).toString
  }
  def getAllTargetsFromName(str: String*) : Seq[String] = {
    val ret = str.flatMap(x => target.filter(_.matches(x)))
    //assert(!ret.isEmpty, s"""Target ${str} not found in ${this.getClass.getSimpleName}:${target.mkString("[",",","]")}""")
    ret.map(getPath(_).toString)
  }

  def getAllPrerequisiteFromName(str: String*) : Seq[String] = {
    val pre = prerequisite.flatMap(_.target)
    val ret = str.flatMap(x => pre.filter(_.matches(x)))
    //assert(!ret.isEmpty, s"""Prerequisite ${str} not found in ${this.getClass.getSimpleName}:${pre.mkString("[",",","]")}""")
    ret.map(getPath(_).toString)
  }

  def |>(pre: Makeable): pre.type = {
    pre.prerequisite += this
    pre
  }

  def makejob: String = getTargetString + " : " + getPrerequisiteString + "\n\t" + getCommandString

  /**
    * recursive function to generate the makefile
    */
  def makefile: String = {
    val preJob = prerequisite.map(z => z.makefile).filter(_.nonEmpty) //generate recursivelly a list of job string, filter empty string generated from Makable without dependency (e.g. input Files)
    preJob += makejob                                                 //add the job definition for this job, this is necessary for the recursion
    preJob.mkString("","\n\n","")
  }
}

trait MakableFile extends Makeable{
  override def makejob : String = ""
}

trait MakeablePhony extends Makeable{
  def phonyTarget: String
  def getPhonyString: String = ".PHONY: " + phonyTarget
  override def getTargetString : String = List(phonyTarget,super.getTargetString).mkString(" ")
  override def makejob: String = getPhonyString + "\n" + super.makejob
}

trait PassFail extends Makeable{
  def passFileName = "PASS"
  override def getTargetString : String = List(getPath(passFileName),super.getTargetString).mkString(" ")
  override def getCommandString: String = super.getCommandString + " && date > " + getPath(passFileName).toString
}

trait MakeableLog extends Makeable{
  def logFile = this.getClass.getSimpleName + ".log"
  override def getCommandString : String = super.getCommandString + " &> " + getPath(logFile).toString
}


object InputFile{
    def apply(file: String): InputFile = InputFile(List(file))
    def apply(file: String,workDir: String): InputFile = InputFile(List(file),workDir)
    def apply[T <: Component](report: SpinalReport[T]): InputFile = InputFile(report.rtlSourcesPaths.toSeq)
}

case class InputFile(file: Seq[String],workDir: String=".") extends MakableFile{
  override def target = file
}

object CreateMakefile{
    def apply(op: Makeable*): String = {
      val nodes = (op.flatMap(_.prerequisite) ++ op).distinct
      val jobStrigs = nodes.map(_.makejob).filter(_.nonEmpty)
      val ret = jobStrigs.mkString("\n\n")
      ret
    }

    def collectTetsTarget(target: String = "test")(op: Makeable*): String = {
        val nodes = (op.flatMap(_.prerequisite) ++ op).collect { case o: PassFail => o }
        val ret = nodes.map( x => x.getPath(x.passFileName)).distinct
        ret.mkString(s""".PHONY: ${target}\n${target} : """," ","")
    }

    def createFolderStructure(op: Makeable*): Unit = {
        val nodes = (op.flatMap(_.prerequisite) ++ op).distinct
        nodes.foreach(_.createDir)
    }

    def makeFolderStructure(op: Makeable*): String = {
        val nodes = (op.flatMap(_.prerequisite) ++ op).distinct
        val folders = nodes.map(_.workDir).distinct
        s"""DIRS=${folders.mkString(" ")}\n$$(info $$(shell mkdir -p $$(DIRS)))"""
    }

    def makeClear(op: Makeable*): String = {
        val nodes = (op.flatMap(_.prerequisite) ++ op).distinct
        val logs = nodes.collect{ case o:MakeableLog => o}.map(x => x.getPath(x.logFile))
        val targets = nodes.flatMap(x => x.target.map(x.getPath(_)))
        val folders = nodes.map(_.workDir).distinct.filterNot(_ == ".")
        val toDelete = (logs ++ targets ++ folders).distinct
        s""".PHONY: clear\nclear:\n\trm ${toDelete.mkString(" ")}"""
    }
  }

object testMake {
    def main(args: Array[String]): Unit = {
      //val test =  InputFile(synt) |> YosysFlow.synthesize() |> IceBram() |> IcePack(workDir="a333")
      val x = InputFile("test.asc") |> IceBram() |> IcePack(workDir="a333")
      val y = x |> IceProg().name("test")
      val z = x |> IceProg().name("test2")
      val t1 = x |> IceProg(workDir="a1")
      val t2 = x |> IceProg(workDir="a2")
      val t3 = x |> IceProg(workDir="a3")
      val t4 = x |> IceProg(workDir="a4")
      println("--------------")

      println(CreateMakefile.makeFolderStructure(x,y,z,t1,t2,t3,t4))
      println(CreateMakefile.collectTetsTarget()(x,y,z,t1,t2,t3,t4))
      println(CreateMakefile(x,y,z,t1,t2,t3,t4))
      println(CreateMakefile.makeClear(x,y,z,t1,t2,t3,t4))
    }
  }


