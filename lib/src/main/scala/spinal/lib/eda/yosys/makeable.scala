package spinal.lib.eda.yosys

import java.io._
import java.io.PrintWriter
import java.nio.file.{Path, Paths}
import scala.sys.process._
import scala.collection._
import spinal.core._

import org.apache.commons.io.FilenameUtils

object Makeable{
  implicit class PimpMyMakeableList(val list: Seq[Makeable]) extends Executable{
    val logFile = None
    //def toMakeableSeq: Seq[Makeable] = tuple.productIterator.collect{case make: Makeable => make}.toSeq

    def getNodes: Seq[Makeable] = list.flatMap(_.prerequisite) ++ list

    def makejobs: String = {
      val nodes = getNodes.distinct
      val jobStrigs = nodes.map(_.makejob).filter(_.nonEmpty)
      jobStrigs.mkString("\n\n")
    }

    def |>(pre: Makeable): Makeable = {
      list.foreach(_ |> pre)
      pre
    }

    def bundleTest(target: String = "test"): String = {
      val nodes = getNodes.collect { case o: PassFail => o }
      val ret = nodes.flatMap( _.passFile).distinct
      ret.mkString(s""".PHONY: ${target}\n${target} : """," ","")
    }

    def bundle(target: String)(filter: PartialFunction[Makeable,Makeable]): String = {
      val nodes = getNodes.collect(filter)
      val ret = nodes.flatMap( _.target).distinct
      ret.mkString(s""".PHONY: ${target}\n${target} : """," ","")
    }

    def all(target: String = "all") = bundle(target){case x:Makeable => x}

    def clean(target: String = "clean") = {
      val nodes = getNodes.distinct
      val inFile = nodes.collect{ case o:InputFile => o.target}.flatten.distinct
      val files = nodes.flatMap(_.target).distinct diff inFile  //al file minus the inputs
      val folders = files.flatMap(f => Option(f.getParent))
      val pass = nodes.collect{ case o: PassFail => o.passFile }.flatten.distinct
      val logs = nodes.collect{ case o: MakeableLog => o.logFile }.flatten.distinct
      // val rootFiles = paths.map( f => if(Option(f.getParent).isEmpty) f.getFileName) //get all generated files without folders
      // val ret = folders.mkString("FOLDERS= "," ","\n") + pass.mkString("FILES= "," ","\n") + logs.mkString("LOGS= "," ","\n") + s".PHONY:${target}\n${target}:\n\trm -rf $$LOGS $$FILES $$FOLDERS"
      var ret = (logs ++ files ++ folders).mkString(s".PHONY:${target}\n${target}:\n\trm -rf "," ","")
      ret
    }

    def mkdir: String = {
      val nodes = getNodes.distinct
      val files = nodes.flatMap(_.target).distinct
      val folders = files.flatMap(f => Option(f.getParent))
      var ret = folders.mkString("$(info creating dirs...$(shell mkdir -p "," "," ))")
      ret
    }

    def makefile: String = List(all(), makejobs, bundleTest(), clean(), mkdir).mkString("\n\n")

    override def runComand = {
      val out = new PrintWriter("Makefile")
      out.println(makefile)
      out.close()
      "make all"
    }
  }
}

trait Makeable{
  def outputFolder(path: Path): Makeable
  val prerequisite: mutable.MutableList[Makeable]
  def needs: Seq[String] = Seq[String]()
  def target: Seq[Path] = Seq[Path]()

  def makeComand: String = this.toString

  def addPrerequisite(pre: Makeable*) = prerequisite ++= pre
  def getPrerequisiteString: String = getAllPrerequisiteFromExtension(needs:_*).mkString(" ")

  def getTargetString: String = target.map(_.normalize.toString).mkString(" ")
  def getCommandString: String = makeComand

  def getPrerequisiteFromExtension(str: String) : Path = {
    val pre = prerequisite.flatMap(_.target)
    val ret = pre.find(x => FilenameUtils.isExtension(x.toString, str))
    assert(!ret.isEmpty, s"""Prerequisite with extension "${str}" not found in ${this.getClass.getSimpleName}:${pre.mkString("[",",","]")}""")
    ret.get
  }

  def getPrerequisiteFromName(str: String) : Path = {
    val pre = prerequisite.flatMap(_.target)
    val ret = pre.find(_.endsWith(str))
    assert(!ret.isEmpty, s"""Prerequisite with name "${str}" not found in ${this.getClass.getSimpleName}:${pre.mkString("[",",","]")}""")
    ret.get
  }

  def getTargetFromExtension(str: String) : Path = {
    val ret = target.find(x => FilenameUtils.isExtension(x.toString, str))
    assert(!ret.isEmpty, s"""Target with extension "${str}" not found in ${this.getClass.getSimpleName}:${target.mkString("[",",","]")}""")
    ret.get
  }

  def getTargetFromName(str: String) : Path = {
    val ret = target.find(_.endsWith(str))
    assert(!ret.isEmpty, s"""Target with name "${str}" not found in ${this.getClass.getSimpleName}:${target.mkString("[",",","]")}""")
    ret.get
  }

  def getAllTargetsFromExtension(str: String*) : Seq[Path] = {
    val ret = target.filter(x => FilenameUtils.isExtension(x.toString, str.toArray))
    ret
  }

  def getAllPrerequisiteFromExtension(str: String*) : Seq[Path] = {
    val pre = prerequisite.flatMap(_.target)
    val ret = pre.filter(x => FilenameUtils.isExtension(x.toString, str.toArray))
    ret
  }

  def |>(pre: Makeable): pre.type = {
    pre.prerequisite += this
    pre
  }

  def |>(pre: Seq[Makeable]): Seq[Makeable] = {
    pre.foreach(this |> _)
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
  def outputFolder(path: Path): this.type = this
  override def makejob : String = ""
}

trait MakeablePhony extends Makeable{
  val phony: Option[String]
  def phony(name: String): Makeable
  def getPhonyString: String = if(phony.nonEmpty) ".PHONY: " + phony.get else ""
  override def target = if(phony.nonEmpty) super.target :+ Paths.get(phony.get) else super.target
  override def makejob: String = getPhonyString + "\n" + super.makejob
}

trait PassFail extends Makeable{
  val passFile : Option[Path]
  def pass(name: Path): Makeable
  override def target = if(passFile.nonEmpty) super.target :+ passFile.get else super.target
  override def getCommandString: String = if(passFile.nonEmpty) super.getCommandString + " && date > " + passFile.get else super.getCommandString
}

trait MakeableLog extends Makeable{
  val logFile: Option[Path]
  def log(name: Path): Makeable
  def getLogFile: Path = logFile.getOrElse(Paths.get(this.getClass.getSimpleName + ".log"))
  override def getCommandString : String = super.getCommandString + " &> " + getLogFile.toString
}


object InputFile{
    def apply(file: Path): InputFile = InputFile(List(file))
    def apply(file: String): InputFile = apply(Paths.get(file))
    def apply[T <: Component](report: SpinalReport[T]): InputFile = InputFile(report.rtlSourcesPaths.map(Paths.get(_)).toSeq)
}

case class InputFile(file: Seq[Path],prerequisite: mutable.MutableList[Makeable]= mutable.MutableList[Makeable]()) extends MakableFile{
  override def target = super.target ++ file.map(_.normalize)
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
      val ret = nodes.flatMap( _.passFile).distinct
      ret.mkString(s""".PHONY: ${target}\n${target} : """," ","")
  }

  def createTarget(target: String = "test")(op: Makeable*): String = {
      val targets = op.flatMap(_.target)
      targets.mkString(s""".PHONY: ${target}\n${target} : """," ","")
  }

      // @TODO
  def makeFolderStructure(op: Makeable*): String = {
      val nodes = (op.flatMap(_.prerequisite) ++ op).distinct
      val folders = nodes.flatMap(_.target).flatMap(x => Option(x.getParent))//.getParent.normalize.toString)).distinct
      if(folders.nonEmpty) s"""DIRS=${folders.mkString(" ")}\n$$(info $$(shell mkdir -p $$(DIRS)))"""
      else                    ""
  }

  def makeClear(op: Makeable*): String = {
      val nodes = (op.flatMap(_.prerequisite) ++ op).distinct
      val logs = nodes.collect{ case o:MakeableLog => o}.map(_.logFile)
      val targets = nodes.flatMap(_.target)
      val folders = targets.map(_.getParent.normalize.toString).distinct
      val toDelete = (logs ++ targets ++ folders).distinct
      s""".PHONY: clear\nclear:\n\trm ${toDelete.mkString(" ")}"""
  }
}
