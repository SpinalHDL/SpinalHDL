package spinal.lib.eda.yosys

import java.io._
import java.io.PrintWriter
import java.nio.file.{Path, Paths}
import scala.sys.process._
import scala.collection._
import spinal.core._

import org.apache.commons.io.FilenameUtils

object string2path {
  implicit def string2Path(str: String): Path = Paths.get(str)
}

object Makeable {
  implicit class PimpMyMakeableList(val list: Seq[Makeable]) extends Executable {
    val logFile = None

    def getNodes: Seq[Makeable] = list.flatMap(_.prerequisite) ++ list

    /** Generate all the job definition
      * This function will return a string with all the UNIQUE job definition
      *
      * @return a String that cimplement all the necessary makejob
      *
      * @example{{{ List(JOB1,JOB2,JOB3).makejob }}}
      */
    def makejobs: String = {
      val nodes     = getNodes.distinct
      val jobStrigs = nodes.map(_.makejob).filter(_.nonEmpty)
      jobStrigs.mkString("\n")
    }

    /** Make each element of the given Seq prerequisite of the next job
      *
      * @param pre the job that depends on
      * @return pre wit this added in his dependecy list
      *
      * @example{{{ List(JOB1,JOB2,JOB3) |> JOB3 }}}
      */
    def |>(pre: Makeable): Makeable = {
      list.foreach(_ |> pre)
      pre
    }

    /** Collect all the PASS file into a given phony target
      * @param target the target name, default to "test"
      * @return a String that collect all the test target in a phony one
      *
      * @example{{{ List(JOB1,JOB2,JOB3).bundleTest("test1") }}}
      */
    def bundleTest(target: String = "test"): String = {
      val nodes = getNodes.collect { case o: PassFail => o }
      val ret   = nodes.flatMap(_.getPass).distinct
      ret.mkString(s""".PHONY: ${target}\n${target} : """, " ", "")
    }

    /** Collect all filtered target into a given phonhy target
      *
      * @param target the target name
      * @param filter the partial function that implement the filter
      * @return a String that collect all the filtered target in a phony one
      *
      * @example{{{
      * List(JOB1,JOB2,JOB3).bundle("test1"){
      *   case o: JOB1 => o
      * }
      * }}}
      */
    def bundle(target: String)(filter: PartialFunction[Makeable, Makeable]): String = {
      val nodes = getNodes.collect(filter)
      val ret   = nodes.flatMap(_.getTarget).distinct
      ret.mkString(s""".PHONY: ${target}\n${target} : """, " ", "")
    }

    /** Collect all target into a given phony target
      *
      * @param target the target name, default to "all"
      * @return a String that collect all the target in a phony one
      *
      * @example{{{ List(JOB1,JOB2,JOB3).all() }}}
      */
    def all(target: String = "all") = bundle(target) { case x: Makeable => x }

    /** Create a clean target
      * This target will delete all generated file and folder by the jobs
      *
      * @param target the target name, default to "clean"
      * @return a String that implement a clean target
      *
      * @example{{{ List(JOB1,JOB2,JOB3).clean() }}}
      */
    def clean(target: String = "clean") = {
      val nodes            = getNodes.distinct
      val inFile           = nodes.collect { case o: InputFile => o.getTarget }.flatten.distinct
      val files            = nodes.flatMap(_.getTarget).distinct diff inFile //al file minus the inputs
      val folderUncomplete = files.flatMap(f => Option(f.getParent))
      val folders          = folderUncomplete.flatMap(x => ((1 until x.getNameCount + 1).map(x.subpath(0, _)).reverse))
      val pass             = nodes.collect { case o: PassFail => o.getPass }.flatten.distinct
      val logs             = nodes.collect { case o: MakeableLog => o.getLog }.flatten.distinct
      var rm               = (logs ++ files ++ pass).mkString(s"rm -rf ", " ", "")
      val rmdir            = folders.mkString("rmdir ", " ", "")
      println(folders)
      s".PHONY:${target}\n${target}:\n\t" + rm + " && " + rmdir
    }

    /** Create script that will generate all the necessary folder structure
      *
      * @return a String that implement the necessary folder structure for the job
      *
      * @example{{{ List(JOB1,JOB2,JOB3).mkdir }}}
      */
    def mkdir: String = {
      val nodes   = getNodes.distinct
      val files   = nodes.flatMap(_.getTarget).distinct
      val folders = files.flatMap(f => Option(f.getParent))
      var ret     = folders.mkString("$(info creating dirs...$(shell mkdir -p ", " ", " ))")
      ret
    }

    /** Create the makefile
      * This will create:
      * - all target
      * - test target
      * - clean target
      * - mkdir script
      *
      * @return a string with all the necessary make task
      *
      * @example{{{ List(JOB1,JOB2,JOB3).makefile }}}
      */
    def makefile: String = List(mkdir, all(), bundleTest(), clean(), makejobs).mkString("\n\n")

    /** @inheritdoc */
    override def runComand = {
      val out = new PrintWriter("Makefile")
      out.println(makefile)
      out.close()
      "make"
    }
  }
}

trait Makeable {
  //the pasepath is arcoded to the project folder
  val basePath = Paths.get(".").normalize()
  def getRelativePath(s: Path) = basePath.relativize(s).normalize()

  /** Change the output folder of all the target/output
    *
    * @param path the path where redirect all the outputs
    * @return a copy of the class with all output file folder changed
    */
  def outputFolder(path: Path): Makeable = this
  val prerequisite: mutable.MutableList[Makeable]

  /** A list of what the command need to function */
  def needs: Seq[String] = Seq[String]()

  /** A list of what the command generate */
  def target: Seq[Path] = Seq[Path]()

  /** The comand that is will be writte in the makefile, dafualt to this.toString */
  def makeComand: String = this.toString

  /** Add a prerequisite to the prerequisite list */
  def addPrerequisite(pre: Makeable*) = prerequisite ++= pre

  /** Create a string with all the prerequisite by their extention */
  // def getPrerequisiteString: String = getAllPrerequisiteFromExtension(needs: _*).map(getRelativePath(_)).mkString(" ")
  def getPrerequisiteString: String = getAllPrerequisiteFromExtension(needs: _*).mkString(" ")

  /** Create a string with all generated target file */
  def getTargetString: String = getTarget.mkString(" ")

  /** Create the command string */
  def getCommandString: String = makeComand

  /** Get the prerequiste by his extension
    *
    * @param str the extention of the prerequisite
    * @return the path of the prerequisite with the given extension
    *
    * @example{{{
    * val job1: Makeable = JOB
    * val pretxt = job1.getPrerequisiteFromName("txt")
    * }}}
    */
  def getPrerequisiteFromExtension(str: String): Path = {
    val pre = prerequisite.flatMap(_.target)
    val ret = pre.find(x => FilenameUtils.isExtension(x.toString, str))
    assert(!ret.isEmpty, s"""Prerequisite with extension "${str}" not found in ${this
      .getClass
      .getSimpleName}:${pre.mkString("[", ",", "]")}""")
    getRelativePath(ret.get)
  }

  /** Get the prerequiste by his extension
    *
    * @param str the extention of the prerequisite
    * @return the path of the prerequisite with the given name

    *
    * @example{{{
    * val job1: Makeable = JOB
    * val pretxt = job1.getPrerequisiteFromName("pre.txt")
    * }}}
    */
  def getPrerequisiteFromName(str: String): Path = {
    val pre = prerequisite.flatMap(_.target)
    val ret = pre.find(_.endsWith(str))
    assert(!ret.isEmpty, s"""Prerequisite with name "${str}" not found in ${this.getClass.getSimpleName}:${pre
      .mkString("[", ",", "]")}""")
    getRelativePath(ret.get)
  }

  /** Get the target by his extension
    *
    * @param str the extention of the target
    * @return the path of the target with the given extension
    *
    * @example{{{
    * val job1: Makeable = JOB
    * val targtxt = job1.getTargetFromExtension("txt")
    * }}}
    */
  def getTargetFromExtension(str: String): Path = {
    val ret = target.find(x => FilenameUtils.isExtension(x.toString, str))
    assert(!ret.isEmpty, s"""Target with extension "${str}" not found in ${this
      .getClass
      .getSimpleName}:${target.mkString("[", ",", "]")}""")
    getRelativePath(ret.get)
  }

  /** Get the target by his extension
    *
    * @param str the extention of the target
    * @return the path of the target with the given name
    *
    * @example{{{
    * val job1: Makeable = JOB
    * val targtxt = job1.getTargetFromName("targ.txt")
    * }}}
    */
  def getTargetFromName(str: String): Path = {
    val ret = target.find(_.endsWith(str))
    assert(!ret.isEmpty, s"""Target with name "${str}" not found in ${this.getClass.getSimpleName}:${target
      .mkString("[", ",", "]")}""")
    getRelativePath(ret.get)
  }

  /** Get all targets by their extension
    *
    * @param str the extention of the target
    * @return the path of all targets with the given extension
    *
    * @example{{{
    * val job1: Makeable = JOB
    * val targ = job1.getAllTargetsFromExtension("txt","png")
    * }}}
    */
  def getAllTargetsFromExtension(str: String*): Seq[Path] = {
    val ret = target.filter(x => FilenameUtils.isExtension(x.toString, str.toArray))
    ret.map(getRelativePath(_))
  }

  /** Get all prerequisites by their extension
    *
    * @param str the extention of the prerequisite
    * @return the path of all prerequisite with the fiven extension
    *
    * @example{{{
    * val job1: Makeable = JOB
    * val targ = job1.getAllPrerequisiteFromExtension("txt","png")
    * }}}
    */
  def getAllPrerequisiteFromExtension(str: String*): Seq[Path] = {
    val pre = prerequisite.flatMap(_.target)
    val ret = pre.filter(x => FilenameUtils.isExtension(x.toString, str.toArray))
    ret.map(getRelativePath(_))
  }

  /** Add this to the dependency of the given comand
    *
    * @param pre the comand where this is will be added as dependecy
    * @return the given comand with this added in the dependecy list
    *
    * @example{{{JOB1 |> JOB2}}}
    */
  def |>(pre: Makeable): pre.type = {
    pre.prerequisite += this
    pre
  }

  /** Add this to the dependency of the given comand list
    *
    * @param pre the comand list where this is will be added as dependecy
    * @return the given comands with this added in the dependecy list
    *
    * @example{{{JOB1 |> List(JOB2,JOB2)}}}
    */
  def |>(pre: Seq[Makeable]): Seq[Makeable] = pre.map(this |> _)

  /** Create the makejob comand
    * @return the formatted string that describe this command as a makejob
    */
  def makejob: String = getTargetString + " : " + getPrerequisiteString + "\n\t" + getCommandString

  /** recursive function to generate the makefile
    * @return a string wit all the makejob necessary to complete the task
    */
  def makefile: String = {
    //generate recursivelly a list of job string, filter empty string generated from Makable without dependency (e.g. input Files)
    val preJob = prerequisite.map(z => z.makefile).filter(_.nonEmpty)
    //add the job definition for this job, this is necessary for the recursion
    preJob += makejob
    preJob.mkString("", "\n\n", "")
  }

  def getTarget: Seq[Path] = target.map(getRelativePath(_))
}

trait MakableFile extends Makeable {

  /** @inheritdoc */
  override def outputFolder(path: Path): this.type = this

  /** @inheritdoc */
  override def makejob: String = ""
}

trait MakeablePhony extends Makeable {
  val phony: Option[String]

  /** Add this comand to a phony target
    *
    * @param name the name of the phony target
    */
  def phony(name: String): Makeable

  /** Create the phony string */
  def getPhonyString: String = if (phony.nonEmpty) ".PHONY: " + phony.get else ""

  /** @inheritdoc */
  override def target = if (phony.nonEmpty) super.target :+ Paths.get(phony.get) else super.target

  /** @inheritdoc */
  override def makejob: String = getPhonyString + "\n" + super.makejob
}

trait PassFail extends Makeable {
  val passFile: Option[Path]

  /** Create a PASS file on comamnd succes
    *
    * @param file the path of the PASS file
    */
  def pass(name: Path): PassFail

  def getPass: Option[Path] = if (passFile.nonEmpty) Some(getRelativePath(passFile.get)) else None

  /** @inheritdoc */
  override def outputFolder(path: Path): PassFail = {
    val old = super.outputFolder(path).asInstanceOf[PassFail]
    if (passFile.nonEmpty) pass(path.resolve(passFile.get)) else old
  }

  /** @inheritdoc */
  override def target = if (passFile.nonEmpty) super.target :+ passFile.get else super.target

  /** @inheritdoc */
  override def getCommandString: String =
    super.getCommandString + (if (passFile.nonEmpty) " && date > " + getRelativePath(passFile.get) else "")
}

trait MakeableLog extends Makeable {
  val logFile: Option[Path]

  /** Direct stdout/stderr to a file
    *
    * @param file the path of the log file
    */
  def log(name: Path): MakeableLog

  def getLog: Option[Path] = if (logFile.nonEmpty) Some(getRelativePath(logFile.get)) else None

  /** @inheritdoc */
  override def outputFolder(path: Path): MakeableLog = {
    val old = super.outputFolder(path).asInstanceOf[MakeableLog]
    if (logFile.nonEmpty) old.log(path.resolve(logFile.get)) else old
  }

  /** @inheritdoc */
  override def getCommandString: String =
    super.getCommandString + (if (logFile.nonEmpty) " &> " + getRelativePath(logFile.get) else "")
}

object InputFile {
  def apply(file: Path): InputFile   = InputFile(List(file))
  def apply(file: String): InputFile = apply(Paths.get(file))
  def apply[T <: Component](report: SpinalReport[T]): InputFile =
    InputFile(report.rtlSourcesPaths.map(Paths.get(_)).toSeq)
}

case class InputFile(
    file: Seq[Path],
    prerequisite: mutable.MutableList[Makeable] = mutable.MutableList[Makeable]()
) extends MakableFile {

  /** @inheritdoc */
  override def target = super.target ++ file.map(_.normalize)
}

// object CreateMakefile {
//   def apply(op: Makeable*): String = {
//     val nodes     = (op.flatMap(_.prerequisite) ++ op).distinct
//     val jobStrigs = nodes.map(_.makejob).filter(_.nonEmpty)
//     val ret       = jobStrigs.mkString("\n\n")
//     ret
//   }

//   def collectTetsTarget(target: String = "test")(op: Makeable*): String = {
//     val nodes = (op.flatMap(_.prerequisite) ++ op).collect { case o: PassFail => o }
//     val ret   = nodes.flatMap(_.passFile).distinct
//     ret.mkString(s""".PHONY: ${target}\n${target} : """, " ", "")
//   }

//   def createTarget(target: String = "test")(op: Makeable*): String = {
//     val targets = op.flatMap(_.target)
//     targets.mkString(s""".PHONY: ${target}\n${target} : """, " ", "")
//   }

//   // @TODO
//   def makeFolderStructure(op: Makeable*): String = {
//     val nodes   = (op.flatMap(_.prerequisite) ++ op).distinct
//     val folders = nodes.flatMap(_.target).flatMap(x => Option(x.getParent)) //.getParent.normalize.toString)).distinct
//     if (folders.nonEmpty) s"""DIRS=${folders.mkString(" ")}\n$$(info $$(shell mkdir -p $$(DIRS)))"""
//     else ""
//   }

//   def makeClear(op: Makeable*): String = {
//     val nodes    = (op.flatMap(_.prerequisite) ++ op).distinct
//     val logs     = nodes.collect { case o: MakeableLog => o }.map(_.logFile)
//     val targets  = nodes.flatMap(_.target)
//     val folders  = targets.map(_.getParent.normalize.toString).distinct
//     val toDelete = (logs ++ targets ++ folders).distinct
//     s""".PHONY: clear\nclear:\n\trm ${toDelete.mkString(" ")}"""
//   }
// }
