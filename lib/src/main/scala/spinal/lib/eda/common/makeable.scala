package spinal.lib.eda.common

import java.io._
import java.io.PrintWriter
import java.nio.file.{Path, Paths, Files}
import scala.io.Source
import scala.sys.process._
import scala.collection._
import spinal.core._

import org.apache.commons.io.FilenameUtils
import collection.JavaConverters._

package object string2path {
  implicit def string2Path(str: String): Path = Paths.get(str)
}

object Makeable {
  implicit class PimpMyMakeableList(val list: Seq[Makeable]){// extends Executable {
    // val logFile = None

    // def getNodes: Seq[Makeable] = list.flatMap(_.prerequisite) ++ list

    // /** Generate all the job definition
    //   * This function will return a string with all the UNIQUE job definition
    //   *
    //   * @return a String that cimplement all the necessary makejob
    //   *
    //   * @example{{{ List(JOB1,JOB2,JOB3).makejob }}}
    //   */
    // def makejobs: String = {
    //   val nodes     = getNodes
    //   val jobStrigs = nodes.map(_.makejob).filter(_.nonEmpty).distinct
    //   jobStrigs.mkString("\n")
    // }

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
    // def bundleTest(target: String = "test"): String = {
    //   val nodes = getNodes.collect { case o: PassFail => o }
    //   val ret   = nodes.flatMap(_.getPass).distinct
    //   ret.mkString(s""".PHONY: ${target}\n${target} : """, " ", "")
    // }

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
    // def bundle(target: String)(
    //     filter: PartialFunction[Makeable, Makeable]
    // ): String = {
    //   val nodes   = getNodes.collect(filter)
    //   val ret = nodes.flatMap( node => if(node.isPhony) List(node.getPhonyTargetString) else node.getTarget).distinct
    //   ret.mkString(s""".PHONY: ${target}\n${target} : """, " ", "")
    // }

    /** Collect all target into a given phony target
      *
      * @param target the target name, default to "all"
      * @return a String that collect all the target in a phony one
      *
      * @example{{{ List(JOB1,JOB2,JOB3).all() }}}
      */
    // def all(target: String = "all") =
    //   bundle(target) { case x: Makeable => x }

    /** Create a clean target
      * This target will delete all generated file and folder by the jobs
      *
      * @param target the target name, default to "clean"
      * @return a String that implement a clean target
      *
      * @example{{{ List(JOB1,JOB2,JOB3).clean() }}}
      */
    // def clean(target: String = "clean") = {
    //   val nodes            = getNodes
    //   val inFile           = nodes.collect { case o: InputFile => o.getTarget }.flatten.distinct
    //   val files            = nodes.flatMap(_.getTarget).distinct diff inFile //all file minus inputs
    //   val folders          = files.flatMap(f => Option(f.getParent)).distinct
    //   val pass             = nodes.collect { case o: PassFail => o.getPass }.flatten.distinct
    //   val logs             = nodes.collect { case o: MakeableLog => o.getLog }.flatten.distinct
    //   var rm               = (logs ++ files ++ pass).mkString(s"rm -f ", " ", "")
    //   val rmdir            = folders.mkString("rmdir -p --ignore-fail-on-non-empty ", " ", "")
    //   s".PHONY:${target}\n${target}:\n\t" + rm + " && " + rmdir
    // }

    /** Create script that will generate all the necessary folder structure
      *
      * @return a String that implement the necessary folder structure for the job
      *
      * @example{{{ List(JOB1,JOB2,JOB3).mkdir }}}
      */
    // def mkdir: String = {
    //   val nodes   = getNodes
    //   val files   = nodes.flatMap(_.getTarget).distinct
    //   val folders = files.flatMap(f => Option(f.getParent))
    //   var ret     = folders.mkString("$(info creating dirs...$(shell mkdir -p ", " ", " ))")
    //   ret
    // }

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
    // def makefile: String =
    //   List(
    //     mkdir,
    //     all(),
    //     bundleTest(),
    //     clean(),
    //     makejobs
    //   ).mkString("\n\n")

    // def writeMakefile(path: Path = Paths.get("Makefile")): Unit = {
    //   val makePath = path.getParent()
    //   def doMakefile(path: Path) = {
    //     val mkfile = new PrintWriter(path.toFile)
    //     mkfile.write(makefile)
    //     mkfile.close()
    //   }
    //   if (Files.exists(path)) {
    //     val file = Source.fromFile(path.toFile)
    //     val fileContents = file.getLines.mkString("\n")
    //     val oldHash      = scala.util.hashing.MurmurHash3.stringHash(fileContents)
    //     val newHash      = scala.util.hashing.MurmurHash3.stringHash(makefile)
    //     if (oldHash == newHash) {
    //       SpinalInfo(s"""Makefile in path "${path.toString}" not changed, skipping write""")
    //       file.close()
    //     } else {
    //       SpinalInfo(s"""Makefile in path "${path.toString}" changed, writing a new one""")
    //       doMakefile(path)
    //     }
    //   } else {
    //     SpinalInfo(s"""Makefile not present, creating a new one in "${path.toString}"""")
    //     doMakefile(path)
    //   }
    // }

    // /** @inheritdoc */
    // override def runComand = {
    //   writeMakefile()
    //   "make"
    // }
  }
}

trait Makeable {
  val phony: Option[String]
  val workDirPath: Path //= Paths.get(".").normalize()
  val _binaryPath: Path
  val prerequisite: mutable.MutableList[Makeable]

  /** Add this comand to a phony target
    *
    * @param name the name of the phony target
    */
  def phony(name: String): Makeable

  def isPhony: Boolean = phony.nonEmpty || target.isEmpty

  /** Create the phony string */
  def getPhonyString: String = if (isPhony) f".PHONY: ${getPhonyTargetString}" else ""

  def getPhonyTargetString: String = phony.getOrElse(this.getClass.getSimpleName)

  def workDir(path:Path): Makeable

  /** change tha path of the binary
    *
    * @param path the path of the binary
    * @return a copy of the class with the binary path changed
    */
  def binaryPath(path: Path): Makeable

  def getWorkPath(source: Path) = if(source.isAbsolute) source else workDirPath.normalize.toAbsolutePath.relativize(source.normalize.toAbsolutePath)

  /** Change the output folder of all the target/output
    *
    * @param path the path where redirect all the outputs
    * @return a copy of the class with all output file folder changed
    */
  def outputFolder(path: Path): Makeable = this

  /** A list of what the command need to function */
  def needs: Seq[String] = Seq[String]()

  /** A list of what the command generate */
  def target: Seq[Path] = Seq[Path]()

  /** The comand that is will be writte in the makefile, dafualt to this.toString */
  def makeComand: String = this.toString

  /** Add a prerequisite to the prerequisite list */
  def addPrerequisite(pre: Makeable*) = prerequisite ++= pre

  /** Create a string with all the prerequisite by their extention */
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
    //getRelativePath(ret.get)
    ret.get
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
    //getRelativePath(ret.get)
    ret.get
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
    getWorkPath(ret.get)
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
    getWorkPath(ret.get)
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
    ret.map(getWorkPath(_))
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
    //ret.map(getRelativePath(_))
    ret
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
  def makejob: String = {
    val job = getTargetString + " : " + getPrerequisiteString + "\n\t" + getCommandString
    if (isPhony) getPhonyString + "\n" + getPhonyTargetString + " " + job else job
  }

  /** recursive function to generate the makefile
    * @return a string wit all the makejob necessary to complete the task
    */
  def makefile: String = {
    //generate recursivelly a list of job string, filter empty string generated from Makable without dependency (e.g. input Files)
    val preJob = prerequisite.map(z => z.makefile).filter(_.nonEmpty)
    //add the job definition for this job, this is necessary for the recursion
    (preJob += makejob).mkString("", "\n\n", "")
  }

  def getTarget: Seq[Path] = target.map(getWorkPath(_))
}

trait MakableFile extends Makeable {

  val _binaryPath: Path = Paths.get(".").normalize()
  def binaryPath(path: Path) = this

  /** @inheritdoc */
  override def outputFolder(path: Path): this.type = this

  /** @inheritdoc */
  override def makejob: String = ""
}



trait PassFail extends Makeable {
  val passFile: Option[Path]

  /** Create a PASS file on comamnd succes
    *
    * @param file the path of the PASS file
    */
  def pass(name: Path): PassFail

  def getPass: Option[Path] = if (passFile.nonEmpty) Some(getWorkPath(passFile.get)) else None

  /** @inheritdoc */
  override def outputFolder(path: Path): PassFail = {
    if (passFile.nonEmpty) pass(path.resolve(passFile.get)) else this
  }

  /** @inheritdoc */
  override def target = if (passFile.nonEmpty) super.target :+ passFile.get else super.target

  /** @inheritdoc */
  override def getCommandString: String =
    super.getCommandString + (if (passFile.nonEmpty) " && date > " + getWorkPath(passFile.get) else "")
}

trait MakeableLog extends Makeable {
  val logFile: Option[Path]

  /** Direct stdout/stderr to a file
    *
    * @param file the path of the log file
    */
  def log(name: Path): MakeableLog

  def getLog: Option[Path] = if (logFile.nonEmpty) Some(getWorkPath(logFile.get)) else None

  /** @inheritdoc */
  override def outputFolder(path: Path): MakeableLog = {
    val old = super.outputFolder(path).asInstanceOf[MakeableLog]
    if (logFile.nonEmpty) old.log(path.resolve(logFile.get)) else old
  }

  /** @inheritdoc */
  override def getCommandString: String =
    super.getCommandString + (if (logFile.nonEmpty) " &> " + getWorkPath(logFile.get) else "")
}

object InputFile {
  def apply(file: Path): InputFile   = InputFile(List(file))
  def apply(file: String): InputFile = apply(Paths.get(file))
  def apply[T <: Component](report: SpinalReport[T]): InputFile =
    InputFile(report.rtlSourcesPaths.map(Paths.get(_)).toSeq)
}



case class InputFile(
    file: Seq[Path],
    phony: Option[String] = None,
    workDirPath: Path =Paths.get(".").normalize(),
    prerequisite: mutable.MutableList[Makeable] = mutable.MutableList[Makeable]()
) extends MakableFile {

  def phony(name: String) = this

  /** @inheritdoc */
  def workDir(path: Path) = this.copy(workDirPath = path)

  /** @inheritdoc */
  //override def target = super.target ++ file.map(_.normalize)
  override def target = super.target ++ file.map( file => getWorkPath(file))
}

object Makefile{
  def apply(commands: Makeable*): Makefile = Makefile(commands = commands.toSeq)
}

case class Makefile(
    commands: Seq[Makeable],
    //workdir: Option[Path] = None,
    logFile: Option[Path] = None,
    workDirPath: Path = Paths.get(".").normalize(),
    binaryPath: Path = Paths.get("make")
  ) extends Executable{

  def workDir(path: Path) = this.copy(workDirPath=path)

  def getNodes: Seq[Makeable] = {
    val nodes = commands.flatMap(_.prerequisite) ++ commands
    nodes
    //nodes.map(node => node.workDir(this.workDirPath.normalize.resolve(node.workDirPath)))
  }

  /** Generate all the job definition
    * This function will return a string with all the UNIQUE job definition
    *
    * @return a String that cimplement all the necessary makejob
    *
    * @example{{{ List(JOB1,JOB2,JOB3).makejob }}}
    */
  def makejobs: String = {
    val nodes     = getNodes
    val jobStrigs = nodes.map(_.makejob).filter(_.nonEmpty).distinct
    jobStrigs.mkString("\n")
  }

  /** Make each element of the given Seq prerequisite of the next job
    *
    * @param pre the job that depends on
    * @return pre wit this added in his dependecy list
    *
    * @example{{{ List(JOB1,JOB2,JOB3) |> JOB3 }}}
    */
  // def |>(pre: Makeable): Makeable = {
  //   commands.foreach(_ |> pre)
  //   pre
  // }

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
  def bundle(target: String)(
      filter: PartialFunction[Makeable, Makeable]
  ): String = {
    val nodes   = getNodes.collect(filter)
    val ret = nodes.flatMap( node => if(node.isPhony) List(node.getPhonyTargetString) else node.getTarget).distinct
    ret.mkString(s""".PHONY: ${target}\n${target} : """, " ", "")
  }

  /** Collect all target into a given phony target
    *
    * @param target the target name, default to "all"
    * @return a String that collect all the target in a phony one
    *
    * @example{{{ List(JOB1,JOB2,JOB3).all() }}}
    */
  def all(target: String = "all") =
    bundle(target) { case x: Makeable => x }

  /** Create a clean target
    * This target will delete all generated file and folder by the jobs
    *
    * @param target the target name, default to "clean"
    * @return a String that implement a clean target
    *
    * @example{{{ List(JOB1,JOB2,JOB3).clean() }}}
    */
  def clean(target: String = "clean") = {
    val nodes            = getNodes
    val inFile           = nodes.collect { case o: InputFile => o.getTarget }.flatten.distinct
    val files            = nodes.flatMap(_.getTarget).distinct diff inFile //all file minus inputs
    val folders          = files.flatMap(f => Option(f.getParent)).distinct
    val pass             = nodes.collect { case o: PassFail => o.getPass }.flatten.distinct
    val logs             = nodes.collect { case o: MakeableLog => o.getLog }.flatten.distinct
    var rm               = (logs ++ files ++ pass).mkString(s"rm -f ", " ", "")
    val rmdir            = folders.mkString("rmdir -p --ignore-fail-on-non-empty ", " ", "")
    s".PHONY:${target}\n${target}:\n\t" + rm + " && " + rmdir
  }

  /** Create script that will generate all the necessary folder structure
    *
    * @return a String that implement the necessary folder structure for the job
    *
    * @example{{{ List(JOB1,JOB2,JOB3).mkdir }}}
    */
  def mkdir: String = {
    val nodes   = getNodes
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
  def makefile: String =
    List(
      mkdir,
      all(),
      bundleTest(),
      clean(),
      makejobs
    ).mkString("\n\n")

  def writeMakefile(path: Path = Paths.get("Makefile")): Unit = {
    val makePath = path.getParent()
    def doMakefile(path: Path) = {
      val mkfile = new PrintWriter(path.toFile)
      mkfile.write(makefile)
      mkfile.close()
    }
    if (Files.exists(path)) {
      val file = Source.fromFile(path.toFile)
      val fileContents = file.getLines.mkString("\n")
      val oldHash      = scala.util.hashing.MurmurHash3.stringHash(fileContents)
      val newHash      = scala.util.hashing.MurmurHash3.stringHash(makefile)
      if (oldHash == newHash) {
        SpinalInfo(s"""Makefile in path "${path.toString}" not changed, skipping write""")
        file.close()
      } else {
        SpinalInfo(s"""Makefile in path "${path.toString}" changed, writing a new one""")
        doMakefile(path)
      }
    } else {
      SpinalInfo(s"""Makefile not present, creating a new one in "${path.toString}"""")
      doMakefile(path)
    }
  }

  /** @inheritdoc */
  override def runComand = {
    val x = f"mkdir -p ${workDirPath}" !! ;
    writeMakefile(workDirPath.resolve("Makefile").normalize)
    f"make -C ${workDirPath}"
  }

}