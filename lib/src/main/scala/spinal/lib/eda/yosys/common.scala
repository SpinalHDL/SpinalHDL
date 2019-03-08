package spinal.lib.eda.yosys

import java.io._
import java.io.PrintWriter
import scala.sys.process._
import scala.collection._

trait WorkDir{
  def workDir: String
  def getPath(rel: String): File = new File(workDir, rel)
  def createDir: Unit = new File(workDir).mkdir()
}

trait Executable extends WorkDir{
  val logFile: String = "verbose.log"
  val logPath: String = "logs"

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")
  def runComand: String = this.toString

  def run(): Int = {
    val dir = new File(workDir)
    dir.mkdir
    val lgDir = new File(dir, logPath)
    lgDir.mkdir
    val lgFile = new File(lgDir, logFile)
    val str = new PrintWriter(lgFile)

    str.println("WORDIR: " + dir.toString())
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
                Process("cmd /C " + this.runComand, dir) ! (log)
              else
                Process(this.runComand, dir) ! (log)
    str.close()
    ret
  }

}

// trait Makable{
//   val prerequisite = new mutable.MutableList[Makable]()
//   def isPhony: Boolean = false
//   def isFile : Boolean = false
//   def target: String

//   def addPrerequisite(pre: Makable*) = prerequisite ++= pre
//   def makeComand: String = this.toString

//   def makejob: String = {
//     val phony = if(isPhony) ".PHONY: " + target + "\n"
//                 else        ""
//     val targets = target + " : "
//     val prerequisites = prerequisite.map(x => x.target).mkString(" ")
//     val ret = if(!isFile) phony+targets + prerequisites + "\n\t" + makeComand
//               else ""
//     ret
//   }
//   def getPrerequisiteFromName(str: String) : String = {
//     val ret = prerequisite.map(_.target).find(_.matches(str))
//     assert(!ret.isEmpty, s"${str} not found in ${prerequisite.map(_.target)}")
//     ret.get
//   }

//   def |>(pre: Makable): pre.type = {
//     pre.prerequisite += this
//     pre
//   }

//   // def <|(pre: Makable*): this.type = {
//   //   this.prerequisite ++= pre
//   //   this
//   // }

//   /**
//     * recursive function to generate the makefile
//     */
//   def makefile: String = {
//     val preJob = prerequisite.map(z => z.makefile).filter(_.nonEmpty) //generate recursivelly a list of job string, filter empty string generated from Makable without dependency (e.g. input Files)
//     preJob += makejob                                                 //add the job definition for this job, this is necessary for the recursion
//     preJob.mkString("","\n\n","")
//   }
// }

// trait MakablePhony extends Makable{
//   override def isPhony : Boolean = true
// }

// trait MakableFile extends Makable{
//   override def isFile : Boolean = true
// }

// case class InputFile(file: String,workDir: String=".") extends MakableFile{
//   def target = List(file)
// }


// object CreateMakefile{
//   def apply[T <: Makeable](op: T*): String = {
//     val nodes = (op.flatMap(_.prerequisite) ++ op).distinct
//     val jobStrigs = nodes.map(_.makejob).filter(_.nonEmpty)
//     val ret = jobStrigs.mkString("\n\n")
//     ret
//   }
// }
// // The correct one/////////////////////////////////////////////////////////
// // trait Makable extends WorkDir{
// //   val prerequisite = new mutable.MutableList[Makable]()

// //   def target : Seq[String]
// //   def needs: Seq[String] = List[String]()
// //   def isPhony: Boolean = false
// //   def isFile : Boolean = false
// //   def isPassFail: Boolean = false
// //   def passCommand: String = s"""rm ${getPath(passFile)}; ${makeComand} && date > ${getPath(passFile)}"""
// //   def passFile: String = "PASS"

// //   def addPrerequisite(pre: Makable*) = prerequisite ++= pre
// //   def makeComand: String = this.toString

// //   def makejob: String = {
// //     val test = if(isPassFail) s"""test : ${getPath(passFile)}\n""" else ""
// //     val phony = if(isPhony) target.mkString(".PHONY: ", " ", "\n")
// //                 else        ""
// //     val targets = {
// //       val targs = if(isPhony) target else target.map(getPath(_))
// //       val ret   = if(isPassFail) targs :+ getPath(passFile) else targs
// //       ret.mkString(""," "," : ")
// //     }
// //     //((if(isPhony) target else target.map(getPath(_))) :+ getPath(passFile)).mkString(""," "," : ")
// //     val prerequisites = prerequisite.flatMap(pre => needs.map(pre.getTargetFromName(_))).mkString(" ")
// //     val ret = if(!isFile) test + phony + targets + prerequisites + "\n\t" + (if(isPassFail) passCommand else makeComand)
// //               else ""
// //     ret
// //   }

// //   def getPrerequisiteFromName(str: String) : String = {
// //     val pre: mutable.MutableList[String] = prerequisite.flatMap(_.target)
// //     val ret = pre.find(_.matches(str))
// //     assert(!ret.isEmpty, s"""Prerequisite ${str} not found in ${this.getClass.getSimpleName}:${pre.mkString("[",",","]")}""")
// //     getPath(ret.get).toString
// //   }

// //   def getTargetFromName(str: String) : String = {
// //     val ret = target.find(_.matches(str))
// //     assert(!ret.isEmpty, s"""Target ${str} not found in ${this.getClass.getSimpleName}:${target.mkString("[",",","]")}""")
// //     getPath(ret.get).toString
// //   }

// //   def |>(pre: Makable): pre.type = {
// //     pre.prerequisite += this
// //     pre
// //   }

// //   /**
// //     * recursive function to generate the makefile
// //     */
// //   def makefile: String = {
// //     val preJob = prerequisite.map(z => z.makefile).filter(_.nonEmpty) //generate recursivelly a list of job string, filter empty string generated from Makable without dependency (e.g. input Files)
// //     preJob += makejob                                                 //add the job definition for this job, this is necessary for the recursion
// //     preJob.mkString("","\n\n","")
// //   }
// // }

// object testMake {

//   def main(args: Array[String]): Unit = {
//     //val x = IceProg() <| IcePack() <| IceBram() <| InputFile("test.asc")
//     val x = InputFile("test.asc") |> IceBram() |> IcePack()
//     val y = x |> IceProg().name("test")
//     val z = x |> IceProg().name("test2")
//     val t1 = x |> IceProg(workDir="a1")
//     val t2 = x |> IceProg(workDir="a2")
//     val t3 = x |> IceProg(workDir="a3")
//     val t4 = x |> IceProg(workDir="a4")
//     println("--------------")

//     println(CreateMakefile(x,y,z,t1,t2,t3,t4))
//   }
// }


// trait Makeable extends WorkDir{
//   //def workDir = this.getClass.getSimpleName
//   val prerequisite = new mutable.MutableList[Makeable]()
//   def needs: Seq[String] = List[String]()
//   def target: Seq[String]
//   def addPrerequisite(pre: Makeable*) = prerequisite ++= pre

//   def makeComand: String = this.toString
//   def getPrerequisiteString: String = prerequisite.flatMap(pre => needs.map(pre.getTargetFromName(_))).mkString(" ")
//   def getTargetString: String = target.map(getPath(_)).mkString(" ")
//   def getCommandString: String = makeComand

//   def getPrerequisiteFromName(str: String) : String = {
//     val pre: mutable.MutableList[String] = prerequisite.flatMap(_.target)
//     val ret = pre.find(_.matches(str))
//     assert(!ret.isEmpty, s"""Prerequisite ${str} not found in ${this.getClass.getSimpleName}:${pre.mkString("[",",","]")}""")
//     getPath(ret.get).toString
//   }

//   def getTargetFromName(str: String) : String = {
//     val ret = target.find(_.matches(str))
//     assert(!ret.isEmpty, s"""Target ${str} not found in ${this.getClass.getSimpleName}:${target.mkString("[",",","]")}""")
//     getPath(ret.get).toString
//   }

//   def |>(pre: Makeable): pre.type = {
//     pre.prerequisite += this
//     pre
//   }

//   def makejob: String = getTargetString + " : " + getPrerequisiteString + "\n\t" + getCommandString

//   /**
//     * recursive function to generate the makefile
//     */
//   def makefile: String = {
//     val preJob = prerequisite.map(z => z.makefile).filter(_.nonEmpty) //generate recursivelly a list of job string, filter empty string generated from Makable without dependency (e.g. input Files)
//     preJob += makejob                                                 //add the job definition for this job, this is necessary for the recursion
//     preJob.mkString("","\n\n","")
//   }
// }

// trait MakableFile extends Makeable{
//   override def makejob : String = ""
// }

// trait MakeablePhony extends Makeable{
//   def phonyTarget: String
//   def getPhonyString: String = ".PHONY: " + phonyTarget
//   override def getTargetString : String = List(phonyTarget,super.getTargetString).mkString(" ")
//   override def makejob: String = getPhonyString + "\n" + super.makejob
// }

// trait PassFail extends Makeable{
//   def passFileName = "PASS"
//   override def getTargetString : String = List(getPath(passFileName),super.getTargetString).mkString(" ")
//   override def getCommandString: String = super.getCommandString + " && date > " + getPath(passFileName).toString
// }

// trait MakeableLog extends Makeable{
//   def logFile = this.getClass.getSimpleName + ".log"
//   override def getCommandString : String = super.getCommandString + " &> " + getPath(logFile).toString
// }

// case class InputFile(file: String,workDir: String=".") extends MakableFile{
//   override def target = List(file)
// }