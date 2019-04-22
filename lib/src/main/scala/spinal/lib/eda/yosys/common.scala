package spinal.lib.eda.yosys

import java.io._
import java.io.PrintWriter
import scala.sys.process._
import scala.collection._
import java.nio.file.{Path, Paths}


// trait Executable extends WorkDir{
//   val logFile: String = "verbose.log"
//   val logPath: String = "logs"

//   val isWindows = System.getProperty("os.name").toLowerCase().contains("win")
//   def runComand: String = this.toString

//   def run(): Int = {
//     val dir = new File(workDir)
//     dir.mkdir
//     val lgDir = new File(dir, logPath)
//     lgDir.mkdir
//     val lgFile = new File(lgDir, logFile)
//     val str = new PrintWriter(lgFile)

//     str.println("WORDIR: " + dir.toString())
//     str.println("COMMAND: " + runComand)
//     val log = new ProcessLogger {
//       override def err(s: => String): Unit = {
//         str.println(s)
//         stderr.println(s)
//       }
//       override def out(s: => String): Unit = {
//         str.println(s)
//         //stdout.println(s)
//       }
//       override def buffer[T](f: => T) = f
//     }
//     val ret: Int = if (isWindows)
//                 Process("cmd /C " + this.runComand, dir) ! (log)
//               else
//                 Process(this.runComand, dir) ! (log)
//     str.close()
//     ret
//   }
// }

trait Executable{
  val logFile: Option[Path]

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  /** The command that will be run, default to this.toString */
  def runComand: String = this.toString

  def run(opt:String*): Int = {
    val dir = if(logFile.nonEmpty) Option(logFile.get.getParent) else None
    if(dir.nonEmpty) dir.get.toFile.mkdir

    // val str = new PrintWriter(logFile.getOrElse.toFile)
    val str = if(logFile.nonEmpty) Option(new PrintWriter(logFile.get.toFile)) else None

    val log = new ProcessLogger {
      override def err(s: => String): Unit = {
        if(str.nonEmpty) str.get.println(s)
        stderr.println(s)
      }
      override def out(s: => String): Unit = {
        if(str.nonEmpty) str.get.println(s) else
        stdout.println(s)
      }
      override def buffer[T](f: => T) = f
    }
    val ret: Int = if (isWindows)
                Process("cmd /C " + this.runComand + opt.mkString(" "," ",""), dir.getOrElse(Paths.get(".")).toFile) ! (log)
              else
                Process(this.runComand + opt.mkString(" "," ",""), dir.getOrElse(Paths.get(".")).toFile) ! (log)
    if(logFile.nonEmpty) str.get.close()
    ret
  }
}