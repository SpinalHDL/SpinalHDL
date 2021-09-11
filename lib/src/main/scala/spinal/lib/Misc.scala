package spinal.lib

import spinal.core._

import java.io.File
import scala.sys.process.{Process, ProcessLogger}

class BoolPimped(pimped: Bool){

  //Warning, there is no overflow protection
  def genEvent : Event = {
    val e = Event
    val reg = RegInit(False)

    reg := (reg | pimped) & !e.ready
    e.valid := pimped | reg

    e
  }
}

/**
 * Endianness enumeration
 */
sealed trait Endianness
/** Little-Endian */
object LITTLE extends Endianness
/** Big-Endian */
object BIG    extends Endianness





object KeepAttribute{
  object syn_keep_verilog extends AttributeFlag("synthesis syn_keep = 1", COMMENT_ATTRIBUTE){
    override def isLanguageReady(language: Language) : Boolean = language == Language.VERILOG || language == Language.SYSTEM_VERILOG
  }

  object syn_keep_vhdl extends AttributeFlag("syn_keep"){
    override def isLanguageReady(language: Language) : Boolean = language == Language.VHDL
  }
  object keep extends AttributeFlag("keep")

  def apply[T <: Data](that : T) = that.addAttribute(keep).addAttribute(syn_keep_verilog).addAttribute(syn_keep_vhdl)
}

/**
 * Run command
 */
object DoCmd {
  val isWindows: Boolean = System.getProperty("os.name").toLowerCase().contains("win")

  /**
   * Run command
   * @param cmd command to run
   */
  def doCmd(cmd: String): Unit = {
    println(cmd)
    if (isWindows)
      Process("cmd /C " + cmd) !
    else
      Process(cmd) !
  }

  /**
   * Run command with custom CWD
   * @param cmd Command to run
   * @param path CWD of new process
   */
  def doCmd(cmd: String, path: String): Unit = {
    println(cmd)
    if (isWindows)
      Process("cmd /C " + cmd, new java.io.File(path)) !
    else
      Process(cmd, new java.io.File(path)) !

  }

  def doCmdWithLog(cmd: String, path: String): String = {
    val stdOut = new StringBuilder()
    class Logger extends ProcessLogger {
      override def err(s: => String): Unit = {
        //if (!s.startsWith("ar: creating ")) println(s)
      }
      override def out(s: => String): Unit = {
        //println(s)
        stdOut ++= s
      }
      override def buffer[T](f: => T) = f
    }
    Process(cmd, new java.io.File(path)).!(new Logger)
    stdOut.toString()
  }
}