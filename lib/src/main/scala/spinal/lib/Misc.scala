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

object KeepAttribute{
  object syn_keep_verilog extends AttributeFlag("synthesis syn_keep = 1", COMMENT_ATTRIBUTE){
    override def isLanguageReady(language: Language) : Boolean = language == Language.VERILOG || language == Language.SYSTEM_VERILOG
  }

  object syn_keep_vhdl extends AttributeFlag("syn_keep"){
    override def isLanguageReady(language: Language) : Boolean = language == Language.VHDL
  }
  object keep extends AttributeFlag("keep")

  def apply[T <: Data](that : T) = that.addAttribute(keep).addAttribute(syn_keep_verilog).addAttribute(syn_keep_vhdl)
  val all = List(keep, syn_keep_verilog ,syn_keep_vhdl)
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

  def doCmd(cmd : Seq[String], location : File) = {
    println(cmd.mkString(" "))
    class Logger extends ProcessLogger {
      override def err(s: => String): Unit = { if(!s.startsWith("ar: creating ")) println(s) }
      override def out(s: => String): Unit = {}
      override def buffer[T](f: => T) = f
    }

    assert(Process(cmd, location).! (new Logger()) == 0)
  }
}


object Repeat{
  def apply[T <: Data](value : T, times : Int) = Cat(List.fill(times)(value))
}



object FlowCmdRsp{
  def apply() : FlowCmdRsp[NoData, NoData] = FlowCmdRsp(NoData(), NoData())
  def apply[T <: Data, T2 <: Data](cmdType : HardType[T], rspType : HardType[T2]) : FlowCmdRsp[T, T2] = new FlowCmdRsp(cmdType, rspType)
}

class FlowCmdRsp[T <: Data, T2 <: Data](cmdType : HardType[T], rspType : HardType[T2]) extends Bundle with IMasterSlave {

  val cmd = Flow(cmdType())
  val rsp = Flow(rspType())


  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }

  def setIdleAll(): this.type ={
    cmd.setIdle()
    rsp.setIdle()
    this
  }

  def setIdle(): this.type ={
    cmd.setIdle()
    this
  }

  def isPending(pendingMax : Int) : Bool = pendingMax match{
    case 1 => RegInit(False) setWhen(cmd.valid) clearWhen(rsp.valid)
  }
}