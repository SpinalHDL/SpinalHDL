package spinal.lib.bus.regif

import spinal.core.{GlobalData, SpinalInfo}

import java.io.PrintWriter

trait BusIfDoc {
  val name: String
  val suffix: String
  val prefix: String = name
  protected var bi: BusIf = null
  protected var header: String = ""

  def body(): String

  def path = s"${bi.docPath}/${name}.${suffix}"

  def setheader(str: String) = {
    header = str
    this
  }

  def setBusIf(bif: BusIf) = bi = bif

  def generate(t: BusIf) = {
    bi = t
//    this.body()
    this.dump()
  }

  def dump(): Unit = {
    val pw = new PrintWriter(path)
    pw.write(this.body())
    pw.close()
    SpinalInfo(s"dump ${path} ")
  }

  val pc = GlobalData.get.phaseContext

  def clean(str: String): String = {
    str.replace("\n", "\\n").replace("\r", "\\r")
  }
}