package spinal.lib.bus.regif

import spinal.core.{GlobalData, SpinalInfo}

import java.io.PrintWriter

trait BusIfDoc {
  val name: String
  val suffix: String
  def body(): String

  def path = s"${bi.docPath}/${name}.${suffix}"

  val prefix: String = ""

  protected var header: String = ""

  def setheader(str: String) = {
    header = str
    this
  }

  protected var bi: BusIf = null

  def setBusIf(bif: BusIf) = bi = bif

  def generate(t: BusIf) = {
    bi = t
    this.body()
    this.dump()
  }

  def dump(): Unit = {
    val pw = new PrintWriter(path)
    pw.write(this.body())
    pw.close()
    SpinalInfo(s"dump ${path}.")
  }

  val pc = GlobalData.get.phaseContext

  def clean(str: String): String = {
    str.replace("\n", "\\n").replace("\r", "\\r")
  }
}