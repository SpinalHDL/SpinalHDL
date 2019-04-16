package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

case class BmbParameter(addressWidth : Int,
                        dataWidth : Int,
                        lengthWidth : Int,
                        sourceWidth : Int,
                        contextWidth : Int
                        ){
  assert(isPow2(dataWidth))
  def byteCount = dataWidth/8
  def sizeWidth = log2Up(log2Up(byteCount)+1)
  def maskWidth = byteCount
}

case class BmbCmd(p : BmbParameter) extends Bundle{
  val source = UInt(p.sourceWidth bits)
  val address = UInt(p.addressWidth bits)
  val data = UInt(p.dataWidth bits)
  val write = Bool()
  val length = UInt(p.lengthWidth bits)
  val size = UInt(p.sizeWidth bits)
  val mask = UInt(p.maskWidth bits)
  val contextWidth = Bits(p.contextWidth bits)
}

case class BmbRsp(p : BmbParameter) extends Bundle{
  val source = UInt(p.sourceWidth bits)
  val data = UInt(p.dataWidth bits)
  val error = Bool()
  val contextWidth = Bits(p.contextWidth bits)
}


case class Bmb(p : BmbParameter)  extends Bundle with IMasterSlave {
  val cmd = Stream(Fragment(BmbCmd(p)))
  val rsp = Stream(Fragment(BmbRsp(p)))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}
