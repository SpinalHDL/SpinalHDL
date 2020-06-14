package spinal.lib.com.eth


import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

case class MiiTxParameter(dataWidth : Int,
                          withEr : Boolean)

case class MiiTx(p : MiiTxParameter) extends Bundle with IMasterSlave {
  val CLK = Bool()
  val D = Bits(p.dataWidth bits)
  val EN = Bool()
  val ER = p.withEr generate Bool()

  override def asMaster(): Unit = {
    in(CLK)
    out(D, EN)
    outWithNull(ER)
  }
}

case class MiiRxParameter(dataWidth : Int)

case class MiiRx(p : MiiRxParameter) extends Bundle with IMasterSlave {
  val CLK = Bool()
  val D = Bits(p.dataWidth bits)
  val DV = Bool()
  val ER = Bool()
  val CRS = Bool()
  val COL = Bool()

  override def asMaster(): Unit = {
    out(CLK, D, DV, ER, CRS, COL)
  }

  def toRxFlow() ={
    val ret = Flow(RxPayload(p.dataWidth))
    ret.valid := RegNext(DV)
    ret.data := RegNext(D)
    ret.error := RegNext(ER)
    ret
  }

  def simReceive(frame : Seq[Int], cd : ClockDomain): Unit ={
    import spinal.core.sim._
    def niple(value : Int): Unit ={
      D   #= value
      DV  #= true
      ER  #= false
      CRS #= false
      COL #= false
      cd.waitSampling()
      DV  #= false
    }
    for(byte <- frame){
      niple(byte & 0xF)
      niple((byte >> 4) & 0xF)
    }
    cd.waitSampling(6)
  }
}

case class Mdio() extends Bundle with IMasterSlave{
  val IO = TriState(Bool)
  val C = Bool()

  override def asMaster(): Unit = {
    master(IO)
    out(C)
  }
}

case class MiiParameter(tx : MiiTxParameter,
                        rx : MiiRxParameter)

case class Mii(p : MiiParameter) extends Bundle with IMasterSlave {
  val TX = MiiTx(p.tx)
  val RX = MiiRx(p.rx)

  override def asMaster(): Unit = {
    master(TX)
    slave(RX)
  }
}
