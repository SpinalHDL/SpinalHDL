package spinal.lib.com.eth


import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

case class PhyRx(dataWidth : Int) extends Bundle {
  val error = Bool()
  val data = Bits(dataWidth bits)
}


case class PhyTx(dataWidth : Int) extends Bundle {
  val data = Bits(dataWidth bits)
}

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

  def toRxFlow() = {
    val s1 = Flow(PhyRx(p.dataWidth))
    s1.valid := RegNext(DV)
    s1.data := RegNext(D)
    s1.error := RegNext(ER)

    val s2 = RegNext(s1)

    val ret = Flow(Fragment(PhyRx(p.dataWidth)))
    ret.valid := s2.valid
    ret.fragment := s2.payload
    ret.last := !s1.valid && s2.valid
    ret
  }

  def simReceive(frame : Seq[Int], cd : ClockDomain): Unit ={
    import spinal.core.sim._
    def nibble(value : Int): Unit ={
      D   #= value
      DV  #= true
      ER  #= false
      CRS #= false
      COL #= false
      cd.waitSampling()
      DV  #= false
    }
    for(byte <- frame){
      nibble(byte & 0xF)
      nibble((byte >> 4) & 0xF)
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

case class RmiiParameter(tx : RmiiTxParameter,
                         rx : RmiiRxParameter)

case class RmiiTxParameter(dataWidth : Int)

case class RmiiRxParameter(dataWidth : Int,
                           withEr    : Boolean)

case class RmiiTx(p : RmiiTxParameter) extends Bundle with IMasterSlave {
  val D = Bits(p.dataWidth bits)
  val EN = Bool()

  override def asMaster(): Unit = {
    out(D, EN)
  }

  def fromTxStream() = {
    val ret = MacTxInterFrame(p.dataWidth)
    val data = RegNext(ret.io.output.data)
    val valid = RegNext(ret.io.output.valid)
    D := ClockDomain.current.withRevertedClockEdge()(data)
    EN := ClockDomain.current.withRevertedClockEdge()(valid)
    ret.io.input
  }
}

case class RmiiRx(p : RmiiRxParameter) extends Bundle with IMasterSlave {
  val D = Bits(p.dataWidth bits)
  val CRS_DV = Bool()
  val ER = p.withEr generate Bool()

  override def asMaster(): Unit = {
    out(D, CRS_DV)
    outWithNull(ER)
  }

  def toRxFlow() = {
    val ret = Flow(Fragment(PhyRx(p.dataWidth)))
    val s1 = Flow(PhyRx(p.dataWidth))
    val s2 = RegNext(s1)
    val s3 = RegNext(s2)

    s1.data := RegNext(D)
    s1.valid := RegNext(CRS_DV)
    if (p.withEr) {
      s1.error := RegNext(ER)
    } else {
      s1.error := Bool(false)
    }

    ret.fragment := s3.payload
    ret.valid := s2.valid || s3.valid
    ret.last := !s1.valid && !s2.valid && s3.valid

    ret
  }

  def simReceive(frame : Seq[Int], cd : ClockDomain): Unit ={
    import spinal.core.sim._
    def nibble(value : Int): Unit ={
      D   #= value
      CRS_DV #= true
      ER  #= false
      cd.waitSampling()
      CRS_DV  #= false
    }
    for(byte <- frame){
      nibble(byte & 0xF)
      nibble((byte >> 4) & 0xF)
    }
    cd.waitSampling(6)
  }
}

case class Rmii(p : RmiiParameter) extends Bundle with IMasterSlave {
  val TX = RmiiTx(p.tx)
  val RX = RmiiRx(p.rx)

  override def asMaster(): Unit = {
    master(TX)
    slave(RX)
  }
}
