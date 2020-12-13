package spinal.lib.bus.bmi

import spinal.lib.memory.ram._
import spinal.core._
import spinal.lib.IMasterSlave

case class wrPort[T <: Data](addrWidth: Int, payloadType: HardType[T], bweWidth: Int = 0) extends Bundle with IMasterSlave {
  val wr    = Bool()
  val bwe   = Bits(bweWidth bits)
  val waddr = UInt(addrWidth bits)
  val wdata: T = payloadType()
  override def asMaster(): Unit = out(this)
  override def clone: wrPort[T] = wrPort(addrWidth, payloadType).asInstanceOf[this.type]

  def <<(that: wrPort[T]): Unit = {
    this.wr    := that.wr
    this.waddr := that.waddr
    this.wdata := that.wdata
  }

  def >>(that: wrPort[T]): Unit = that << this

  def fromWhen(that: wrPort[T], cond: Bool): Unit = {
    this.wr    := cond && that.wr
    this.waddr := that.waddr
    this.wdata := that.wdata
  }

  def muxWith(that: wrPort[T]): wrPort[T] = {
    val ret = cloneOf(this)
    ret.wr    := Mux(this.wr, this.wr, that.wr)
    ret.waddr := Mux(this.wr, this.waddr, that.waddr)
    ret.wdata := Mux(this.wr, this.wdata, that.wdata)
    ret
  }
}

case class rdPort[T <: Data](addrWidth: Int, payloadType: HardType[T]) extends Bundle with IMasterSlave {
  val rd    = Bool()
  val raddr = UInt(addrWidth bits)
  val rdata: T = payloadType()
  override def asMaster(): Unit = {out(rd, raddr); in(rdata)}
  override def clone: rdPort[T] = rdPort(addrWidth, payloadType).asInstanceOf[this.type]

  def <<(that: rdPort[T]): Unit = {
    this.rd    := that.rd
    this.raddr := that.raddr
    that.rdata := this.rdata
  }

  def >>(that: rdPort[T]): Unit = that << this

  def muxWith(that: rdPort[T]): rdPort[T] = {
    val ret = cloneOf(this)
    ret.rd    := Mux(this.rd, this.rd,    that.rd)
    ret.raddr := Mux(this.rd, this.raddr, that.raddr)
    this.rdata  := ret.rdata
    that.rdata  := ret.rdata
    ret
  }

  def routerAt(cond: Bool, A: rdPort[T], B: rdPort[T]) = {
    A.rd    := this.rd &&  cond
    B.rd    := this.rd && !cond
    A.raddr := this.raddr
    B.raddr := this.raddr
    this.rdata := Mux(cond, A.rdata, B.rdata)
  }
}


/* single Port*/
case class sPort[T <: Data](addrWidth: Int, payloadType: HardType[T], bweWidth: Int = 0) extends Bundle with IMasterSlave {
  val cs, wr = Bool()
  val bwe    = Bits(bweWidth bits)
  val addr   = UInt(addrWidth bits)
  val wdata: T = payloadType()
  val rdata: T = payloadType()

  override def asMaster(): Unit = {
    out(cs, wr, bwe, addr, wdata)
    in(rdata)
  }

  def <<(that: sPort[T]) = {
    this.cs    := that.cs
    this.wr    := that.wr
    this.bwe   := that.bwe
    this.addr  := that.addr
    this.wdata := that.wdata
    that.rdata := this.rdata
  }

  def >>(that: sPort[T]) = that << this

  def muxWith(that: sPort[T]): sPort[T] = {
    val ret = cloneOf(this)
    ret.cs      := Mux(this.cs, this.cs, that.cs)
    ret.bwe     := Mux(this.cs, that.bwe, that.bwe)
    ret.cs      := Mux(this.cs, this.cs, that.cs)
    ret.wdata   := Mux(this.cs, this.wdata, that.wdata)
    this.rdata  := ret.rdata
    that.rdata  := ret.rdata
    ret
  }

  def join(rd: rdPort[T], wr: wrPort[T]): Unit = {
    this.cs    := wr.wr ||  rd.rd
    this.wr    := wr.wr
    this.bwe   := wr.bwe
    this.addr  := Mux(wr.wr, wr.waddr, rd.raddr)
    this.wdata := wr.wdata
    rd.rdata   := this.rdata
  }

  def ~>(that: Ram1rw) = {
    that.io.cs    := this.cs
    that.io.wr    := this.wr
    that.io.bwe   := this.bwe
    that.io.addr  := this.addr
    that.io.wdata := this.wdata.asBits
  }
}

/* true dual Port*/
case class tdPort[T <: Data](addrWidth: Int, payloadType: HardType[T], bweWidth: Int = 0) extends Bundle with IMasterSlave {
  val A = sPort(addrWidth, payloadType)
  val B = sPort(addrWidth, payloadType, bweWidth)

  override def asMaster(): Unit = {
    A.asMaster()
    B.asMaster()
  }

  def <<(that: tdPort[T]) = {
    this.A << that.A
    this.B << that.B
  }

  def >>(that: tdPort[T]) = that << this
}

/* simple dual Port*/
case class sdPort[T <: Data](addrWidth: Int, payloadType: HardType[T], bweWidth: Int = 0) extends Bundle with IMasterSlave {
  val rd = rdPort(addrWidth, payloadType)
  val wr = wrPort(addrWidth, payloadType, bweWidth)

  override def asMaster(): Unit = {
    rd.asMaster()
    wr.asMaster()
  }

  def ~>(that: Ram1r1w): Unit = {
    that.io.wr    := this.wr.wr
    that.io.bwe   := this.wr.bwe
    that.io.waddr := this.wr.waddr
    that.io.wdata := this.wr.wdata.asBits
    that.io.rd    := this.rd.rd
    that.io.raddr := this.rd.raddr
  }

  def <<(that: sdPort[T]): Unit = {
    this.rd << that.rd
    this.wr << that.wr
  }

  def >>(that: sdPort[T]): Unit = that << this
}
