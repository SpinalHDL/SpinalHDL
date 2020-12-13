package spinal.lib.memory.ram

import spinal.core._
import spinal.lib.{IMasterSlave, master, slave}

case class Bist(aw: Int, dw: Int) extends Bundle with IMasterSlave{
  val  bist_rst_n      = Bool()
  val  bist_en         = Bool()
  val  bist_start      = Bool()
  val  bist_ce_mk      = Bool()
  val  bist_check      = Bool()
  val  bist_addr       = UInt(aw bits)
  val  bist_data       = Bits(dw bits)
  val  bist_wen        = Bool()
  val  bist_pivot_ctl  = Bool()
  val  bist_fail       = Bool()

  override def asMaster(): Unit = {
    in(bist_fail)
    out(bist_rst_n, bist_en, bist_start, bist_ce_mk,
      bist_check, bist_addr, bist_data, bist_wen,
      bist_pivot_ctl
    )
  }

  def drive(that: Bist) = {
    require(this.bist_addr.getWidth >= that.bist_addr.getWidth)
    require(this.bist_data.getWidth >= that.bist_data.getWidth)

    that.bist_rst_n     :=  this.bist_rst_n
    that.bist_en        :=  this.bist_en
    that.bist_start     :=  this.bist_start
    that.bist_ce_mk     :=  this.bist_ce_mk
    that.bist_check     :=  this.bist_check
    that.bist_addr      :=  this.bist_addr.resized
    that.bist_data      :=  this.bist_data.resized
    that.bist_wen       :=  this.bist_wen
    that.bist_pivot_ctl :=  this.bist_pivot_ctl
  }

  def >>(that: Bist) = {
    this.drive(that)
    this.bist_fail := that.bist_fail
  }

  def >>>(thats: List[Bist]) = {
    thats.foreach{ that =>
      this.drive(that)
    }
    this.bist_fail := thats.map(_.bist_fail).reduce(_ || _)
  }

  def tileZero() = {
    bist_fail := False
  }
}

case class Ex(short: Boolean) extends Bundle{
  val  ra         = Bits(2 bits)
  val  wa         = Bits(3 bits)
  val  wpulse     = Bits(3 bits)

  def >>(that: Ex) = {
    that.ra := this.ra
    that.wa := this.wa
    that.wpulse := this.wpulse
  }
}

case class Repaire(repaire: Boolean) extends Bundle with IMasterSlave{
  val  cre1         = Bool()
  val  cre2         = Bool()
  val  fca1         = UInt(6 bits)
  val  fca2         = UInt(6 bits)
  val  error_data1  = Bits(7 bits)
  val  error_data2  = Bits(7 bits)

  override def asMaster(): Unit = {
    in(error_data1, error_data2)
    out(cre1, cre2, fca1, fca2)
  }

  def >>(that: Repaire) = {
    that.cre1   := this.cre1
    that.cre2   := this.cre2
    that.fca1   := this.fca1
    that.fca2   := this.fca2
    this.error_data1  := that.error_data1
    this.error_data2  := that.error_data2
  }
}

case class Scan(short: Boolean = false, repaire: Boolean = false) extends Bundle with IMasterSlave{
  val  scan_mode  = Bool()
  val  scan_en    = Bool()
  val  ls         = Bool()
  val  rme        = Bool()
  val  rm         = Bits(2 bits)

  val ex = if(short) null else Ex(short)
  val rp = if(repaire) Repaire(repaire) else null

  if(!short){
    this.getClass.getMethod("ex").invoke(this).asInstanceOf[Data].setName("io_scan")
  }
  if(repaire){
    val name = this.getClass.getMethod("rp").getName()
    this.getClass.getMethod("rp").invoke(this).asInstanceOf[Data].setName("io_scan")
  }

  override def asMaster(): Unit = {
    out(scan_mode, scan_en, ls, rme, rm)
    if(!short){out(ex)}
    if(repaire){master(rp)}
  }

  def >>(that: Scan): Unit = {
    that.scan_mode := this.scan_mode
    that.scan_en   := this.scan_en
    that.ls        := this.ls
    that.rme       := this.rme
    that.rm        := this.rm
    (that.short, this.short) match {
      case (false, false)=> (this.ex  >> that.ex)
      case (false, true) => SpinalError("source dest Ex mismatch")
      case _ =>
    }
    (that.repaire, this.repaire) match {
      case (true, true)  => (this.rp  >> that.rp)
      case (true, false) => SpinalError("source dest Repaire mismatch")
      case _ =>
    }
  }

  def >>>(thats: List[Scan]): Unit = {
    thats.foreach{that =>
      this >> that
    }
  }

  def tileZero() = {
    if(repaire){
      rp.error_data1 := 0
      rp.error_data2 := 0
    }
  }
}
