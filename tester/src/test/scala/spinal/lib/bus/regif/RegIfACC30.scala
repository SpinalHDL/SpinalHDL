package spinal.lib.bus.regif

import AccessType._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.AhbLite3
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.bus.amba4.apb._
import spinal.lib.bus.amba4.apb.sim.Apb4Driver
import spinal.lib.bus.wishbone.Wishbone


class RegIfBasicAccessTest(busname: String) extends Component{
  this.setDefinitionName(s"RegIfBasicAccessTest_${busname}")
  val (bus, busif) = busname match{
    case "apb3" => {
      val bus = slave(Apb3(Apb3Config(32, 32)))
      bus -> BusInterface(bus, (0x000, 4 MiB), 0, regPre = "AP")
    }
    case "apb4" => {
      val bus = slave(Apb4(Apb4Config(32, 32)))
      bus -> BusInterface(bus, (0x000, 4 MiB), 0, regPre = "AP")
    }
    case _ => SpinalError("not support yet")
  }

  def assertError(aset: Boolean, msg: String = "SLVERROR assert fail") = bus match {
    case bs: Apb3 => {
      assert(bs.PSLVERROR.toBoolean == aset, msg)
    }
    case bs: Apb4 => {
      assert(bs.PSLVERR.toBoolean == aset, msg)
    }
    case _ => SpinalError("not support yet")
  }

  val reg_ro      = busif.newReg(doc = "RO    ").field(Bits(32 bit), RO   , "7788abcd".asHex, doc = "ro   ")
  val reg_rw      = busif.newReg(doc = "RW    ").field(Bits(32 bit), RW   , "77880002".asHex, doc = "rw   ").asOutput()
  val reg_rc      = busif.newReg(doc = "RC    ").field(Bits(32 bit), RC   , "77880003".asHex, doc = "rc   ").asOutput()
  val reg_rs      = busif.newReg(doc = "RS    ").field(Bits(32 bit), RS   , "77880004".asHex, doc = "rs   ").asOutput()
  val reg_wrc     = busif.newReg(doc = "WRC   ").field(Bits(32 bit), WRC  , "77880005".asHex, doc = "wrc  ").asOutput()
  val reg_wrs     = busif.newReg(doc = "WRS   ").field(Bits(32 bit), WRS  , "77880006".asHex, doc = "wrs  ").asOutput()
  val reg_wc      = busif.newReg(doc = "WC    ").field(Bits(32 bit), WC   , "77880007".asHex, doc = "wc   ").asOutput()
  val reg_ws      = busif.newReg(doc = "WS    ").field(Bits(32 bit), WS   , "77880008".asHex, doc = "ws   ").asOutput()
  val reg_wsrc    = busif.newReg(doc = "WSRC  ").field(Bits(32 bit), WSRC , "77880009".asHex, doc = "wsrc ").asOutput()
  val reg_wcrs    = busif.newReg(doc = "WCRS  ").field(Bits(32 bit), WCRS , "7788000a".asHex, doc = "wcrs ").asOutput()
  val reg_w1c     = busif.newReg(doc = "W1C   ").field(Bits(32 bit), W1C  , "ffffffff".asHex, doc = "w1c  ").asOutput()
  val reg_w1s     = busif.newReg(doc = "W1S   ").field(Bits(32 bit), W1S  , "00000000".asHex, doc = "w1s  ").asOutput()
  val reg_w1t     = busif.newReg(doc = "W1T   ").field(Bits(32 bit), W1T  , "0000ffff".asHex, doc = "w1t  ").asOutput()
  val reg_w0c     = busif.newReg(doc = "W0C   ").field(Bits(32 bit), W0C  , "ffffffff".asHex, doc = "w0c  ").asOutput()
  val reg_w0s     = busif.newReg(doc = "W0S   ").field(Bits(32 bit), W0S  , "00000000".asHex, doc = "w0s  ").asOutput()
  val reg_w0t     = busif.newReg(doc = "W0T   ").field(Bits(32 bit), W0T  , "0000ffff".asHex, doc = "w0t  ").asOutput()
  val reg_w1src   = busif.newReg(doc = "W1SRC ").field(Bits(32 bit), W1SRC, "77880012".asHex, doc = "w1src").asOutput()
  val reg_w1crs   = busif.newReg(doc = "W1CRS ").field(Bits(32 bit), W1CRS, "77880013".asHex, doc = "w1crs").asOutput()
  val reg_w0src   = busif.newReg(doc = "W0SRC ").field(Bits(32 bit), W0SRC, "77880014".asHex, doc = "w0src").asOutput()
  val reg_w0crs   = busif.newReg(doc = "W0CRS ").field(Bits(32 bit), W0CRS, "77880015".asHex, doc = "w0crs").asOutput()
  val reg_wo      = busif.newReg(doc = "WO    ").field(Bits(32 bit), WO   , "77880016".asHex, doc = "wo   ").asOutput()
  val reg_woc     = busif.newReg(doc = "WOC   ").field(Bits(32 bit), WOC  , "77880017".asHex, doc = "woc  ").asOutput()
  val reg_wos     = busif.newReg(doc = "WOS   ").field(Bits(32 bit), WOS  , "77880018".asHex, doc = "wos  ").asOutput()
  val reg_w1      = busif.newReg(doc = "W1    ").field(Bits(32 bit), W1   , "77880019".asHex, doc = "w1   ").asOutput()
  val reg_wo1     = busif.newReg(doc = "WO1   ").field(Bits(32 bit), WO1  , "7788001a".asHex, doc = "wo1  ").asOutput()
  val reg_na      = busif.newReg(doc = "NA    ").field(Bits(32 bit), NA   , "7788001b".asHex, doc = "na   ").asOutput()
  val reg_w1p     = busif.newReg(doc = "W1P   ").field(Bits(32 bit), W1P  , "00000000".asHex, doc = "w1p  ").asOutput()
  val reg_w0p     = busif.newReg(doc = "W0P   ").field(Bits(32 bit), W0P  , "00000000".asHex, doc = "w0p  ").asOutput()
  val reg_hsrw    = busif.newReg(doc = "HSRW  ").field(Bits(32 bit), HSRW , "7788001e".asHex, doc = "hsrw ").asOutput()
                    busif.newReg(doc = "ROV   ").field(Bits(32 bit), ROV  , "abcdef66".asHex, doc = "rov")
  val reg_bmsc_2a = busif.newReg(doc = "BMSC-A").field(Bits(32 bit), W1S  , "00000000".asHex, doc = "32 bit write 1 set").asOutput()
                    busif.newReg(doc = "BMSC-B").parasiteField(reg_bmsc_2a, W1C  , 0         , doc = "32 bit write 1 clear") //two address share one reg
  val reg_bmsc_4a = busif.newReg(doc = "BMSC-A").field(Bits(32 bit),  RW  , 0, doc = "32 bit RW").asOutput()
                    busif.newReg(doc = "BMSC-B").parasiteField(reg_bmsc_4a, W1S  , 0, doc = "32 bit write 1 set")   //4 address share one reg
                    busif.newReg(doc = "BMSC-C").parasiteField(reg_bmsc_4a, W1C  , 0, doc = "32 bit write 1 clear") //4 address share one reg
                    busif.newReg(doc = "BMSC-D").parasiteField(reg_bmsc_4a, RO , 0, doc = "32 bit read only")     //4 address share one reg
  val reg_rwhs    = busif.newReg(doc = "RWHS  ").field(Bits(32 bit), RWHS , "bb88001e".asHex, doc = "rwhs ").asOutput()
  reg_ro := "fedcba98".asHex

  val refdata = List("12345678".asHex, "5a5a5a5a".asHex, "ffffffff".asHex, "00000000".asHex, "37abcdef".asHex, "11111111".asHex, "35af0782".asHex)
  def write(addr: Long, data: BigInt, strb: BigInt = 0xff): Unit = {
    bus match {
      case bs: Apb3     => Apb3Driver(bs, this.clockDomain).write(addr, data)
      case bs: Apb4     => Apb4Driver(bs, this.clockDomain).write(addr, data, strb)
      case bs: AhbLite3 => SpinalError("AhbLIte3 regif test not support yet")
      case bs: Wishbone => SpinalError("Wishbon  regif test not support yet")
    }
    sleep(0)
  }
  def read(addr: Long): BigInt = {
    val t = bus match {
      case bs: Apb3 => Apb3Driver(bs, this.clockDomain).read(addr)
      case bs: Apb4 => Apb4Driver(bs, this.clockDomain).read(addr)
      case bs: AhbLite3 => SpinalError("AhbLIte3 regif test not support yet")
      case bs: Wishbone => SpinalError("Wishbon  regif test not support yet")
    }
    sleep(0)
    t
  }

  def siminit() = {
    this.clockDomain.forkStimulus(2)
    this.clockDomain.waitSampling(100)
  }
  def regression() = {
    siminit()
    tc00_ro   (0x0000)
    tc01_rw   (0x0004)
    tc02_rc   (0x0008)
    tc03_rs   (0x000c)
    tc04_wrc  (0x0010)
    tc05_wrs  (0x0014)
    tc06_wc   (0x0018)
    tc07_ws   (0x001c)
    tc08_wsrc (0x0020)
    tc09_wcrs (0x0024)
    tc10_w1c  (0x0028)
    tc11_w1s  (0x002c)
    tc12_w1t  (0x0030)
    tc13_w0c  (0x0034)
    tc14_w0s  (0x0038)
    tc15_w0t  (0x003c)
    tc16_w1src(0x0040)
    tc17_w1crs(0x0044)
    tc18_w0src(0x0048)
    tc19_w0crs(0x004c)
    tc20_wo   (0x0050)
    tc21_woc  (0x0054)
    tc22_wos  (0x0058)
    tc23_w1   (0x005c)
    tc24_wo1  (0x0060)
    tc25_na   (0x0064)
    tc26_w1p  (0x0068)
    tc27_w0p  (0x006c)
    tc28_hsrw (0x0070)
    tc29_rov  (0x0074)
    tc30_bmsc_2a (0x0078)
    tc31_bmsc_4a (0x0080)
  }
  def tc00_ro   (addr: Long) = {
    val rdata = read(addr)
    assert("fedcba98".asHex == rdata, s"0xfedcba98 != 0x${rdata.hexString(32)}, RW test failed")
    SpinalInfo("tc00 RO - testpass")
  }
  def tc01_rw   (addr: Long) = {
    def test(data: BigInt) = {
      write(addr, data)
      val rdata = read(addr)
      assert(data == rdata, s"0x${data.hexString(32)} != 0x${rdata.hexString(32)}, RW test failed")
    }
    refdata.foreach(test(_))
    SpinalInfo("tc01 RW - test pass")
  }
  def tc02_rc   (addr: Long) = {
    def test(data: BigInt) = {
      write(addr, data)
      read(addr)
      val rdata = read(addr)
      assert(rdata == 0, s"0x${rdata.hexString(32)} != 0x00000000, RC test failed")
    }
    refdata.foreach(test(_))
    SpinalInfo("tc02 RC - test TBA-pass")
  }
  def tc03_rs   (addr: Long) = {
    def test(data: BigInt) = {
      write(addr, data)
      read(addr)
      val rdata = read(addr)
      assert(rdata == "FFFFFFFF".asHex, s"0x${rdata.hexString(32)} != 0xFFFFFFFF, RS test failed")
    }
    refdata.foreach(test(_))
    SpinalInfo("tc03 RS - test TBA-pass")
  }
  def tc04_wrc  (addr: Long) = {
    def test(data: BigInt) = {
      write(addr, data)
      assert(reg_wrc.toBigInt == data, s"0x${data.hexString(32)} != 0x${reg_wrc.toBigInt.hexString(32)}, WRC, write assert fail")
      read(addr)
      val rdata = read(addr)
      assert(rdata == 0, s"0x${rdata.hexString(32)} != 0x00000000, WRC test failed")
    }
    refdata.foreach(test(_))
    SpinalInfo("tc04 WRC - test pass")
  }
  def tc05_wrs  (addr: Long) = {
    def test(data: BigInt) = {
      write(addr, data)
      assert(reg_wrs.toBigInt == data, s"0x${data.hexString(32)} != 0x${reg_wrs.toBigInt.hexString(32)}, WRS, write assert fail")
      read(addr)
      val rdata = read(addr)
      assert(rdata == "FFFFFFFF".asHex, s"0x${rdata.hexString(32)} != 0xFFFFFFFF, WRS test failed")
    }
    refdata.foreach(test(_))
    SpinalInfo("tc05 WRC - test pass")
  }
  def tc06_wc   (addr: Long) = {
    val rdata = read(addr)
    assert(rdata != 0, s"reset value not empty before write")
    write(addr, "abcdef53".asHex)
    val rdata1 = read(addr)
    assert(rdata1 == 0, s"0x${rdata.hexString(32)} != 0x00000000, WC test failed")
    SpinalInfo("tc06 WC - test TBA-pass")
  }
  def tc07_ws   (addr: Long) = {
    val rdata = read(addr)
    assert(rdata != "FFFFFFFF".asHex, s"reset value not high before write")
    write(addr, "abcdef53".asHex)
    val rdata1 = read(addr)
    assert(rdata1 == "FFFFFFFF".asHex, s"0x${rdata.hexString(32)} != 0xFFFFFFFF, WS test failed")
    SpinalInfo("tc07 WC - test TBA-pass")
  }
  def tc08_wsrc (addr: Long) = {
    val rdata = read(addr)
    assert(rdata != 0, s"reset value not empty before write")
    write(addr, "abcdef53".asHex)
    read(addr)
    val rdata1 = read(addr)
    assert(rdata1 == 0, s"0x${rdata.hexString(32)} != 0x00000000, WC test failed")
    SpinalInfo("tc08 WSRC - test TBA-pass")
  }
  def tc09_wcrs (addr: Long) = {
    val rdata = read(addr)
    assert(rdata != "FFFFFFFF".asHex, s"reset value not high before write")
    write(addr, "abcdef53".asHex)
    read(addr)
    val rdata1 = read(addr)
    assert(rdata1 == "FFFFFFFF".asHex, s"0x${rdata.hexString(32)} != 0xFFFFFFFF, WS test failed")
    SpinalInfo("tc09 WCRS - test TBA-pass")
  }
  def tc10_w1c  (addr: Long) = {
    val TV = List(
      "000000ff".asHex -> "ffffff00".asHex,
      "0000aa00".asHex -> "ffff5500".asHex,
      "00330000".asHex -> "ffcc5500".asHex,
      "cc000000".asHex -> "33cc5500".asHex,
      "ff000000".asHex -> "00cc5500".asHex,
      "00f00f00".asHex -> "000c5000".asHex,
      "0000ffff".asHex -> "000c0000".asHex
    )
    def test(t: (BigInt, BigInt)) = {
      write(addr, t._1)
      val rdata = read(addr)
      assert(rdata == t._2, s"0x${rdata.hexString(32)} != 0x${t._2.hexString(32)}, W1C test failed")
    }
    TV.foreach(test)
    SpinalInfo("tc10 W1C - test pass ")
  }
  def tc11_w1s  (addr: Long) = {
    val TV = List(
      "00000023".asHex -> "00000023".asHex,
      "00004500".asHex -> "00004523".asHex,
      "00670000".asHex -> "00674523".asHex,
      "89000000".asHex -> "89674523".asHex,
      "00000000".asHex -> "89674523".asHex,
      "ffff0000".asHex -> "ffff4523".asHex,
      "0000ffff".asHex -> "ffffffff".asHex
    )
    def test(t: (BigInt, BigInt)) = {
      write(addr, t._1)
      val rdata = read(addr)
      assert(rdata == t._2, s"0x${rdata.hexString(32)} != 0x${t._2.hexString(32)}, W1S test failed")
    }
    TV.foreach(test)
    SpinalInfo("tc11 W1S - test pass ")
  }
  def tc12_w1t(addr: Long) = {
    // reg_w1t init  0x0000FFFF
    val data = "abcdef53".asHex
    write(addr, data)
    val rdata = read(addr)
    assert(rdata == "abcd10ac".asHex, s"0x${rdata.hexString(32)} != 0xabcd10ac, W1T test failed")
    write(addr, data)
    val rdata1 = read(addr)
    assert(rdata1 == "0000ffff".asHex, s"0x${rdata1.hexString(32)} != 0x0000ffff, W1T test failed")
    SpinalInfo("tc12 W1S - [TBD-Warning] ")
  }
  def tc13_w0c  (addr: Long) = {
    // reg_w0c init  0xFFFFFFFF
    val TV = List(
      "ffffff00".asHex -> "ffffff00".asHex,
      "ffff55ff".asHex -> "ffff5500".asHex,
      "ff33ffff".asHex -> "ff335500".asHex,
      "ccffffff".asHex -> "cc335500".asHex,
      "00ffffff".asHex -> "00335500".asHex,
      "ff0ff0ff".asHex -> "00035000".asHex,
      "ffff0000".asHex -> "00030000".asHex
    )

    def test(t: (BigInt, BigInt)) = {
      write(addr, t._1)
      val rdata = read(addr)
      assert(rdata == t._2, s"0x${rdata.hexString(32)} != 0x${t._2.hexString(32)}, W0C test failed")
    }

    TV.foreach(test)
    SpinalInfo("tc13 WOC - test pass")
  }
  def tc14_w0s  (addr: Long) = {
    val TV = List(
      "ffffff00".asHex -> "000000ff".asHex,
      "ffff00ff".asHex -> "0000ffff".asHex,
      "0fffffff".asHex -> "f000ffff".asHex,
      "ffc3ffff".asHex -> "f03cffff".asHex,
      "f5ffffff".asHex -> "fa3cffff".asHex
    )

    def test(t: (BigInt, BigInt)) = {
      write(addr, t._1)
      val rdata = read(addr)
      assert(rdata == t._2, s"0x${rdata.hexString(32)} != 0x${t._2.hexString(32)}, W0S test failed")
    }

    TV.foreach(test)
    SpinalInfo("tc14 WOS - test pass")
  }
  def tc15_w0t  (addr: Long) = {
    //reg_w0t = 0x0000FFFF
    val data = "00ffff00".asHex
    write(addr, "00ffff00".asHex)
    val rdata = read(addr)
    assert(rdata == "ff00ff00".asHex, s"0x${rdata.hexString(32)} != 0xFF00FF00, W0T test failed")
    write(addr, "00ffff00".asHex)
    val rdata1 = read(addr)
    assert(rdata1 == "0000FFFF".asHex, s"0x${rdata1.hexString(32)} != 0x0000FFFF, W0T test failed")
    SpinalInfo("tc15 W0T - test pass")
  }
  def tc16_w1src(addr: Long) = {
    SpinalInfo("W1SRC - [TBD-Warning] ")
  }
  def tc17_w1crs(addr: Long) = {
    SpinalInfo("W1CRS - [TBD-Warning] ")
  }
  def tc18_w0src(addr: Long) = {
    SpinalInfo("W0SRC - [TBD-Warning] ")
  }
  def tc19_w0crs(addr: Long) = {
    SpinalInfo("W1CRS - [TBD-Warning] ")
  }
  def tc20_wo   (addr: Long) = {
    write(addr, "12345678".asHex)
    assert(reg_wo.toBigInt == "12345678".asHex, s"0x12345678 != 0x${reg_wo.toBigInt.hexString(32)}, WO, write assert fail")
    write(addr, "abcdef12".asHex)
    assertError(false, s"WO test  readError Fail")
    assert(reg_wo.toBigInt == "abcdef12".asHex, s"0xabcdef12 != 0x${reg_wo.toBigInt.hexString(32)}, WO, write assert fail")
    read(addr)
    assertError(true, s"WO test  readError Fail")
    SpinalInfo("WO - test pass ")
  }
  def tc21_woc  (addr: Long) = {SpinalInfo("tc21 WOC  [TBD-Warning]")}
  def tc22_wos  (addr: Long) = {SpinalInfo("tc22 WOS  [TBD-Warning]")}
  def tc23_w1   (addr: Long) = {
    write(addr, "12345678".asHex)
    assert(reg_w1.toBigInt == "12345678".asHex, s"0x12345678 != 0x${reg_w1.toBigInt.hexString(32)}, W1, write assert fail")
    write(addr, "00ff1344".asHex)
    assert(reg_w1.toBigInt == "12345678".asHex, s"0x12345678 != 0x${reg_w1.toBigInt.hexString(32)}, W1, write assert fail")
    val rdata = read(addr)
    assert(rdata == "12345678".asHex, s"0x${rdata.hexString(32)} != 0x12345678, W1 read test failed")
    SpinalInfo("tc23 W1  test pass")
  }
  def tc24_wo1  (addr: Long) = {
    write(addr, "fabcdef3".asHex)
    assert(reg_wo1.toBigInt == "fabcdef3".asHex, s"0xfabcdef3 != 0x${reg_wo1.toBigInt.hexString(32)}, WO1, write assert fail")
    write(addr, "fabcdef3".asHex)
    assert(reg_wo1.toBigInt == "fabcdef3".asHex, s"0xfabcdef3 != 0x${reg_wo1.toBigInt.hexString(32)}, WO1, write assert fail")
    read(addr)
    assertError(true, s"WO1 test  readError Fail")
    SpinalInfo("tc24 WO1  test pass")}
  def tc25_na   (addr: Long) = {SpinalInfo("tc25 NA   [TBD-Warning]")}
  def tc26_w1p  (addr: Long) = {SpinalInfo("tc26 W1P  [TBD-Warning]")}
  def tc27_w0p  (addr: Long) = {SpinalInfo("tc27 W0P  [TBD-Warning]")}
  def tc28_hsrw (addr: Long) = {SpinalInfo("tc28 HSRW [TBD-Warning]")}
  def tc29_rov  (addr: Long) = {
    val rdata = read(addr)
    assert(rdata == "abcdef66".asHex, s"0x${rdata.hexString(32)} != 0xabcdef66, ROV read test failed")
    SpinalInfo("tc29 ROV  test pass")
  }
  def tc30_bmsc_2a (addr: Long) = {
    val addr0 = addr       //w1s
    val addr1 = addr + 4   //w1c
    val TV = List(
      "000000ff".asHex -> "000000ff".asHex,
      "0000aa00".asHex -> "0000aa00".asHex,
      "00330000".asHex -> "00330000".asHex,
      "cc000000".asHex -> "cc000000".asHex,
      "ff000000".asHex -> "ff000000".asHex,
      "00f00f00".asHex -> "00f00f00".asHex,
      "0000ffff".asHex -> "0000ffff".asHex
    )
    def test(t: (BigInt, BigInt)) = {
      write(addr0, t._1)
      val rdata = read(addr0)
      assert(rdata == t._2, s"0x${rdata.hexString(32)} != 0x${t._2.hexString(32)}, BMSC W1S addr0 read test failed")
      val rdata2 = read(addr1)
      assert(rdata2 == t._2, s"0x${rdata2.hexString(32)} != 0x${t._2.hexString(32)}, BMSC W1S addr1 read test failed")
      write(addr1, t._1)
      val rdata3 = read(addr0)
      assert(rdata3 == 0, s"0x${rdata3.hexString(32)} != 0x00000000, BMSC W1C addr read test failed")
    }
    TV.foreach(test)
    SpinalInfo("tc30 BMSC-2A - test pass ")
  }
  def tc31_bmsc_4a (addr: Long) = {
    val addr0 = addr      //rw
    val addr1 = addr + 4  //w1s
    val addr2 = addr + 8  //w1c
    val addr3 = addr + 12 //ro
    write(addr0, "0000FFFF".asHex)
    var rdata = read(addr3)
    assert(rdata == "0000FFFF".asHex, s"0x${rdata.hexString(32)} != 0x0000FFFF, BMSC4R -RW test failed")
    write(addr1, "00ff0000".asHex)
    rdata = read(addr3)
    assert(rdata == "00FFFFFF".asHex, s"0x${rdata.hexString(32)} != 0x00FFFFFF, BMSC4R -w1S test failed")
    write(addr2, "001c3a5f".asHex)
    rdata = read(addr3)
    assert(rdata == "00e3c5a0".asHex, s"0x${rdata.hexString(32)} != 0x00e3c5a0, BMSC4R -w1C test failed")
    SpinalInfo("tc31 BMSC-4A - test pass ")
  }
}

object BasicTest{
  val spinalConfig = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING,
      resetKind = ASYNC,
      resetActiveLevel = LOW
    ),
    defaultClockDomainFrequency = FixedFrequency(200 MHz),
    targetDirectory = "./out/rtl/",
    headerWithDate = true,
    inlineConditionalExpression = true,
    oneFilePerComponent = false,
    nameWhenByFile = false,
    removePruned = true,
    anonymSignalPrefix = "t",
    mergeAsyncProcess = true)

  val simcfg = SpinalSimConfig().withConfig(spinalConfig)

  def genrtl(name: String = "apb4") = {
    name match {
      case "apb3" => spinalConfig.generateVerilog(new RegIfBasicAccessTest("apb3"))
      case "apb4" => spinalConfig.generateVerilog(new RegIfBasicAccessTest("apb4"))
      case "demo" => spinalConfig.generateVerilog(new RegIfExample)
      case _      => spinalConfig.generateVerilog(new RegIfExample)
    }
  }

  def main(args: Array[String] = Array("apb4")) = {
    val bus = args.head
    simcfg
      .compile(new RegIfBasicAccessTest(bus))
      .doSimUntilVoid(s"regif_${bus}_test") { dut =>
        dut.regression()
        simSuccess()
      }
  }
}
