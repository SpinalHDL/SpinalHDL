package spinal.lib.bus.regif

import spinal.lib._

import scala.collection.{immutable, mutable}

trait RegIfRef{
  val dw: Int = 32
  val strbWidth = dw/8
  val ACCESS_LIST = List(
    AccessType.RO, AccessType.RW, AccessType.RC, AccessType.RS,
    AccessType.WRC, AccessType.WRS, AccessType.WC, AccessType.WS,
    AccessType.WSRC, AccessType.WCRS, AccessType.W1C, AccessType.W1S,
    AccessType.W1T, AccessType.W0C, AccessType.W0S, AccessType.W0T,
    AccessType.W1SRC, AccessType.W1CRS, AccessType.W0SRC, AccessType.W0CRS,
    AccessType.WO, AccessType.W0C, AccessType.W0S, AccessType.W1,
    AccessType.WO1, AccessType.NA, AccessType.NA, AccessType.NA, //AccessType.W1P, AccessType.W0P,
    AccessType.ROV, AccessType.HSRW, AccessType.RWHS
  )
  /* todo  W1P, WOP test
  * */

  val maps = immutable.HashMap(ACCESS_LIST.zipWithIndex.map(t => t._2 -> t._1): _*)

  val NUM_REGISTERS = ACCESS_LIST.size

  val writeHistory = mutable.HashSet[BigInt]()
  protected val FailMsg = new scala.collection.mutable.ListBuffer[String]()

  def putMsg(msg: String) = if(!msg.isEmpty) FailMsg.append(msg)
  def isFail = !FailMsg.isEmpty

  protected def onRead(accType: AccessType, oldVal: BigInt): BigInt = accType match {
    case AccessType.RO    => oldVal
    case AccessType.ROV   => oldVal
    case AccessType.RW    => oldVal
    case AccessType.RC    => 0
    case AccessType.RS    => (BigInt(1) << dw) - 1
    case AccessType.WRC   => 0
    case AccessType.WRS   => (BigInt(1) << dw) - 1
    case AccessType.WC    => oldVal
    case AccessType.WS    => oldVal
    case AccessType.WSRC  => 0
    case AccessType.WCRS  => (BigInt(1) << dw) - 1
    case AccessType.W1C   => oldVal
    case AccessType.W1S   => oldVal
    case AccessType.W1T   => oldVal
    case AccessType.W0C   => oldVal
    case AccessType.W0S   => oldVal
    case AccessType.W0T   => oldVal
    case AccessType.W1SRC => 0
    case AccessType.W1CRS => (BigInt(1) << dw) - 1
    case AccessType.W0SRC => 0
    case AccessType.W0CRS => (BigInt(1) << dw) - 1
    case AccessType.WO    => oldVal
    case AccessType.WOC   => oldVal
    case AccessType.WOS   => oldVal
    case AccessType.W1    => oldVal
    case AccessType.WO1   => oldVal
    case AccessType.NA    => oldVal
    case AccessType.W1P   => oldVal
    case AccessType.W0P   => oldVal
    case AccessType.HSRW  => oldVal
    case AccessType.RWHS  => oldVal
    case _                => oldVal
  }

  def compare(acc: AccessType, addr: Long, dutdata: BigInt, refdata: BigInt, msg: String) = {
    /* Why use HexString(32)  for avoid negative BigInt issue: BigInt(-0x376a80d7) == BigInt(c8957f29) */
    if(dutdata.hexString(32) == refdata.hexString(32)) {
      ""
    } else {
      f"Error: addr=0x${addr.hexString(16)}[${acc}%5s] fail, dut(0x${dutdata.hexString(32)}) != ref(0x${refdata.hexString(32)})${msg}"
    }
  }

  // Result is the NEXT state of the register
  protected def onWrite(accType: AccessType, oldVal: BigInt, newVal: BigInt, wstrb: BigInt, firstWrite: Boolean): BigInt = {
    def mask: BigInt  = wstrb.toBinInts(strbWidth).reverse.map(t => if (t == 0) "00" else "FF").mkString("").asHex
    def nmask: BigInt = wstrb.toBinInts(strbWidth).reverse.map(t => if (t == 0) "FF" else "00").mkString("").asHex
    val t = accType match {
      case AccessType.RO    =>  oldVal
      case AccessType.ROV   =>  oldVal
      case AccessType.RW    => (oldVal & nmask) |  (newVal & mask)
      case AccessType.RC    =>  oldVal
      case AccessType.RS    =>  oldVal
      case AccessType.WRC   => (oldVal & nmask) |  (newVal & mask)
      case AccessType.WRS   => (oldVal & nmask) |  (newVal & mask)
      case AccessType.WC    =>  oldVal & nmask
      case AccessType.WS    => (oldVal & nmask) |            mask
      case AccessType.WSRC  => (oldVal & nmask) |            mask
      case AccessType.WCRS  => (oldVal & nmask)
      case AccessType.W1C   =>  oldVal          & ~(newVal & mask)
      case AccessType.W1S   =>  oldVal          |  (newVal & mask)
      case AccessType.W1T   =>  oldVal          ^  (newVal & mask)
      case AccessType.W0C   =>  oldVal          &  (newVal | nmask)
      case AccessType.W0S   =>  oldVal          | ~(newVal | nmask)
      case AccessType.W0T   =>  oldVal          ^ ~(newVal | nmask)
      case AccessType.W1SRC =>  oldVal          |  (newVal & mask)
      case AccessType.W1CRS =>  oldVal          & ~(newVal & mask)
      case AccessType.W0SRC =>  oldVal          | ~(newVal | nmask)
      case AccessType.W0CRS =>  oldVal          &  (newVal | nmask)
      case AccessType.WO    => (oldVal & nmask) |  (newVal & mask)
      case AccessType.WOC   =>  oldVal & nmask
      case AccessType.WOS   => (oldVal & nmask) |            mask
      case AccessType.W1    => if (firstWrite) (oldVal & nmask) |  (newVal & mask) else oldVal
      case AccessType.WO1   => if (firstWrite) (oldVal & nmask) |  (newVal & mask) else oldVal
      case AccessType.NA    => BigInt(0)
      case AccessType.W1P   => oldVal          |  (newVal &  mask)
      case AccessType.W0P   => oldVal          ^ ~(newVal | nmask)
      case AccessType.HSRW  => (oldVal & nmask) |  (newVal & mask)
      case AccessType.RWHS  => (oldVal & nmask) |  (newVal & mask)
      case _                =>  oldVal
    }
    t
  }
}
