package spinal.lib.bus.regif

import spinal.core._
import spinal.core.fiber.Handle
import spinal.lib.{BigIntRicher, WhenBuilder}

import language.experimental.macros
import scala.collection.mutable.ListBuffer

trait BusIfBase extends Area{
  val busDataWidth: Int
  val busAddrWidth: Int

  val askWrite: Bool
  val askRead: Bool
  val doWrite: Bool
  val doRead: Bool
  lazy val cg_en: Bool = True
  lazy val bus_nsbit: Bool = False //NS-bit(Non-Secure Access flag bit)

  val reg_wrerr: Bool
  val reg_rderr: Bool
  val reg_rdata: Bits

  val bus_rdata: Bits
  def bus_slverr: Bool = if(withSecFireWall)  (reg_wrerr || reg_rderr) else reg_rderr

  @deprecated("readData rename to bus_rdata", "2024.12.30")
  lazy val readData: Bits = bus_rdata
  @deprecated("readError rename to bus_rderr", "2024.12.30")
  lazy val readError: Bool = bus_slverr

  val writeData: Bits
  val readSync: Boolean = true
  val withStrb: Boolean
  val withSecFireWall: Boolean

  val wstrb: Bits  //= withstrb generate(Bits(strbWidth bit))
  val wmask: Bits  //= withstrb generate(Bits(busDataWidth bit))
  val wmaskn: Bits //= withstrb generate(Bits(busDataWidth bit))

  private var version = s"SpinalHDL-${Spinal.version}"
  protected var _addrAlignCheck = true
  def setAlignCheck(value: Boolean = false) = _addrAlignCheck = value
  def setVersion(ver: String): Unit = version = ver
  def getVersion: String = version

  def readAddress(): UInt
  def writeAddress(): UInt

  def readHalt(): Unit
  def writeHalt(): Unit

  def busByteWidth: Int = scala.math.ceil(this.busDataWidth / 8.0).toInt
  def bw: Int = busByteWidth

  def wordAddressInc: Int = busByteWidth
  def strbWidth: Int = busByteWidth
  def underbitWidth: Int = log2Up(wordAddressInc)

  def mwdata(sec: Range): Bits = if(withStrb) writeData(sec) & wmask(sec) else writeData(sec)

  /**
   * Gap-aware run-length encoding algorithm (Enhanced: Added block size consistency check)
   * @param blocks Input list of blocks, each block is a consecutive list of Reg
   * @param tolerance Tolerance span L, intervals exceeding this tolerance will break continuity
   * @return List of triples (first block, consecutive block count, interval)
   */
  def groupConsecutiveBlocks(blocks: List[List[RegSlice]]): List[(List[RegSlice], Int, Int)] = {
    if (blocks.isEmpty) return Nil
    if (blocks.size == 1) return List((blocks.head, 1, (blocks.head.last.nextAddr - blocks.head.head.addr).toInt))
    val gaps = blocks
      .sliding(2)
      .collect { case List(a, b) => (b.last.nextAddr - a.head.addr).toInt }
      .toList
    val freqGap = gaps.groupBy(identity).map(_._2).toList.sortBy(-_.size).head.head
    val L = (1 << log2Up(freqGap/bw)) * bw * 2  //Tolerance is 2 * gap
    grupConsecutiveBlocks(blocks, L)
  }

  def grupConsecutiveBlocks(blocks: List[List[RegSlice]], tolerance: Int): List[(List[RegSlice], Int, Int)] = {
    if (blocks.isEmpty) return Nil
    // Get the starting address (value of the first Reg) and size of each block
    val starts = blocks.map(block => block.head.addr)
    val sizes = blocks.map(t => (t.last.nextAddr - t.head.addr).toInt)

    var result = List.empty[(List[RegSlice], Int, Int)]
    var i = 0

    while (i < blocks.length) {
      val firstBlock = blocks(i)
      val firstSize = sizes(i)
      var currentCount = 1
      var currentGap: Option[Int] = None
      var j = i

      // Try to extend the current group
      while (j < blocks.length - 1) {
        val gapVal = (starts(j + 1) - starts(j)).toInt
        val nextSize = sizes(j + 1)

        // Check continuity conditions: interval within tolerance and same block size
        if (gapVal <= tolerance && firstSize == nextSize) {
          if (currentCount == 1) {
            // Only one block in current group, can try to establish new group
            currentGap = Some(gapVal)
            currentCount += 1
          } else {
            // Multiple blocks already in current group, check if interval is consistent
            if (gapVal == currentGap.get) {
              currentCount += 1  // Consistent interval, continue expanding group
            } else {
              // Interval inconsistent, end current group
              j = blocks.length // Break out of loop
            }
          }
        } else {
          // Interval exceeds tolerance or different block size, end current group
          j = blocks.length // Break out of loop
        }
        j += 1
      }

      // Determine output interval: use inter-block interval for multi-block groups,
      // use block size for single-block groups
      val outputGap = if (currentCount > 1) currentGap.get else firstSize
      result = result :+ (firstBlock, currentCount, outputGap)

      // Move to next group
      i += currentCount
    }

    result
  }

  def initStrbMasks() = {
    if (withStrb) {
      (0 until strbWidth).foreach { i =>
        wmask ((i + 1) * 8 - 1 downto i * 8) := Mux(wstrb(i), B(0xFF, 8 bit), B(0, 8 bit))
        wmaskn((i + 1) * 8 - 1 downto i * 8) := Mux(wstrb(i), B(0, 8 bit), B(0xFF, 8 bit))
      }
    }
  }
  /**
   * Strobed write merge: per byte lane of `sec`, `upd` where the lane's strobe is set and `old` elsewhere.
   * One Mux per lane instead of `(old & wmaskn) | (upd & wmask)`: synthesis turns the strobe into the clock enable of
   * the register flip-flops, the AND/OR form costs a LUT per register bit.
   */
  private def strbMerge(old: Bits, upd: Bits, sec: Range): Bits = {
    Cat((sec.min to sec.max).groupBy(_ / 8).toSeq.sortBy(_._1).map { case (lane, bits) =>
      val slice = (bits.max - sec.min) downto (bits.min - sec.min)
      Mux(wstrb(lane), upd(slice), old(slice))
    })
  }
  def wdata(reg: BaseType, sec: Range): Bits = wdata(reg, sec, oper = "normal")
  def wdata(reg: BaseType, sec: Range, oper: String):Bits = {
    lazy val old: Bits = reg match {
      case t: Bits => t
      case t: UInt => t.asBits
      case t: SInt => t.asBits
      case t: Bool => t.asBits
      case _       => SpinalError(s"only accept BiterVector ${reg} for section ${sec} Range")
    }
    oper match {
      case "clear"  => if (withStrb) strbMerge(old, B(0, sec.size bit), sec)                else B(0, sec.size bit)
      case "set"    => if (withStrb) strbMerge(old, B(sec.size bit, default -> True), sec) else Bits(sec.size bit).setAll()
      case "normal" => if (withStrb) strbMerge(old, writeData(sec), sec)                    else writeData(sec)
      case "toggle" => if (withStrb) strbMerge(old, ~old, sec)                              else ~reg.asBits(sec)
      case _ => SpinalError(s"unrecognize '${oper}''")
    }
  }
  def mwdata(pos: Int): Bool = if(withStrb) writeData(pos) & wmask(pos) else writeData(pos)
  def wdata(reg: Bool, pos: Int): Bool = wdata(reg, pos, oper = "normal")
  def wdata(reg: Bool, pos: Int, oper: String): Bool = {
    oper match {
      case "clear"  => if (withStrb) Mux(wstrb(pos / 8), False,          reg) else False
      case "set"    => if (withStrb) Mux(wstrb(pos / 8), True,           reg) else True
      case "normal" => if (withStrb) Mux(wstrb(pos / 8), writeData(pos), reg) else writeData(pos)
      case "toggle" => if (withStrb) Mux(wstrb(pos / 8), ~reg,           reg) else ~reg
      case _ => SpinalError(s"unrecognize '${oper}''")
    }
  }
}
