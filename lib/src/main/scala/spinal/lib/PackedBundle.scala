package spinal.lib

import spinal.core._

/**
  * Similar to Bundle but with bit packing capabilities.
  * Use pack implicit functions to assign fields to bit locations
  * - pack(Range, [Endianness]) - Packs the data into Range aligning to bit Endianness if too wide
  * - packAt(Position) - Packs the data starting at Position. Uses full data length
  * - packTo(Position) - Packs the data ending at Position. Uses full data length
  *
  * Providing no location tag will place the next data value immediately after the last.
  *
  * @example {{{
  *     val regWord = new PackedBundle {
  *       val init = Bool().packAt(0) // Bit 0
  *       val stop = Bool() // Bit 1
  *       val result = Bits(16 bit).packTo(31) // Bits 16 to 31
  *     }
  * }}}
  *
  */
class PackedBundle extends Bundle {

  class TagBitPackExact(val range: Range, val endianness: Endianness) extends SpinalTag

  private def computePackMapping(): Seq[(Range, Data)] = {
    var lastPos = 0
    elements.map(_._2).map(d => {
      val r = d.getTag(classOf[TagBitPackExact]) match {
        case t: Some[TagBitPackExact] =>
          t.x.range
        case None =>
          (lastPos+d.getBitsWidth) downto (lastPos+1)
      }
      lastPos = r.high
      (r, d)
    })
  }

  override def asBits: Bits = {
    val mappings = computePackMapping()
    val maxWidth = mappings.map(_._1.high).max + 1
    val packed = B(0, maxWidth bit)
    for ((range, data) <- mappings) {
      val endianness: Endianness = data.getTag(classOf[TagBitPackExact]) match {
        case t: Some[TagBitPackExact] => t.x.endianness
        case _ => LITTLE
      }
      endianness match {
        case LITTLE =>
          packed(range) := data.asBits.takeLow(range.size.min(data.getBitsWidth))
        case BIG =>
          packed(range) := data.asBits.takeHigh(range.size.min(data.getBitsWidth))
      }
    }
    packed
  }

  override def assignFromBits(bits: Bits): Unit = assignFromBits(bits, bits.getBitsWidth, 0)

  override def assignFromBits(bits: Bits, hi: Int, lo: Int): Unit = {
    val mappings = computePackMapping()
    for((elRange, el) <- mappings) {
      val endianness: Endianness = el.getTag(classOf[TagBitPackExact]) match {
        case t: Some[TagBitPackExact] => t.x.endianness
        case _ => LITTLE
      }
      if (!(lo >= elRange.high || hi < elRange.low)) {
        val diff = (elRange.size - el.getBitsWidth).max(0)
        endianness match {
          case LITTLE =>
            val boundedBitsRange = hi.min(elRange.high-diff) downto lo.max(elRange.low)
            el.assignFromBits(bits(boundedBitsRange),
              boundedBitsRange.high - elRange.low,
              boundedBitsRange.low - elRange.low)
          case BIG =>
            val boundedBitsRange = hi.min(elRange.high) downto lo.max(elRange.low+diff)
            el.assignFromBits(bits(boundedBitsRange),
              boundedBitsRange.high - elRange.low-diff,
              boundedBitsRange.low - elRange.low-diff)
        }
      }
    }
  }

  override def getBitsWidth: Int = computePackMapping().map(_._1.high).max+1

  implicit class DataPositionEnrich[T <: Data](t: T) {
    /**
      * Place the data at the given range. Extra bits will be lost (unassigned or read) if the data does not fit with the range.
      * @param range Range to place the data
      * @param endianness Bit direction to align data within the range
      * @return Self
      */
    def pack(range: Range, endianness: Endianness = LITTLE): T = {
      t.addTag(new TagBitPackExact(range, endianness))
      t
    }

    /**
      * Packs data starting at the bit position
      * @param pos Starting bit position of the data
      * @return Self
      */
    def packAt(pos: Int): T = {
      t.pack(pos + t.getBitsWidth - 1 downto pos)
    }

    /**
      * Packs data ending at the bit position
      * @param pos Ending bit position of the data
      * @return Self
      */
    def packTo(pos: Int): T = {
      t.pack(pos downto pos - t.getBitsWidth + 1)
    }
  }
}
