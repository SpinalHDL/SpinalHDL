package spinal.lib

import spinal.core._

import scala.collection.mutable.ArrayBuffer

/** Similar to Bundle but with bit packing capabilities.
  * Use pack implicit functions to assign fields to bit locations
  * - pack(Range, [Endianness]) - Packs the data into Range aligning to bit Endianness if too wide
  * - packFrom(Position) - Packs the data starting (LSB) at Position. Uses full data length
  * - packTo(Position) - Packs the data ending (MSB) at Position. Uses full data length
  *
  * Providing no location tag will place the next data value immediately after the last.
  *
  * @example {{{
  *     val regWord = new PackedBundle {
  *       val init = Bool().packFrom(0) // Bit 0
  *       val stop = Bool() // Bit 1
  *       val result = Bits(16 bit).packTo(31) // Bits 16 to 31
  *     }
  * }}}
  */
class PackedBundle extends Bundle {

  class TagBitPackExact(val range: Range) extends SpinalTag

  /** Builds and caches the range mappings for PackedBundle's elements.
    * Tracks the width required for all mappings.
    * Does not check for overlap of elements.
    */
  private class MappingBuilder {
    var nextPos = 0
    var highBit = 0
    val mapping = ArrayBuffer[(Range, Data)]()

    def addData(d: Data): Unit = {
      val r = d.getTag(classOf[TagBitPackExact]) match {
        case t: Some[TagBitPackExact] =>
          val origRange = t.get.range

          // Check if the tagged range is too large for the data
          if (origRange.size <= d.getBitsWidth) {
            origRange
          } else {
            // Need to truncate the tagged range to the size of the actual data
            val newSize = origRange.size.min(d.getBitsWidth)

            // Retain the range directionality
            if (origRange.step > 0) {
              (origRange.max - newSize - 1) to origRange.max
            } else {
              origRange.max downto (origRange.max - newSize - 1)
            }
          }

        case None =>
          // Assume the full range of the data with the MSB as the highest bit
          (nextPos + d.getBitsWidth - 1) downto (nextPos)
      }
      nextPos = r.high + 1

      // Update the bit width
      highBit = highBit.max(r.high)

      mapping.append(r -> d)
    }

    def width = highBit + 1
  }

  private val mapBuilder = new MappingBuilder()

  /** Gets the mappings of Range to Data for this PackedBundle
    * @return Seq of (Range,Data) for all elements
    */
  def mappings = mapBuilder.mapping

  def packed: Bits = {
    val maxWidth = mappings.map(_._1.high).max + 1
    val packed = B(0, maxWidth bit)
    for ((range, data) <- mappings) {
      if (range.step > 0) {
        // "Little endian" -- ascending range
        val subBits = data match {
          case subPacked: PackedBundle => subPacked.packed
          case _                       => data.asBits
        }
        packed(range) := subBits.takeLow(range.size.min(data.getBitsWidth)).resize(range.size)
      } else {
        // "Big endian" -- descending range
        val subBits = data match {
          case subPacked: PackedBundle => subPacked.packed
          case _                       => data.asBits
        }
        packed(range) := subBits.takeHigh(range.size.min(data.getBitsWidth)).resizeLeft(range.size)
      }
    }
    packed
  }

  def unpack(bits: Bits): Unit = unpack(bits, bits.getBitsWidth, 0)

  def unpack(bits: Bits, hi: Int, lo: Int): Unit = {
    for ((elRange, el) <- mappings) {
      // Check if the assignment range falls within the current data's range
      // This happens when the data range's high or low falls within the assignment's hi and lo
      // ...or whenever lo isn't past the data range's high and hi isn't below the data range's low
      if ((elRange.low >= lo && elRange.low < hi) || (elRange.high >= lo && elRange.high < hi)) {
        if (elRange.step > 0) {
          // "Little endian" -- ascending range
          val subBits = bits(elRange).resize(el.getBitsWidth)
          el match {
            case subPacked: PackedBundle => subPacked.unpack(subBits)
            case _                       => el.assignFromBits(subBits)
          }
        } else {
          // "Big endian" -- descending range
          val subBits = bits(elRange).resizeLeft(el.getBitsWidth)
          el match {
            case subPacked: PackedBundle => subPacked.unpack(subBits)
            case _                       => el.assignFromBits(subBits)
          }
        }
      }
    }
  }

  def getPackedWidth: Int = mappings.map(_._1.high).max + 1

  implicit class DataPositionEnrich[T <: Data](t: T) {

    /** Place the data at the given range. Extra bits will be lost (unassigned or read) if the data does not fit with the range.
      * @param range Range to place the data
      * @return Self
      */
    def pack(range: Range): T = {
      t.addTag(new TagBitPackExact(range))
      t
    }

    /** Place the data at the given range. Extra bits will be lost (unassigned or read) if the data does not fit with the range.
      *
      * @param range      Range to place the data
      * @param endianness Bit direction to align data within the range
      * @return Self
      */
    def pack(range: Range, endianness: Endianness = LITTLE): T = {
      endianness match {
        case LITTLE => pack(range.low to range.high)
        case BIG    => pack(range.high downto range.low)
      }
    }

    /** Packs data starting (LSB) at the bit position
      * @param pos Starting bit position of the data
      * @return Self
      */
    def packFrom(pos: Int): T = {
      t.pack(pos + t.getBitsWidth - 1 downto pos)
    }

    /** Packs data ending (MSB) at the bit position
      * @param pos Ending bit position of the data
      * @return Self
      */
    def packTo(pos: Int): T = {
      t.pack(pos downto pos - t.getBitsWidth + 1)
    }
  }

  override def valCallbackRec(ref: Any, name: String): Unit = {
    super.valCallbackRec(ref, name)

    // Process the data
    ref match {
      case d: Data =>
        mapBuilder.addData(d)
      case _ =>
    }
  }
}

/** An enhanced form of PackedBundle with Word-centric packing.
  * Offers all the same implicit packing assignment functions, but applies packing to an assigned word.
  * - inWord(WordIndex) - Indicates which word to pack into. Must be used after a pack assigment. If no pack range was given then the entire data length will be assumed. Ranges that exceed the word will wrap into subsequent words.
  *
  * Like PackedBundle, providing no pack or word assignments will place data immediately after the last.
  *
  * @example {{{
  *     val wordPacked = PackedWordBundle(8 bits) {
  *       val aNumber = UInt(8 bits).word(0) // Bits 7 downto 0
  *       val bNumber = UInt(8 bits).pack(0 to 7).word(1) // Bits 8 to 15
  *       val large   = Bits(18 bits).word(2) // Bits 33 downto 16
  *       val flag    = Bool() // Bit 34
  * }}}
  * @param wordWidth Width of a word, as BitCount
  */
class PackedWordBundle(wordWidth: BitCount) extends PackedBundle {

  implicit class WordEnrich[T <: Data](t: T) {

    def inWord(index: Int) = {
      val bitPackExact = t.getTag(classOf[TagBitPackExact])

      if (bitPackExact.isDefined) {
        // Update the BitPackExact if it exists on the Data

        // Remove the old tag as it's bit range was in reference to a word's bits
        t.removeTag(bitPackExact.get)

        val oldRange = bitPackExact.get.range
        val basePos = index * wordWidth.value

        // Build the new range from the old and the word's base position
        val newRange = {
          if (oldRange.step > 0) {
            oldRange.low + basePos to oldRange.high + basePos
          } else {
            oldRange.high + basePos downto oldRange.low + basePos
          }
        }

        t.pack(newRange)
      } else {
        // Add the full range of the Data starting at the given word index
        val basePos = index * wordWidth.value
        t.packFrom(basePos)
      }
    }
  }
}
