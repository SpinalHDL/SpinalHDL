package spinal

import spinal.core._
import spinal.lib.tools.binarySystem
import spinal.core.fiber.{Engine, Handle}
import spinal.lib.generator.Export

import scala.collection.Seq
import scala.collection.Iterable
import scala.collection.generic.Growable

package object lib  {
  //  def Stream[T <: Data](that : T) : Stream[T] = new Stream[T](that)
  //  def mm [T <: Data with IMasterSlave](that : T) = that.asMaster()
  type Event = Stream[NoData]


  def Event = new Stream(NoData)

  def NoData = new NoData

  def export[T](named : Handle[T], value :  => Any) = {
    Engine.get.onCompletion += {() => Component.current.addTag(new Export(named.getName, value)) }
  }

  def export[T](h : Handle[T]) = {
    Engine.get.onCompletion += {() => Component.current.addTag(new Export(h.getName, h.get)) }
    h
  }

  def export[T <: SpinalTag](h : T) = {
    Engine.get.onCompletion += {() => Component.current.addTag(h) }
    h
  }


  //implicit def easyStream[T <: Bundle](that: Stream[T]) = that.data
  implicit def traversableOncePimped[T <: Data](that: Seq[T]) = new TraversableOncePimped[T](that)
  implicit def traversableOnceBoolPimped(that: Seq[Bool]) = new TraversableOnceBoolPimped(that)
  implicit def traversableOnceAnyPimped[T <: Any](that: Seq[T]) = new TraversableOnceAnyPimped(that)
  implicit def growableAnyPimped[T <: Any](that: Growable[T]) = new GrowableAnyPimped(that)

  implicit def clockDomainPimped(cd: ClockDomain) = new ClockDomainPimped(cd)



//  implicit def seqPimped[T <: Data](that: scala.IndexedSeq[T]) = new TraversableOncePimped[T](that)

  implicit def flowFragmentPimped[T <: Data](that: Flow[Fragment[T]]) = new FlowFragmentPimped[T](that)
  implicit def streamFragmentPimped[T <: Data](that: Stream[Fragment[T]]) = new StreamFragmentPimped[T](that)
  implicit def streamBitsPimped[T <: Data](that: Stream[Bits]) = new StreamBitsPimped(that)
  implicit def flowBitsPimped[T <: Data](that: Flow[Bits]) = new FlowBitsPimped(that)
  implicit def dataCarrierFragmentPimped[T <: Data](that: DataCarrier[Fragment[T]]) = new DataCarrierFragmentPimped[T](that)
  implicit def dataCarrierFragmentBitsPimped(that: DataCarrier[Fragment[Bits]]) = new DataCarrierFragmentBitsPimped(that)
  implicit def streamFragmentBitsPimped(that: Stream[Fragment[Bits]]) = new StreamFragmentBitsPimped(that)
  implicit def stringPimped(that: String) = new StringPimped(that)
  implicit def memPimped[T <: Data](mem: Mem[T]) = new MemPimped(mem)
  implicit def boolPimped(that: Bool) = new BoolPimped(that)

  implicit class UIntPimper(that : UInt){
    def toOneHot : Bits = B"1" << that
  }

  implicit def easyFragment[T <: Data](that: Fragment[T]) = that.fragment
  
  def StreamArbiterFactory = new StreamArbiterFactory()
  type ScalaStream[T] = collection.immutable.Stream[T]
  def ScalaStream = collection.immutable.Stream

  /**
    * binarySystem
    */
  private val hex: String => BigInt = binarySystem.StringToLiteral.hex
  private val dec: String => BigInt = binarySystem.StringToLiteral.dec
  private val oct: String => BigInt = binarySystem.StringToLiteral.oct
  private val bin: String => BigInt = binarySystem.StringToLiteral.bin

  implicit class BinaryBuilder(private val sc: StringContext) {
    def x(args: Any*): Int = hex(sc.parts.head).toInt
    def o(args: Any*): Int = oct(sc.parts.head).toInt
    def b(args: Any*): Int = bin(sc.parts.head).toInt
  }

  implicit class BinaryBuilder2(val s: String) {
    def asHex: BigInt = hex(s)
    def asDec: BigInt = dec(s)
    def asOct: BigInt = oct(s)
    def asBin: BigInt = bin(s)
    def hexToBinInts: List[Int] = binarySystem.LiteralToBinInts.BigIntToBinInts(BigInt(s, 16))
    def hexToBinIntsAlign: List[Int] = binarySystem.LiteralToBinInts.BigIntToBinInts(BigInt(s, 16), 4 * s.size)
  }

  trait LiteralRicher {
    val bigInt: BigInt
    val defaultAlignBit: Int = 0

    def hexString(): String = binarySystem.LiteralToString.HexString(bigInt, defaultAlignBit)
    def octString(): String = binarySystem.LiteralToString.OctString(bigInt, defaultAlignBit)
    def binString(): String = binarySystem.LiteralToString.BinString(bigInt, defaultAlignBit)

    def hexString(bitSize: Int): String = binarySystem.LiteralToString.HexString(bigInt, bitSize)
    def octString(bitSize: Int): String = binarySystem.LiteralToString.OctString(bigInt, bitSize)
    def binString(bitSize: Int): String = binarySystem.LiteralToString.BinString(bigInt, bitSize)

    def toBinInts(): List[Int] = binarySystem.LiteralToBinInts.BigIntToBinInts(bigInt, defaultAlignBit)
    def toDecInts(): List[Int] = binarySystem.LiteralToBinInts.BigIntToDecInts(bigInt, defaultAlignBit)
    def toOctInts(): List[Int] = binarySystem.LiteralToBinInts.BigIntToOctInts(bigInt, defaultAlignBit)

    def toBinInts(num: Int): List[Int] = binarySystem.LiteralToBinInts.BigIntToBinInts(bigInt, num)
    def toDecInts(num: Int): List[Int] = binarySystem.LiteralToBinInts.BigIntToDecInts(bigInt, num)
    def toOctInts(num: Int): List[Int] = binarySystem.LiteralToBinInts.BigIntToOctInts(bigInt, num)
  }

  implicit class BigIntRicher(value: BigInt) extends LiteralRicher {
    val bigInt = value
  }

  implicit class LongRicher(value: Long) extends LiteralRicher  {
    val bigInt = BigInt(value)
  }

  implicit class IntRicher(value: Int) extends LiteralRicher {
    val bigInt = BigInt(value)
  }

  implicit class ByteRicher(value: Byte) extends LiteralRicher {
    val bigInt = BigInt(value)

    override val defaultAlignBit: Int = 8
  }

  implicit class BinIntsRicher(li: List[Int]){
    def binIntsToOctAlignHigh: String  = binarySystem.BinIntsToLiteral.binIntsToOctString(li, true)
    def binIntsToHexAlignHigh: String  = binarySystem.BinIntsToLiteral.binIntsToHexString(li, true)
    def binIntsToOct: String    = binarySystem.BinIntsToLiteral.binIntsToOctString(li)
    def binIntsToHex: String    = binarySystem.BinIntsToLiteral.binIntsToHexString(li)
    def binIntsToBigInt: BigInt = binarySystem.BinIntsToLiteral.binIntsToBigInt(li)
    def binIntsToInt: Int       = binIntsToBigInt.toInt
    def binIntsToLong: Long     = binIntsToBigInt.toLong
  }
}
