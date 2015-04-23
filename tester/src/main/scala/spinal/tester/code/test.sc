
import java.util.Calendar

import spinal.core._
import spinal.lib._
import scala.reflect.runtime.universe._


val stringList = Seq("a","b","c")
println(stringList.reduceRight((l,r) => r + l))
println(Calendar.getInstance().getTime())
//val bits = scala.collection.mutable.BitSet(BigInt(16))
////bits.aggregate(BigInt(16))
val arr = new Array[Byte](2)
arr(0) = 0xFF.toByte
arr(1) = 0x7F.toByte
val bi = BigInt.apply(arr)

val aggregator = new BitAggregator
aggregator.add(0xd,9)
aggregator.add(0xe,4)
aggregator.add(0xf,4)

println(aggregator.toBytes.map("%02X" format _).reverse.mkString(" ") + "  ")
class Foo(name: String, i: Int) { def this(name: String) = this(name, 0) }
val params = typeOf[Foo].declaration(nme.CONSTRUCTOR).asTerm.alternatives.collect {
  case m: MethodSymbol => m.paramss.map(_.map(_.name))
}
params
private  val codeSections = Seq(
  // (hw: hw_implemented) => hw.getIncludeCode, // Must remove duplicates files
  (hw: Int) => "asd",
  (hw: Int) => "asd2"
)
def a = 2
def xx(dummy : Int) = () => a
val f1 : (Int) => (()  => Int) = xx
val f2 : (Int) => (()  => Int) = xx
f1.hashCode()
f2.hashCode()
class XAS{
  object attributs{
    def a = 2
  }
  def ? = attributs
}
new XAS().?.a
U(2)
U"x1492"
Bool(true)
spinal.core.log2Up(0)
spinal.core.log2Up(1)
spinal.core.log2Up(2)
spinal.core.log2Up(3)
BigInt(1).bitLength
BigInt(0).bitLength
BigInt(-1).bitLength
BigInt(-2).bitLength
class BBB {
  def apply(blocks: (Int,() => Unit)*): Unit = {
    blocks.foreach(_._2)
  }
  def __(b : BBB) : BBB = this
}
val b = new BBB
b __ b __ b
//BBB(1 -> () => {
//  println("BBB block hallo1")
//},() => 2 -> {
//  println("BBB block hallo2")
//})
