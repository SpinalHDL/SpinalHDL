
import spinal.core._
import scala.reflect.runtime.universe._

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
