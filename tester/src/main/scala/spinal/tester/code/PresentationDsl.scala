package spinal.tester.code

import scala.collection.mutable.ArrayBuffer


/**
 * Created by PIC32F_USER on 13/11/2016.
 */
object IntroHdl{
  import spinal.core._

  class MyComponent extends Component {
    val io = new Bundle {
      val a           = in    Bool
      val b           = in    Bool
      val c            = in    Bool
      val result   = out Bool
    }
    val a_and_b = io.a & io.b
    val not_c = !io.c
    io.result := a_and_b | not_c
  }

  def main(args: Array[String]) {
    SpinalVhdl(new MyComponent)
  }
}

object PresentationDSL{



  object AST{
    import spinal.core._

    val a,b,c  = Bool
    val result = Bool
    result := (a || b) && c
  }

  object AST2{
    class Node{
      val inputs = ArrayBuffer[Node]()
    }
    class Bool extends Node{
      def &&(right : Bool) : Bool = ???
      def ||(right : Bool) : Bool = ???
      def unary_! : Bool = ???
    }
    class And extends Node
    class Or extends Node
    class Not extends Node

  }

  object AST3{
    import spinal.core._
    val bools = List.fill(3)(in Bool)

    var boolsAnd = bools.head
    for(bool <- bools.tail){
      boolsAnd = boolsAnd && bool
    }

    val output = out Bool()
    output := boolsAnd

  }


  object BuilderPattern{

  }

  object CurryingAndBlocks{
    import spinal.core._

    object when {
      def apply(cond: Bool)(block: => Unit): Unit = {
        //...
      }
    }
    val something, data, result = Bool
    result := False
    when(something){
      result := data
    }
  }

  object Otherwise{
    import spinal.core._

    class When(context : Bool){
      def otherwise(block : => Unit) : Unit =  {
        //...
      }
    }

    object when {
      def apply(cond: Bool)(block: => Unit): When = {
        //...
        new When(cond)
      }
    }

    when(???){
      //...
    } otherwise {
      //...
    }
  }

  object Context{
    object when {
      var context = List[Bool]()
      def apply(cond: Bool)(block: => Unit): Unit = {
        context = cond :: context
        block
        context = context.tail
      }
    }

    class Node
    class Bool extends Node{
      def := (that : Bool): Unit ={
        // use when.context
      }
    }
  }

  object StringInterpolation{
    implicit class LiteralBuilder(sc: StringContext) {
      def x(args: Any*): BigInt = BigInt(args(0).asInstanceOf[String].replace("_",""),16)
    }

    val literal = x"CAFE_F00D"
  }

  object PimpMyLibraryPattern{
    implicit class StringPimper(pimped : String){
      def countUpperCases() : Int = pimped.count(_.isUpper)
    }

    "ChewBacca".countUpperCases()   //2


  }

  object Prefix{
    import spinal.core.BitCount
    import spinal.core._


    class BaseType{
      def asInput() : this.type = ???
    }

    class Bool extends BaseType
    class UInt(width : Int) extends BaseType

    object in{
      def Bool() = new Bool().asInput()
      def UInt(width : BitCount) = new UInt(width.value).asInput()
    }

    val a = in Bool()
    val b = in UInt(4 bits)


    val c = Bool().asInput()
  }


  object Postfix{
    case class BitCount(value : Int)
    implicit class IntPimper(pimped : Int){
      def bits = BitCount(pimped)
    }

    class UInt(width : Int)
    object UInt{
      def apply(width : BitCount) = new UInt(width.value)
    }
    
    val a,b = UInt(4 bits)
  }

  object VarOperator{
    case class MegaInt(value : Int){
      def +(that : MegaInt) = MegaInt(this.value + that.value)
    }

    var a = MegaInt(666)
    a     += MegaInt(22) //a = MegaInt(666 + 22)
  }

  object DataTypesWrong {
    class Bundle {
      def :=(that: this.type) = ??? //Can't do that
    }
  }

  object DataTypes{
    import spinal.core._

    implicit class BundlePimper[T <: Bundle](pimped : T){
      def :=(that : T) = ???
    }

    class RGB extends Bundle{
      val r,g,b = UInt(8 bits)
    }

    val a,b = new RGB
    a := b  //It is strongly typed and only accept RGB := RGB
  }



  object Color{
    import spinal.core._
    case class Color(channelWidth: Int) extends Bundle {
      val r,g,b = UInt(channelWidth bits)

      def +(that: Color): Color = {
        val result = Color(channelWidth)

        result.r := this.r + that.r
        result.g := this.g + that.g
        result.b := this.b + that.b

        return result
      }
    }

    val a,b = Color(8)
    val c = a + b
    val d = Color(8)
    d := c

    def foo(i : Int) = 3
    foo{3}
  }




  object Pinsec{
    
  }
}
