package spinal.tester.code

import spinal.core._

//
//class AdderCell extends Component {
//  val io = new Bundle {
//    val a, b, cin = in Bool
//    val sum, cout = out Bool
//  }
//  io.sum := io.a ^ io.b ^ io.cin
//  io.cout := (io.a & io.b) | (io.a & io.cin) | (io.b & io.cin)
//}
//
//class Adder(width: Int) extends Component {
//  val io = new Bundle {
//    val a, b = in UInt (width bit)
//    val cin = in Bool
//    val sum = out UInt (width bit)
//    val cout = out Bool
//  }
//  val cells = Array.fill(width)(new AdderCell)
//  for(i <- 0 until width){
//    cells(i).io.a := io.a(i)
//    cells(i).io.b := io.b(i)
//    io.sum(i) := cells(i).io.sum
//  }
//
//  cells(0).io.cin := io.cin
//  for(i <- 1 until width){
//    cells(i).io.cin := cells(i-1).io.cout
//  }
//  io.cout := cells(width-1).io.cout
//}
//
//object Adder{
//  def main(args: Array[String]): Unit = {
//    SpinalVhdl(new Adder(4))
//  }
//}
//
//object AssignmentTrash{
//  class TopLevel extends Component{
//
//    var x = UInt(4 bit)
//    val y,z = out UInt(4 bit)
//    x := 0
//    y := x      //y read x with the value 0
//    x \= x + 1
//    z := x      //z read x with the value 1
//  }
//  def main(args: Array[String]): Unit = {
//    SpinalVhdl(new TopLevel)
//  }
//}
//
//
//object CatTrash{
//  class TopLevel extends Component{
//
//    val output = out(Cat(Seq(True,False,False)))
//  }
//  def main(args: Array[String]): Unit = {
//    SpinalVhdl(new TopLevel)
//  }
//}




object toto {
  import spinal.core._
  class CarryAdder(size: Int) extends Component {
    val io = new Bundle {
      val a = in UInt (size bit)
      val b = in UInt (size bit)
      val result = out UInt (size bit) //result = a + b
    }

    var c = False //Carry, like a VHDL variable
    for (i <- 0 until size) {
      //Create some intermediate value in the loop scope.
      val a = io.a(i)
      val b = io.b(i)

      //The carry adder's asynchronous logic
      io.result(i) := a ^ b ^ c
      c \= (a & b) | (a & c) | (b & c); //variable assignment
    }
  }


  //object Main {
    def main(args: Array[String]) {
      SpinalVhdl(new CarryAdder(4))
    }
 // }
}


// spinal.core contain all basics (Bool, UInt, Bundle, Reg, Component, ..)
import spinal.core._

//A simple component definition
class MyTopLevelX extends Component {
  //Define some input/output. Bundle like a VHDL record or a verilog struct.
  val io = new Bundle {
    val a = in Bool
    val b = in Bool
    val c = out Bool
  }

  //Define some asynchronous logic
  io.c := io.a & io.b
}

//This is the main of the project. It create a instance of MyTopLevel and
//call the SpinalHDL library to flush it into a VHDL file.
object MyMain {
  def main(args: Array[String]) {
    SpinalVhdl(new MyTopLevelX)
  }
}



class Ram_1w_1r(_wordWidth: Int, _wordCount: Int) extends BlackBox {
  val generic = new Generic {
    val wordCount = _wordCount
    val wordWidth = _wordWidth
  }

  val io = new Bundle {
    val clk = in Bool

    val wr = new Bundle {
      val en = in Bool
      val addr = in UInt (log2Up(_wordCount) bit)
      val data = in Bits (_wordWidth bit)
    }
    val rd = new Bundle {
      val en = in Bool
      val addr = in UInt (log2Up(_wordCount) bit)
      val data = out Bits (_wordWidth bit)
    }
  }

  mapClockDomain(clock=io.clk)
}

import spinal.lib._
object test{
  val a = in UInt(3 bit)
  val str = Stream(Bool)
  str <-/< str
a.pull()
}

object Trash{

  val asynchronousSignal = UInt(8 bit)

  val buffer0 = Reg(UInt(8 bit)) addTag(crossClockDomain)
  val buffer1 = Reg(UInt(8 bit))

  buffer0 := asynchronousSignal
  buffer1 := buffer0


  case class RGB(channelWidth : Int) extends Bundle{
    val red   = UInt(channelWidth bit)
    val green = UInt(channelWidth bit)
    val blue  = UInt(channelWidth bit)

    def isBlack : Bool = red === 0 && green === 0 && blue === 0
    def isWhite : Bool = {
      val max = U((channelWidth-1 downto 0) -> True)
      return red === max && green === max && blue === max
    }
  }

  case class VGA(channelWidth : Int) extends Bundle{
    val hsync = Bool
    val vsync = Bool
    val color = RGB(channelWidth)
  }

  val vgaIn  = VGA(8)         //Create a RGB instance
  val vgaOut = VGA(8)
  vgaOut := vgaIn            //Assign the whole bundle
  vgaOut.color.green := 0    //Fix the green to zero

  val myVecOfSInt = Vec(SInt(8 bit),2)
  myVecOfSInt(0) := 2
  myVecOfSInt(1) := myVecOfSInt(0) + 3

  val myVecOfMixedUInt = Vec(UInt(3 bit), UInt(5 bit), UInt(8 bit))

  val x,y,z = UInt(8 bit)
  val myVecOf_xyz_ref = Vec(x,y,z)
  for(element <- myVecOf_xyz_ref){
    element := 0   //Assign x,y,z with the value 0
  }
  myVecOf_xyz_ref(1) := 3    //Assign y with the value 3

  val myBool = Bool()
  myBool := False         // := is the assignment operator
  myBool := Bool(false)   // Use a Scala Boolean to create a literal

  val myUInt = UInt(8 bit)
  myUInt := U(2,8 bit)
  myUInt := U(2)
  myUInt := U"0000_0101"  // Base per default is binary => 5
  myUInt := U"h1A"        // Base could be x (base 16)
  //               h (base 16)
  //               d (base 10)
  //               o (base 8)
  //               b (base 2)
  myUInt := U"8h1A"
  myUInt := 2             // You can use scala Int as literal value
}

object TrashTopLevel {
  class TopLevel extends Component {
    val x,y,z = out(UInt(8 bit))
    val myVecOf_xyz_ref = Vec(x,y,z)
    for(element <- myVecOf_xyz_ref){
      element := 0   //Assign x,y,z with the value 0
    }
    myVecOf_xyz_ref(1) := 3    //Assign y with the value 3
    myVecOf_xyz_ref(in(UInt(2 bit))) := 4    //Assign y with the value 3
  }


  //object Main {
  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel())
  }
  // }


  case class RGB(channelWidth : Int) extends Bundle{
    val red   = UInt(channelWidth bit)
    val green = UInt(channelWidth bit)
    val blue  = UInt(channelWidth bit)

    def isBlack : Bool = red === 0 && green === 0 && blue === 0
  }

  val source = Stream(RGB(8))
  val sink   = Stream(RGB(8))
  sink <-< source.throwWhen(source.isBlack)

}
