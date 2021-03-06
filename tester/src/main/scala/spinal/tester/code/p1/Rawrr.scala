package spinal.tester.code.p1

import spinal.core._
import spinal.core.fiber._

object MyScalaProgramme{
  def main(args: Array[String]) {
    println("Hello world")

    var message = "Hello world";
    message += " !"
    println(message)
  }
}

class MyToplevel() extends Component {
    //...
}

object MyScalaProgramme1{
  def main(args: Array[String]) {
    SpinalVerilog(new MyToplevel())
  }
}

class MyToplevel2() extends Component {
  val a, b = in Bool()
  val result = out Bool()
  result := a && b
}
class MyToplevel3() extends Component {
  val in1, in2 = new Bool()
  in1.setName("a")
  in2.setName("b")
  in1.asInput()
  in2.asInput()
  val out1 = new Bool()
  out1.setName("a")
  out1.asOutput()
  out1.assignFrom(in1 && in2)
}

class MyToplevel4() extends Component {
  val a, b = in Bool()
  val result = out Bool()
  result := a && b
}


object MyScalaProgramme2{
  def main(args: Array[String]) {
    val message = "Hello world";
    if(message.contains("Hello")){
      println("all good")
    } else {
      println("Order 66")
    }
  }
}

class MyToplevel5(doOr : Boolean) extends Component {
  val a, b = in Bool()
  val result = out Bool()
  if(doOr){
    result := a || b
  } else {
    result := a && b
  }
}

object GenerateToplevel{
  def main(args: Array[String]) {
    SpinalVerilog(new MyToplevel5(doOr = true))
  }
}

class MyToplevel6() extends Component {
  val a,b,c,d = in UInt(8 bits)
  val result = out UInt(8 bits)

  val inputs = List(a,b,c,d)
  var accumulate = U(0, 8 bits)
  for(idx <- 0 to inputs.size-1){
    accumulate = accumulate + inputs(idx)
    accumulate.setName(s"accumulate_$idx")
  }
  result := accumulate
}

object GenerateToplevel6{
  def main(args: Array[String]) {
    SpinalVerilog(new MyToplevel6())
  }
}


class MyToplevel7() extends Component {
  val a,b,c = in Bool()
  val result = out Bool()

  def bufferThenOr(that : Bool) = {
    val reg = RegNext(that)
    when(reg){
      result := True
    }
  }

  result := False
  bufferThenOr(a)
  bufferThenOr(b)
  bufferThenOr(c)
}



object GenerateToplevel7{
  def main(args: Array[String]) {
    SpinalVerilog(new MyToplevel7())
  }
}



object MyScalaProgramme3{
  def main(args: Array[String]) {
    val message = "Hello world";
    if(message.contains("Hello")){
      println("all good")
    } else {
      println("Order 66")
    }
  }
}


class MyToplevel8() extends Component {
  val a = Bool() //Automatic a.setName("a")
  val logic1 = new Area{
    val tmp = Bool() // Automaticaly tmp.setName("logic1_tmp")
  }
  def func1() = {
    val tmp = Bool() //Unamed signal
  }
  def func2() = new Area {
    val tmp = Bool() //Automatic a.setCompositeName(this, "tmp")
  }
  val logic2 = func2() // Automaticaly name logic2.tmp signal as "logic2_tmp"
}



object GenerateToplevel8{
  def main(args: Array[String]) {
    SpinalVerilog(new MyToplevel8())
  }
}


object DemoSpinalSim extends App{
  class Dut extends Component{
    val a,b = in UInt(8 bits)
    val result = out(a + b)
  }

  import spinal.core.sim._

  SimConfig.withFstWave.compile(new Dut).doSimUntilVoid{dut =>
    fork{
      dut.a #= 0; sleep(10)
      dut.a #= 1; sleep(10)
      dut.a #= 2; sleep(10)
      dut.a #= 3; sleep(10)
    }
    fork{
      for(i <- 0 until 10){
        dut.b #= i
        sleep(5)
      }
    }
  }


  def func(x : Int, y : Int) : Int = {
    val result = x + y
    return result
  }

  def func2(x : Int, y : Int) = {  //Return type can by inferred
    val result = x + y
    result  //Last statement of the block is used as return value implicitly
  }

  def func3(x : Int, y : Int) = {
    x + y
  }

  def func4(x : Int, y : Int) = x + y
}

object DemoGenerator {
  import spinal.lib.generator._

  class Root() extends Generator{
    //Define some Handle which will be later loaded with real values
    val a,b = Handle[Int]

    //Print a + b
    val calculator = new Generator{
      //Specify that this generator need a and b before executing his tasks
      dependencies += a
      dependencies += b

      //Create a new task that will run when all the dependencies are loaded
      add task{
        val sum = a.get + b.get
        println(s"a + b = $sum") //Will print a + b = 7
      }
    }

    //load a and b with values, which will then unlock the calculator generator
    a.load(3)
    b.load(4)
  }
}
