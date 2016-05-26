package spinal.tester.code

import spinal.core._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 21/05/2016.
 */


object PlayB7 {

  class TopLevel extends Component {

    val io = new Bundle {
      val x = in  UInt( 4 bits )
      val z = out UInt( 4 bits )
    }
    val tmp = U(0)
    io.z := tmp
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayB6 {

  class TopLevel extends Component {
    val notUsed = False

    val result = out(True)
  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayB5 {

  class TopLevel extends Component {
    val myClock = in Bool

    val myClockDomain = ClockDomain(myClock, True)

    val area = new ClockingArea(myClockDomain) {
      val result = out(RegNext(in(Bool)) init (True))
    }

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}


object PlayB1 {

  class TopLevel extends Component {
    val io = new Bundle() {
      val cond = in Bool
      val input = in UInt (4 bit)
      val output = out UInt(4 bits)
    }

    var carry = Bool(false)
    for (bit <- io.input.asBools) {
      when(io.cond) {
        carry \= carry & bit
      }
    }
    io.output := carry.asUInt

  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdlBuilder(new TopLevel)
      .elaborate()
  }
}

object PlayB2 {

  class TopLevel extends Component {

    val input = in UInt (4 bit)
    val output = out UInt(4 bits)
    output := input + input
    //    switch(io.input){
    //      is(0){
    //        io.output := 0
    //      }
    //      is(1){
    //        io.output := 1
    //      }
    //      is(2){
    //        io.output := 2
    //      }
    //      default{
    //        io.output := 3
    //      }
    //    }
  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdlBuilder(new TopLevel)
      .elaborate()
  }
}
object PlayB3 {

  class TopLevel extends Component {

    val input = in UInt (4 bit)
    val output = out UInt(4 bits)
    output := 1

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new TopLevel)
  }
}

object PlayB4 {

  class TopLevel extends Component {
    val address = in UInt(4 bits)
    val writeData = in Bits(8 bits)
    val chipSelect = in Bool
    val writeEnable = in Bool
    val readData = out Bits(8 bits)

    val mem = Mem(Bits(8 bits),16)
    readData := mem.writeOrReadSync(address,writeData,chipSelect,writeEnable)




    address := 0
  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdl(new TopLevel)
  }
}


object PlayAssert {

  class TopLevel extends Component {
    val a,b = in UInt(4 bits)
    val tmp = a + 2
    when(a.lsb) {
      when(a.msb) {
        assert(tmp === 8, "Yolo", NOTE)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdl(new TopLevel)
  }
}



object PlayAssert2 {

  class TopLevel extends Component {
    val valid = RegInit(False)
    val ready = in Bool

    when(ready){
      valid := False
    }
    // some logic

    assert(
      assertion = !(valid.fall && !ready),
      message   = "Valid drop when ready was low",
      severity  = ERROR
    )
  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdl(new TopLevel)
  }
}


object PlayPerf {

  def timeOf(that : => Unit,message : String) : Unit = {
    for(i <- 0 until 10){
      val start = System.nanoTime()
      that
      val end = System.nanoTime()
      println(message + " " + (end-start)*1e-9)
    }

  }

  def main(args: Array[String]): Unit = {
    var total = 0
    val arrayBuffer = ArrayBuffer.tabulate(1000*1000*10)(i => i)
    val array = Array.tabulate(1000*1000*10)(i => i)
    timeOf({
      var sum = 0
      for(e <- arrayBuffer){
        sum += e
      }
      total += sum
    },"For loop ArrayBuffer")

    timeOf({
      var sum = 0
      var idx = arrayBuffer.length
      while(idx != 0){
        idx -= 1
        sum += arrayBuffer(idx)
      }
      total += sum
    },"While ArrayBuffer")


    timeOf({
      var sum = 0
      for(e <- array){
        sum += e
      }
      total += sum
    },"For loop Array")

    timeOf({
      var sum = 0
      var idx = array.length
      while(idx != 0){
        idx -= 1
        sum += array(idx)
      }
      total += sum
    },"While Array")


    timeOf({
      var sum = 0
      def doIt(i : Int) = sum += i
      var idx = array.length
      while(idx != 0){
        idx -= 1
        sum += idx
      }
      total += sum
    },"while inc")

    timeOf({
      var sum = 0
      def doIt(i : Int) = sum += i
      var idx = array.length
      while(idx != 0){
        idx -= 1
        doIt(idx)
      }
      total += sum
    },"while doit")


    abstract class AbstractWorker{
      def doIt(dx : Int) : Unit
    }

    var sum = 0
    class WorkerA extends AbstractWorker{
      override def doIt(idx : Int) : Unit = sum += idx
    }
    class WorkerB extends AbstractWorker{
      override def doIt(idx : Int) : Unit = print(idx)
    }

    val arrayWorkerA = Array.tabulate(1000*1000*10)(i => new WorkerA)
    timeOf({
      sum = 0
      var idx = array.length
      while(idx != 0){
        idx -= 1
        arrayWorkerA(idx).doIt(idx)
      }
      total += sum
    },"while WorkerA")

    class NodeArrayBuffer(val inputs : ArrayBuffer[Int]){

    }

    abstract class NodeAbstract{
      def onEachInput(doThat : (Int,Int) => Unit) : Unit
    }

    class FuncCtion(a : Int,b : Int) extends NodeAbstract{
      override def onEachInput(doThat: (Int, Int) => Unit): Unit = {
        doThat(a,0)
        doThat(b,1)
      }
    }
    class OperRator(a : Int,b : Int) extends NodeAbstract{
      override def onEachInput(doThat: (Int, Int) => Unit): Unit = {
        doThat(a,0)
        doThat(b,1)
      }
    }

    val arrayNodeArrayBuffer = Array.tabulate(1000*1000)(i => new NodeArrayBuffer(ArrayBuffer(i*2,i*3)))
    val arrayNodeAbstract = Array.tabulate(1000*1000)(i => if(i % 2 == 0) new FuncCtion(i*2,i*3) else new OperRator(i*2,i*3))

    timeOf({
      sum = 0
      var idx = arrayNodeArrayBuffer.length
      while(idx != 0){
        idx -= 1
        arrayNodeArrayBuffer(idx).inputs.foreach(sum += _)
      }
      total += sum
    },"while NodeArrayBuffer foreach")

    timeOf({
      sum = 0
      var idx = arrayNodeArrayBuffer.length
      while(idx != 0){
        idx -= 1
        for(e <- arrayNodeArrayBuffer(idx).inputs)
          sum += e
      }
      total += sum
    },"while NodeArrayBuffer for")

    timeOf({
      sum = 0
      var idx = arrayNodeArrayBuffer.length
      while(idx != 0){
        idx -= 1
        val inputs = arrayNodeArrayBuffer(idx).inputs
        var idx2 = inputs.length
        while(idx2 != 0){
          idx2 -= 1
          sum += inputs(idx2)
        }
      }
      total += sum
    },"while NodeArrayBuffer while")


    timeOf({
      sum = 0
      var idx = arrayNodeAbstract.length
      while(idx != 0){
        idx -= 1
        arrayNodeAbstract(idx).onEachInput((e,i) => sum += e)
      }
      total += sum
    },"while NodeAbstract")

    class Toto{
      var i = 5
    }
    val toto = new Toto
    timeOf({
      sum = 0
      var idx = arrayNodeAbstract.length
      while(idx != 0){
        idx -= 1
        new Toto
      }
      total += sum
    },"while new")

    timeOf({
      sum = 0
      var idx = arrayNodeAbstract.length
      while(idx != 0){
        idx -= 1
        toto.getClass().newInstance()
      }
      total += sum
    },"while new")


    println(total)
  }
}

