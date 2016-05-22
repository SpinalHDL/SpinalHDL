package spinal.tester.code

import spinal.core._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 21/05/2016.
 */
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


object PlayPerf {

  def timeOf(that : => Unit,message : String) : Unit = {
    for(i <- 0 until 10){
      that
    }
    val start = System.nanoTime()
    that
    val end = System.nanoTime()
    println(message + " " + (end-start)*1e-9)
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


    println(total)
  }
}

