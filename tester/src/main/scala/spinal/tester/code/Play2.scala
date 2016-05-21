package spinal.tester.code

import spinal.core._

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
    val io = new Bundle() {
      val input = in UInt (4 bit)
      val output = out UInt(4 bits)
    }

    switch(io.input){
      is(0){
        io.output := 0
      }
      is(1){
        io.output := 1
      }
      is(2){
        io.output := 2
      }
      default{
        io.output := 3
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //SpinalVhdl(new TopLevel)
    SpinalVhdlBuilder(new TopLevel)
      .elaborate()
  }
}

