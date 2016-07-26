package spinal.tester.code

import spinal.core._

class CounterWithParity(size : Int) extends Component{
  val io = new Bundle{
    val increment = in Bool
    val value = out UInt(size bit)
    val evenParity = out Bool
  }

  val counter = RegInit(U(0,size bit))
  when(io.increment){
    counter := counter + U(1)
  }
  io.evenParity := counter.asBools.reduceLeft(_ ^ _)

  io.value := counter
}


object CounterWithParity {
  def main(args: Array[String]) {
    SpinalVhdl(new CounterWithParity(4))
  }
}