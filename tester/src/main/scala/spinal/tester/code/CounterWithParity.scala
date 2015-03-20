package spinal.tester.code

import spinal.core._

class CounterWithParity(size : Int) extends Component{
  val io = new Bundle{
    val increment = in Bool()
    val value = out UInt(size bit)
    val evenParity = out Bool()
  }

  val counter = RegInit(UInt(0 lit,size bit))
  when(io.increment){
    counter := counter + UInt(1 lit)
  }
  io.evenParity := counter.toBools.reduceLeft(_ ^ _)

  io.value := counter
}


object CounterWithParity {
  def main(args: Array[String]) {
    SpinalVhdl(new CounterWithParity(4))
  }
}