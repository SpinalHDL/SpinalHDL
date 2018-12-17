package spinal.sim

import scala.util.continuations._
object SimError{
  def apply(message : String): Unit ={
    System.out.flush()
    Thread.sleep(20)
    System.err.println("\n\n" + message)
    throw new Exception()
  }
}


object Suspendable{
  def repeat(times : Long)(body : => Unit@suspendable): Unit@suspendable ={
    var idx = 0l
    while(idx != times){
      idx += 1
      body
    }
  }
}

object WaveFormat{
  object VCD extends WaveFormat("vcd")
  object FST extends WaveFormat("fst")
  object DEFAULT extends WaveFormat
  object NONE extends WaveFormat
}

class WaveFormat(val ext : String = "???")