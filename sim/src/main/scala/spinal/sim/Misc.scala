package spinal.sim

object SimError{
  def apply(message : String): Unit ={
    System.out.flush()
    Thread.sleep(20)
    System.err.println("\n\n" + message)
    throw new Exception()
  }
}

@deprecated("You can now use regular scala for loops as scala suspendable aren't use anymore.", "1.3.0")
object Suspendable{
  def repeat(times : Long)(body : => Unit): Unit ={
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