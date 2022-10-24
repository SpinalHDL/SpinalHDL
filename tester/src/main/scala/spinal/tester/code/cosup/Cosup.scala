package spinal.tester.code.cosup

import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class Cosup {
  val firstThing = Bool()
  firstThing.setAsReg()
  firstThing.setName("reg1")

  val secondThing = Bool()
  secondThing.setAsReg()
  secondThing.setName("reg2")

  secondThing assignFrom(!firstThing)



  val reg1, reg2 = Reg(Bool())
  reg1 := !reg2

  val things = ArrayBuffer[Bool]()
  for(i <- 0 to 2){
    val thing = Reg(Bool())
    things += thing
  }

  val logics = things.map(r => !r)
  val allSet = logics.andR


  fork{

  }




}

object VexPlay{

  class Stageable(ht : HardType[Data]) {

  }

  class Stage {
    val valid = Reg(Bool())
    val stuck = Bool()
    val discard = Bool()

    def insert(key : Stageable) : Data = ???
    def input(key : Stageable) : Data = ???
    def output(key : Stageable) : Data = ???
  }

  trait Plugin {
    def setup(p: MyCpu) : Unit
    def build(p: MyCpu) : Unit
  }

  class MyCpu(plugins : List[Plugin]) extends Component {
    val decode = new Stage
    val execute = new Stage
    val memory = new Stage
    val writeBack = new Stage

    var stages = List(decode, execute, memory, writeBack)
    for(p <- plugins) p.setup(this)
    for(p <- plugins) p.build(this)

    //...
  }

//  object PC extends Stageable(UInt(32 bits))
//  //..
//  decode.insert(INSTRUCTION) := X
//  //..
//  Y := execute.input(INSTRUCTION)


  import spinal.core.fiber._
  // Create two empty Handles
  val a, b = Handle[Int]

  // Create a Handle which will be loaded asynchronously by the given body result
  val calculator = Handle {
    a.get + b.get // .get will block until they are loaded
  }

  // Same as above
  val printer = Handle {
    println(s"a + b = ${calculator.get}") // .get is blocking until the calculator body is done
  }

  // Synchronously load a and b, this will unblock a.get and b.get
  a.load(3)
  b.load(4)
}

class Toplevel extends Component{
  val a,b = in(SInt(8 bits))
  val result = out(RegNext(a * b) init(0))
}

object Simulation extends App{
  SimConfig.withFstWave.compile(new Toplevel).doSim{ dut =>
    dut.clockDomain.forkStimulus(10)

    var phase = 0.0
    var phaseIncr = 0.01
    dut.clockDomain.onSamplings{
      def rangeFit(value : Double) = (value*127).toInt
      dut.a #= rangeFit(Math.sin(phase))
      dut.b #= rangeFit(Math.cos(phase))
      phase += phaseIncr
      phaseIncr *= 1.001
    }

    dut.clockDomain.waitSampling(2000)
  }
}
