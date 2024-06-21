package spinal.core

import spinal.lib._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

object wrapInterface {
  def apply[B <: Bundle](comp: => Component{val io: B}) = new Component {
    val dut = comp
    this.setDefinitionName(s"${dut.definitionName}_wrap")
    val io = cloneOf(dut.io)
    io.flattenForeach(x => x.removeTag(IsInterface))
    io.flatten.zip(dut.io.flatten)
      .foreach{case (a, b) => if(b.isInput) a.asInput() else a.asOutput()}
    dut.io <> io
  }
}

case class BundleInIf () extends Component {
  val io = new Bundle {
    val color = slave(Intf(8))
    val c = out UInt(8 bits)
  }

  io.color.ready := RegNext(!io.color.valid)
  val o = Reg(UInt(8 bits)) init(0)
  when(io.color.valid && io.color.ready) {
    o := io.color.payload.red + io.color.payload.blue + io.color.payload.green
  }
  io.c := o

}

case class Intf(w: Int) extends Interface with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val payload = ColorBundle(w)
  tieParameter(payload.red, addParameter("width", w))
  tieParameter(payload.blue, "width")
  tieParameter(payload.green, "width")

  override def asMaster(): Unit = {
    in(ready)
    out(valid, payload)
  }

  @modport
  def mst = asMaster()

  @modport
  def slv = asSlave()
}

case class ColorBundle(w: Int) extends Bundle {
  val red = UInt(w bits)
  val green = UInt(w bits)
  val blue = UInt(w bits)
}

class BundleInIfTest extends SpinalAnyFunSuite{
  import spinal.core.sim._
  import spinal.sim._
  def comp = wrapInterface(BundleInIf())
  //Config.spinal.generateSystemVerilog(comp)
  SimConfig.withConfig(SpinalConfig(mode = SystemVerilog, svInterface = true)).withFstWave.compile(comp).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    var modelState = BigInt(0)
    for (idx <- 0 to 99) {
      // Drive the dut inputs with random values
      dut.io.color.valid.randomize()
      val a = dut.io.color.payload.red.randomize()
      val b = dut.io.color.payload.blue.randomize()
      val c = dut.io.color.payload.green.randomize()

      // Wait a rising edge on the clock
      dut.clockDomain.waitRisingEdge()

      assert(modelState == dut.io.c.toBigInt)
      if(dut.io.color.valid.toBoolean && dut.io.color.ready.toBoolean)
        modelState = (a + b + c) & 0xff
    }
  }
}
