package spinal.core

import spinal.lib._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

case class Top() extends Component {
  val io = new Bundle {
    val color = slave(BundleIntf(8))
    val c = master(Flow(UInt(8 bits)))
  }

  val color = Pipe(io.color, 4)
  val u_b = BundleInBundleInIf()
  u_b.io.color <> color
  io.c <> u_b.io.c
}

case class BundleInBundleInIf () extends Component {
  val io = new Bundle {
    val color = slave(BundleIntf(8))
    val c = master(Flow(UInt(8 bits)))
  }

  val cnt = Reg(UInt(3 bits)) init(0)
  when(io.color.valid) {
    cnt := cnt + 1
  }
  io.color.ready := cnt === 0
  //io.color.ready := RegNext(!io.color.valid)
  val o = Reg(UInt(8 bits)) init(0)
  when(io.color.valid && io.color.ready) {
    o := io.color.payload.red + io.color.payload.blue + io.color.payload.green + io.color.payload.bd.a + io.color.payload.bd.b + io.color.payload.num.reduce(_+_)
  }
  io.c.payload := o
  io.c.valid := RegNext(io.color.valid && io.color.ready, False)

}

object Pipe {
  def apply(x: BundleIntf): BundleIntf = {
    val u_pipe = new Pipe
    u_pipe.io.s <> x
    u_pipe.io.m
  }
  def apply(x: BundleIntf, level: Int): BundleIntf = {
    level match {
      case 1 => apply(x)
      case l => apply(apply(x), l - 1)
    }
  }
}

class Pipe extends Component {
  val io = new Bundle {
    val s = slave(BundleIntf(8))
    val m = master(BundleIntf(8))
  }

  val rValidN = RegInit(True) clearWhen(io.s.valid) setWhen(io.m.ready)
  val rData = RegNextWhen(io.s.payload, io.s.ready)

  io.s.ready := rValidN

  io.m.valid := io.s.valid || !rValidN
  io.m.payload := Mux(rValidN, io.s.payload, rData)

}

case class BundleIntf(w: Int) extends Interface with IMasterSlave {
  val valid = Bool()
  val ready = Bool()
  val payload = ColorBundleVec(w)
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

case class ColorBundleVec(w: Int) extends Bundle {
  val red = UInt(w bits)
  val green = UInt(w bits)
  val blue = UInt(w bits)
  val num = Vec(UInt(w bits), 8)
  val bd = Abundle(w)
}

case class Abundle(w: Int) extends Bundle {
  val a = UInt(w bits)
  val b = UInt(w bits)
}

class BundleInIfTest2 extends SpinalAnyFunSuite{
  import spinal.core.sim._
  import spinal.sim._
  def comp = wrapInterface(Top())
  //Config.spinal.generateSystemVerilog(comp)
  SimConfig.withConfig(SpinalConfig(mode = SystemVerilog, svInterface = true)).withFstWave.compile(comp).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    var modelState: List[BigInt] = List()
    var toCompare = BigInt(0)
    for (idx <- 0 to 99) {
      // Drive the dut inputs with random values
      dut.io.color.valid.randomize()
      val a = dut.io.color.payload.red.randomize()
      val b = dut.io.color.payload.blue.randomize()
      val c = dut.io.color.payload.green.randomize()
      val bda = dut.io.color.payload.bd.a.randomize()
      val bdb = dut.io.color.payload.bd.b.randomize()
      val num = dut.io.color.payload.num.map(_.randomize())

      // Wait a rising edge on the clock
      dut.clockDomain.waitRisingEdge()

      if(dut.io.color.valid.toBoolean && dut.io.color.ready.toBoolean)
        modelState = modelState.+:((a + b + c + bda + bdb + num.reduce(_+_)) & 0xff)
      if(dut.io.c.valid.toBoolean) {
        toCompare = modelState.last
        modelState = modelState.dropRight(1)
      }
      assert(toCompare == dut.io.c.payload.toBigInt)
    }
  }
}
object MyDirEnum extends SpinalEnum {
  val a, b = newElement()
}

class IMasterSlaveDirDeclareTest extends SpinalAnyFunSuite {

  test("IMasterSlaveDirDeclare directions") {
    case class TestInterface() extends Bundle with IMasterSlaveDirDeclare {
      // "spaceful" syntax
      val spacefulOut = out port Bool()
      val spacefulIn = in port Bool()
      val spacefulMaster = master port Stream(Bool())
      val spacefulSlave = slave port Stream(Bool())

      // "braces" syntax
      val bracesOut = out(Bool())
      val bracesIn = in(Bool())
      val bracesMaster = master(Stream(Bool()))
      val bracesSlave = slave(Stream(Bool()))

      // "short" syntax
      val shortOut = out Bool ()
      val shortIn = in UInt (8 bits)
      val shortMaster = master Stream (Bool())
      val shortSlave = slave Flow (Bool())

      // factories and remaining overloads
      val hardTypeIn = in port HardType(Bits(4 bits))
      val hardTypeMaster = master port HardType(Stream(Bool()))
      val enumOut = out port MyDirEnum
      val clonedOut = out cloneOf (shortIn)
      val vecOut = out port Vec(Bool(), 2)
      val variadicA, variadicB = Bool()
      out(variadicA, variadicB)
    }

    SpinalVerilog(new Component {
      /** `o`/`i` are the directions seen from the master side, flipped for a slave */
      def check(itf: TestInterface, asMaster: Boolean): Unit = {
        val o = if (asMaster) out else in
        val i = if (asMaster) in else out

        assert(itf.spacefulOut.getDirection == o)
        assert(itf.spacefulIn.getDirection == i)
        assert(itf.bracesOut.getDirection == o)
        assert(itf.bracesIn.getDirection == i)
        assert(itf.shortOut.getDirection == o)
        assert(itf.shortIn.getDirection == i)
        assert(itf.hardTypeIn.getDirection == i)
        assert(itf.enumOut.getDirection == o)
        assert(itf.clonedOut.getDirection == o)
        assert(itf.vecOut(0).getDirection == o)
        assert(itf.variadicA.getDirection == o)
        assert(itf.variadicB.getDirection == o)

        for (m <- List(itf.spacefulMaster, itf.bracesMaster, itf.shortMaster, itf.hardTypeMaster)) {
          assert(m.valid.getDirection == o)
          assert(m.ready.getDirection == i)
          assert(m.payload.getDirection == o)
        }
        for (s <- List(itf.spacefulSlave, itf.bracesSlave)) {
          assert(s.valid.getDirection == i)
          assert(s.ready.getDirection == o)
          assert(s.payload.getDirection == i)
        }
        assert(itf.shortSlave.valid.getDirection == i)
        assert(itf.shortSlave.payload.getDirection == i)
      }

      val withoutDir = TestInterface()
      withoutDir.assignDontCareToUnasigned()
      assert(withoutDir.spacefulOut.getDirection == null)

      val masterInterface = master port TestInterface()
      val slaveInterface = slave port TestInterface()
      check(masterInterface, asMaster = true)
      check(slaveInterface, asMaster = false)

      slaveInterface <> masterInterface
    })
  }

  test("IMasterSlaveDirDeclare inout") {
    case class TestInterface() extends Bundle with IMasterSlaveDirDeclare {
      val testOut = out port Bool()
      val testInout = inout port Analog(Bool())
    }
    SpinalVerilog(new Component {
      val masterInterface = master port TestInterface()
      val slaveInterface = slave port TestInterface()

      assert(masterInterface.testOut.getDirection == out)
      assert(slaveInterface.testOut.getDirection == in)
      assert(masterInterface.testInout.getDirection == inout)
      assert(slaveInterface.testInout.getDirection == inout)

      masterInterface.testOut := slaveInterface.testOut
    })
  }
}
