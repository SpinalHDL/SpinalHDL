package integration

import spinal.tester.SpinalTesterCocotbBase

import spinal.core._

object CommonTester {
  class BundleA() extends Bundle {
    val bod = new Bundle {
      val gggg = Bool()
      val aosi = UInt(3 bit)
    }
    val ahe = Bool()
    val zwg = Bool()
  }

  case class BundleAA() extends BundleA {
    val vsw  = Bool()
    val lwee = UInt(5 bit)
  }

  case class CommonTester() extends Component {
    val io = new Bundle {
      val conds = in port Vec(Bool(), 8)

      val inUIntA      = in  port UInt(8 bit)
      val inUIntB      = in  port UInt(8 bit)
      val outUIntAdder = out port UInt()

      val inAA      = in  port BundleAA()
      val inAABits  = in  port Bits(BundleAA().getBitsWidth bit)
      val outAA     = out port BundleAA()
      val outAABits = out port Bits(BundleAA().getBitsWidth bit)

      val complexLiteral = out port UInt(16 bit)

      val assign = new Bundle {
        val sel      = in  port Vec(UInt(4 bit), 4)
        val bitDemux = out port Bits(16 bit)

        def doIt(): Unit = {
          bitDemux         := B(0)
          bitDemux(sel(0)) := conds(0)

          when(conds(1)) {
            bitDemux(sel(1)) := conds(2)
          } elsewhen (conds(3)) {
            bitDemux(sel(0)) := conds(4)
          }

          when(conds(5)) {
            bitDemux(sel(1)) := conds(6)
          }

          bitDemux(5) := True
        }
      }

      def doIt(): Unit = {
        assign.doIt()
      }
    }

    io.doIt()

    io.outAA.assignFromBits(io.inAABits)
    io.outAABits := io.inAA.asBits

    io.complexLiteral(15 downto 4)  := 0x70
    io.complexLiteral(15 downto 12) := (U(2) + U(1)).resized
    io.complexLiteral(6)            := True
    io.complexLiteral(3)            := True
    io.complexLiteral(5)            := True
    io.complexLiteral(3 downto 0)   := 2
    io.complexLiteral(13)           := False

    def combAdderFunc(x: UInt, y: UInt): UInt = {
      val ret  = UInt(widthOf(x) max widthOf(y) bits)
      val size = io.inUIntA.getWidth

      var c = False
      for (i <- 0 until size) {
        val a = x(i)
        val b = y(i)
        ret(i) := a ^ b ^ c
        c \= (a & b) | (a & c) | (b & c)
      }

      ret
    }

    io.outUIntAdder := combAdderFunc(io.inUIntA, io.inUIntB)

    // Clone test
    case class MyBundle(paramBool: Bool, asd: Int) extends Bundle {
      val a = cloneOf(paramBool)
    }

    case class MyBundle2() extends Bundle {
      val a = Bool()
    }

    cloneOf(MyBundle(True, 1))
    cloneOf(MyBundle2())
  }
}

class CommonTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "CommonTester"

  override def createToplevel: Component = new CommonTester.CommonTester

  override def pythonTestLocation: String =
    "tester/src/test/python/spinal/CommonTester"
}
