package spinal.lib

import spinal.core._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

class SpinalSimCompanionTester extends SpinalAnyFunSuite {
  test("instantiation") {
    SpinalVerilog(new Component{
      val test = new Bundle with Companion[Bool] {
        val b1 = Bool()
        @companion val b2 = Bool()
        val b3 = Bool()
      }
      assert(test().isInstanceOf[Bool])
      assert(test.b2 == test())
    })
  }

  test("assignment") {
    SpinalVerilog(new Component {
      val i = 64
      val testBits = Bits(i bits)
      val testCompanion = new Bundle with Companion[Bits] {
        val other = Bool()
        @companion val comp = Bits(i bits)
      }
      assert(testBits.getBitsWidth == testCompanion().getBitsWidth)
      testCompanion() := i
      testBits := testCompanion()
    })
  }

  test("random") {
    SimConfig.compile(new CompanionTester()).doSim(seed = 0) { dut =>
      //randomize() doesn't work in simulation, small workaround
      val rand = new scala.util.Random
      for(i <- 0 until 1000) {
        dut.io.inCompanion() #= rand.nextInt(BigInt(2).pow(18).toInt)
        sleep(1)
        assert(checkAssign(dut))
      }
    }
  }

  test("sweep") {
    SimConfig.compile(new CompanionTester()).doSim(seed = 0) { dut =>
      for(i <- 0 until BigInt(2).pow(18).toInt) {
        dut.io.inCompanion() #= i
        sleep(1)
        assert(checkAssign(dut))
      }
    }
  }

  test("nested_companion") {
    SpinalVerilog(new Component{
      //with companion needed for nesting!
      type B = Bundle with Companion[Bool]
      val outer = new Bundle with Companion[B] {
        @companion val inner = new Bundle with Companion[Bool] {
          @companion val test = Bool()
        }
      }
      assert(outer() == outer.inner)
      assert(outer.inner() == outer.inner.test)
      //double apply is yucky but necessary
      assert(outer()() == outer.inner.test)
    })
  }

  test("use_companion_name") {
    SpinalVerilog(new Component{
      val test = new Bundle with Companion[Bool] {
        @companion val companion = Bool()
      }
      //this works since companion is now a reference to the Bundle val, not the one in Companion
      assert(test.companion == test())
    })
  }

  test("incorrect_casting") {
    SpinalVerilog(new Component{
      var test: Bundle with Companion[Bits] = null
      try {
        test = new Bundle with Companion[Bits] {
          val b1 = Bits(5 bits)
          @companion val b2 = Bool()
        }
      } catch {
        //type not checked upon instantiation, should not fail
        case _: Throwable => assert(false)
      }
      try {
        //apply method should fail, casting Bits to Bool
        test()
      } catch {
        case c: java.lang.ClassCastException => println(s"Exception Caught!: ${c}")
        case _: Throwable => assert(false)
      }
    })
  }

  test("single_companion") {
    SpinalVerilog(new Component{
      try {
        val test = new Bundle with Companion[Bool] {
          val b1 = Bool()
          val b2 = Bool()
        }
        //no Companion present, should throw assertion error
        test()
      } catch {
        case a: java.lang.AssertionError => println(s"Exception Caught!: ${a}")
        case _: Throwable => assert(false)
      }
      try {
        val test = new Bundle with Companion[Bool] {
          @companion val b1 = Bool()
          @companion val b2 = Bool()
        }
        //more than one Companion present, should throw assertion error
        test()
      } catch {
        case a: java.lang.AssertionError => println(s"Exception Caught!: ${a}")
        case _: Throwable => assert(false)
      }
    })
  }

  test("non_multi_data") {
    SpinalVerilog(new Component{
      try {
        //extending Companion on a UInt (non-MultiData)
        val test = new UInt with Companion[Bits] {
          setWidth(5)
          @companion val toBits = Bits(this.getBitsWidth bits)
        }
      } catch {
        case a: java.lang.AssertionError => println(s"Exception Caught!: ${a}")
        case _: Throwable => assert(false)
      }
      try {
        /* TODO(?) : Make a simple MultiData type and try extending Companion with it here */
      } catch {
        // Should not fail that custom MultiData
        case _: Throwable => assert(false)
      }
    })
  }

  def checkAssign(dut: CompanionTester): Boolean = {
    val checkUInt = dut.io.inCompanion().toInt == dut.io.outInt.toInt
    // BitVector lsb method doesn't work in simulation, this is just a workaround
    val compLsb = (dut.io.inCompanion().toInt % 2).toBoolean
    val checkBool = compLsb == dut.io.outBool.toBoolean
    checkUInt && checkBool
  }
}

class CompanionTester extends Component {
  val b = 18 //only to 18 since any more will take eternity
  val io = new Bundle {
    val inCompanion = new Bundle with Companion[UInt] {
      @companion val comp = in UInt(b bits)
    }
    val outInt = out UInt(b bits)
    val outBool = out Bool()
  }
  io.inCompanion().simPublic()
  io.outInt.simPublic()
  io.outBool.simPublic()

  io.outInt := io.inCompanion()
  io.outBool := io.inCompanion()(0)
}
