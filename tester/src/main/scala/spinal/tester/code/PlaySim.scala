package spinal.tester.code
import spinal.sim._
import spinal.core._
import spinal.core.sim._

import scala.util.Random

object SimSynchronouExample {
  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }
    io.result := RegNext(io.a + io.b - io.c) init(0)
  }

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new Dut, "choubaka"){ dut =>
      dut.clockDomain.forkStimulus(period = 10)

      var idx = 0
      while(idx < 100) {
        val a, b, c = Random.nextInt(256)
        dut.io.a #= a
        dut.io.b #= b
        dut.io.c #= c
        dut.clockDomain.waitActiveEdge()
        assert(dut.io.result.toInt == ((a+b-c) & 0xFF))
        idx += 1
      }
    }
    SimConfig.withWave.doSim(new Dut, "choubaka"){ dut =>
      dut.clockDomain.forkStimulus(period = 10)

      var idx = 0
      while(idx < 100) {
        val a, b, c = Random.nextInt(256)
        dut.io.a #= a
        dut.io.b #= b
        dut.io.c #= c
        dut.clockDomain.waitActiveEdge()
        assert(dut.io.result.toInt == ((a+b-c) & 0xFF))
        idx += 1
      }
    }
  }
}



object SimSTimedCall {
  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
    }
    io.result := (io.a + io.b - io.c)
  }

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.doSim(new Dut, "choubaka"){ dut =>
      dut.io.a #= 5
      def f = {
        dut.io.a #= dut.io.a.toInt + 1
      }
      delayed(10)(f)

      sleep(1000)
    }
  }
}



object PlaySimGhdl extends App{
  class toplevel extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
      val comb   = out UInt (8 bits)
    }
    io.result := RegNext(io.a + io.b - io.c) init (0)
    io.comb := RegNext(io.a + io.b - io.c) init (0)
//    io.comb := io.a + io.b + io.c
  }

  SimConfig.withGhdl.withWave.doSim(new toplevel, "choubaka") { dut =>

//    dut.io.c #= 42
//    sleep(100)
//    for(i <- 0 until 20) sleep(0)
    sleep(1)

    dut.clockDomain.forkStimulus(period = 10)
    dut.clockDomain.forkSimSpeedPrinter(0.2)


    var idx = 0
    while (idx < 100) {
      val a, b, c = Random.nextInt(256)
      dut.io.a #= a
      dut.io.b #= b
      dut.io.c #= c
      dut.clockDomain.waitActiveEdge()
      sleep(0) //TODO assert should fail without it.
      assert(dut.io.result.toInt == ((a + b - c) & 0xFF))
      idx += 1
    }



    sleep(1000)
  }
}

object PlaySimGhdl2 extends App{
  class toplevel extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
      val comb   = out UInt (8 bits)
    }
    io.result := RegNext(io.a + io.b - io.c) init (0)
    io.comb := io.a + io.b + io.c
  }

  SimConfig.withWave.withGhdl.doSim(new toplevel, "rawrr"){ dut =>
    dut.clockDomain.forkStimulus(period = 10)
    dut.clockDomain.forkSimSpeedPrinter(0.2)


    var model = -1
    var times = 0
    dut.clockDomain.onSamplings{
//      println(simTime())
//      if(times > 10) {
//        simFailure("Rawr at ")
        assert(dut.io.result.toInt == model || model == -1)
        model = ((dut.io.a.toInt + dut.io.b.toInt - dut.io.c.toInt) & 0xFF)
//      }
      dut.io.a #= Random.nextInt(256)
      dut.io.b #= Random.nextInt(256)
      dut.io.c #= Random.nextInt(256)
      times += 1
    }


//    sleep(1000)
    for(repeat <- 0 until 4) {
      val startAt = System.nanoTime
      waitUntil(times == 2000000)
      times = 0
      val endAt = System.nanoTime
      System.out.println((endAt - startAt) * 1e-6 + " ms")
    }
  }
}



object PlaySimGhdl3 extends App{
  class toplevel extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (8 bits)
      val result = out UInt (8 bits)
      val comb   = out UInt (8 bits)
    }
    io.result := RegNext(io.a + io.b - io.c) init (0)
    io.comb := io.a + io.b + io.c
  }
  val compiled = SimConfig.withWave.withGhdl.compile(new toplevel)
  def tb(){
    compiled.doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.forkSimSpeedPrinter(0.2)

//      sleep(1000)
//              dut.io.a #= Random.nextInt(256)
//      sleep(1000)

      var model = -1
      var times = 0
      dut.clockDomain.onSamplings {
        //      println(simTime())
        //      if(times > 10) {
        //        simFailure("Rawr at ")
        assert(dut.io.result.toInt == model || model == -1)
        model = ((dut.io.a.toInt + dut.io.b.toInt - dut.io.c.toInt) & 0xFF)
        //      }
        dut.io.a #= Random.nextInt(256)
        dut.io.b #= Random.nextInt(256)
        dut.io.c #= Random.nextInt(256)
        times += 1
      }


      //    sleep(1000)
      for (repeat <- 0 until 4) {
        val startAt = System.nanoTime
        waitUntil(times == 2000)
        times = 0
        val endAt = System.nanoTime
        System.out.println((endAt - startAt) * 1e-6 + " ms")
      }
    }
  }

  tb()
  tb()
  tb()
  tb()
  tb()
}


// export TEST=rawrr
// cd simWorkspace/unnamed
// verilator_coverage --write-info $TEST.info  --annotate asd $TEST.dat
// genhtml $TEST.info --output-directory coverage
// xdg-open coverage/index.html

object PlayCoverage extends App{
  val compiled = SimConfig.withCoverage.compile(new Component{
    val a,b = in UInt(8 bits)
    val result = out UInt(8 bits)
    result := RegNext(a + b)
  })
  compiled.doSim("rawrr"){dut =>
    dut.clockDomain.forkStimulus(10)
    for(i <- 0 until 10){
      dut.a #= i
      dut.b #= 0
      dut.clockDomain.waitSampling()
    }
  }
}


object PlaySimGhdlBugTodo extends App{
  class Toplevel extends Component {
    val a = in SInt (16 bits)
    val b = out SInt (16 bits)
    b := a
  }

  SimConfig.withGhdl.withWave.doSim(new Toplevel){ dut =>
    dut.a #= 1
    sleep(1)
    assert(dut.b.toInt == 1)
    dut.a #= -1
    sleep(1)
    assert(dut.b.toInt == -1)
  }
}

object PlayTracingOff extends App{
  class Sub extends Component {
    val a = in SInt (16 bits)
    val b = out SInt (16 bits)
    b := a
  }

  class WaveTop extends Component {
    val io = new Bundle{
      val a = in SInt (16 bits)
      val b = out SInt (16 bits)
    }
    val sub0 = new Sub
    sub0.a := io.a
    val sub1 = new Sub
    sub1.a := sub0.b
    io.b := sub1.b
  }

  SpinalConfig(targetDirectory = "./tmp").generateVerilog{
    val dut = new WaveTop
    dut.sub0.tracingOff()
    dut
  }
}

