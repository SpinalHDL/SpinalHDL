package spinal.tester.scalatest


import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._
import org.scalatest.FunSuite
import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3Config, AhbLite3Decoder}
import spinal.lib.bus.misc.SizeMapping


class DefaultAhbLite3SlaveCustom(config: AhbLite3Config) extends Component{
  val io = slave(AhbLite3(config))

  io.HREADYOUT := True
  io.HRESP     := RegNext(io.HSEL & !io.isIdle, False)
  io.HRDATA    := 0

}

class SpinalSimAhbLiteDecoder extends FunSuite {



  class TopDecoderDefault extends Component {

    val config =AhbLite3Config(32, 32)
    val io = new Bundle{
      val m1 = slave(AhbLite3(config))
      val s1 = Vec(master(AhbLite3(config)), 3)
    }

    val custom = new DefaultAhbLite3SlaveCustom(config)

    val decoder = AhbLite3Decoder(
      master = io.m1 ,
      slaves = Seq(
        io.s1(0) -> SizeMapping(0x00000000, 0x10000000),
        io.s1(1) -> SizeMapping(0x10000000, 0x10000000),
        io.s1(2) -> SizeMapping(0x20000000, 0x10000000)
      ),
      defaultSlave = custom.io
    )

  }

  class TopDecoder extends Component {

    val config =AhbLite3Config(32, 32)
    val io = new Bundle{
      val m1 = slave(AhbLite3(config))
      val s1 = Vec(master(AhbLite3(config)), 3)
    }

    val decoder = AhbLite3Decoder(
      master = io.m1 ,
      slaves = Seq(
        io.s1(0) -> SizeMapping(0x00000000, 0x10000000),
        io.s1(1) -> SizeMapping(0x10000000, 0x10000000),
        io.s1(2) -> SizeMapping(0x20000000, 0x10000000)
      )
    )

  }


  test("Decoder AHB") {

    val compiledRTL = SimConfig.withWave.withConfig(SpinalConfig(inlineRom = true)).compile(new TopDecoderDefault())

    compiledRTL.doSim{ dut =>

      dut.clockDomain.forkStimulus(2)

      dut.clockDomain.waitActiveEdge()

      // default value
      dut.io.m1.HSEL      #= false
      dut.io.m1.HADDR     #= 0x00000000
      dut.io.m1.HREADY    #= true
      dut.io.m1.HWRITE    #= true
      dut.io.m1.HSIZE     #= 2
      dut.io.m1.HBURST    #= 0
      dut.io.m1.HPROT     #= 0
      dut.io.m1.HTRANS    #= 0
      dut.io.m1.HMASTLOCK #= false
      dut.io.m1.HWDATA    #= 0x000000AA

      dut.io.s1(0).HREADYOUT #= true
      dut.io.s1(0).HRESP     #= false
      dut.io.s1(0).HRDATA    #= 0x00000001

      dut.io.s1(1).HREADYOUT #= true
      dut.io.s1(1).HRESP     #= false
      dut.io.s1(1).HRDATA    #= 0x00000002

      dut.io.s1(2).HREADYOUT #= true
      dut.io.s1(2).HRESP     #= false
      dut.io.s1(2).HRDATA    #= 0x00000003

      dut.clockDomain.waitActiveEdge(5)

      // write something
      dut.io.m1.HSEL   #= true
      dut.io.m1.HADDR  #= 0x20000100
      dut.io.m1.HTRANS #= 2

      dut.clockDomain.waitActiveEdge(1)

      dut.io.m1.HSEL   #= false
      dut.io.m1.HADDR  #= 0x20000100
      dut.io.m1.HTRANS #= 0

      dut.clockDomain.waitActiveEdge(1)

      dut.io.m1.HSEL   #= true
      dut.io.m1.HADDR  #= 0x70000100
      dut.io.m1.HTRANS #= 2

      dut.clockDomain.waitActiveEdge()

      dut.io.m1.HSEL   #= false
      dut.io.m1.HADDR  #= 0x00
      dut.io.m1.HTRANS #= 0


      dut.clockDomain.waitActiveEdge(10)
    }
  }


}
