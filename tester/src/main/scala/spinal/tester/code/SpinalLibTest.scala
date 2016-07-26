package spinal.tester.code

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

object SpinalLibTest {

  class BundleAA extends BundleA {
    val a = Bool
    val d = Bool
    val e = MyEnum.craft()
  }

  class BundleA extends Bundle {
    val b = Bool
    val c = UInt(8 bit)
  }

  object MyEnum extends SpinalEnum {
    val s0, s1, s2 = newElement()
  }

  object MyEnum2 extends SpinalEnum {
    val e0, e1, e2 = newElement()
  }

  class TopLevel extends Component {
    val io = new Bundle {
      val clkA = in Bool
      val resetA = in Bool

      val clkB = in Bool
      val resetB = in Bool

      val inRegBundle = in(new BundleAA())
      val outRegBundle = out(new BundleAA())

      val slaveFlow = slave(new Flow(new BundleA))
      val masterFlow = master(new Flow(new BundleA))

      val slaveStream = slave(new Stream(new BundleA))
      val masterStream = master(new Stream(new BundleA))


      val slaveStreamClkA = slave(new Stream(new BundleA))
      val masterStreamClkB = master(new Stream(new BundleA))

      //val arbiter = new StreamArbiterIO(new BundleA,4)

      val uart = new UartCtrlIo(UartCtrlGenerics())
      val uartX = new UartCtrlIo(UartCtrlGenerics())

     // val fifo = new StreamFifoIo(Bits(36 bit),256)
    }

    /*val fifo = new StreamFifo(Bits(36 bit),256)
    fifo.io <> io.fifo*/

    val uartCtrl = new UartCtrl()
    io.uart <> uartCtrl.io

    val uartCtrlX = new UartCtrl()
    io.uartX <> uartCtrlX.io


    val clockA = ClockDomain(io.clkA, io.resetA)
    val clockB = ClockDomain(io.clkB, io.resetB)


//    val arbiter = new StreamArbiterPriorityImpl(new BundleA,4,true)
//    arbiter.io <> io.arbiter

    {




      val regBundle = Reg(io.inRegBundle)
      regBundle.a.init(True)
      regBundle.e.init(MyEnum.s1())

      regBundle := io.inRegBundle
      io.outRegBundle := regBundle
    }


    io.masterFlow <-< io.slaveFlow
    io.masterStream connectFrom io.slaveStream


//    val crossClockStream = new CrossClockStream_HandShake(io.slaveStreamClkA.data,clockA,clockB)
//    crossClockStream.io.input << io.slaveStreamClkA
//    io.masterStreamClkB << crossClockStream.io.output


   io.masterStreamClkB << StreamCCByToggle(io.slaveStreamClkA,clockA,clockB)

  }


  def main(args: Array[String]) {
    println("START")
    var comp: TopLevel = null

    SpinalVhdl({
      comp = new TopLevel
      comp
    })


    println("DONE")


  }

}

