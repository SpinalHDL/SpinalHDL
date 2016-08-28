package spinal.tester.scalatest

import org.scalatest.{Stepwise, Sequential, Suites}
import spinal.core._
import spinal.lib.soc.pinsec.Pinsec

/**
 * Created by PIC32F_USER on 22/08/2016.
*/

//object PinsecTester{
//  case class PinsecTester() extends Component{
//    val io = new Bundle{
//      val
//    }
//  }
//}

class PinsecTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "PinsecTester"
  override def pythonTests: Seq[(String,String)] = List(
    "jtag" -> "tester/src/test/python/spinal/Pinsec/jtag",
    "uart" -> "tester/src/test/python/spinal/Pinsec/uart"
  )
  override def createToplevel: Component = {
    val pinsec = new Pinsec
    pinsec.axi.rom.ram.randBoot()
    pinsec.axi.ram.ram.randBoot()
    pinsec
  }

  override def backendConfig(config: SpinalConfig): SpinalConfig = config.copy(defaultClockDomainFrequency = FixedFrequency(50 MHz))
}


//
//class OrderedSuite extends Stepwise(
//  new BundleTesterCocotbBoot,
//
//    new FixedPointTesterCocotbBoot,
//    new WhenTesterCocotbBoot,
//    new StreamTesterCocotbBoot,
//    new BlackboxTesterCocotbBoot
//  ,
//  new ZeroWidthTesterCocotbBoot
//)