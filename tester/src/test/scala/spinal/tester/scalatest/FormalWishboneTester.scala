package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.formal._
import spinal.core.formal._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.bus.wishbone.{AddressGranularity, WishboneArbiter, WishboneConfig}


class WishboneArbiterFormal(config : WishboneConfig, portCount : Int) extends Component {
  val dut = FormalDut(new WishboneArbiter(config, portCount))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.anyseq_inputs()
}

class FormalWishboneArbiter extends SpinalFormalFunSuite {
  for(portCount <- Seq(1, 2, 5);
      granularity <- Seq(AddressGranularity.WORD, AddressGranularity.BYTE, AddressGranularity.UNSPECIFIED);
      useErr <- Seq(true, false)) {
    val config = WishboneConfig(32, 32, useERR = useErr, addressGranularity = granularity)
    test(s"Test WB Arbiter p${portCount} err ${useErr} granularity ${granularity}") {
      FormalConfig.withDebug.withProve(15).withCover(15).
        doVerify(new WishboneArbiterFormal(config, portCount))
    }
  }
}