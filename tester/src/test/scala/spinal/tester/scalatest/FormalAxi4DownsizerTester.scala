package spinal.tester.scalatest

import spinal.core.formal._
import spinal.core._
import spinal.lib.{History, Counter, master, slave}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4ReadOnly, Axi4WriteOnly, Axi4Config, Axi4Downsizer, Axi4WriteOnlyDownsizer, Axi4ReadOnlyDownsizer}

class FormalAxi4DownsizerTester extends SpinalFormalFunSuite {
  def tester(inConfig: Axi4Config, outConfig: Axi4Config) {
    FormalConfig
      //   .withBMC(10)
      //   .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val dut = FormalDut(new Axi4WriteOnlyDownsizer(inConfig, outConfig))
        val reset = ClockDomain.current.isResetActive

        assumeInitial(reset)

        val input = slave(Axi4WriteOnly(inConfig))
        dut.io.input << input

        val output = master(Axi4WriteOnly(outConfig))
        dut.io.output >> output

        val maxStall = 16
        val inputAssumes = input.withAssumes(maxStall)
        val outputAsserts = output.withAsserts(maxStall)

        output.withCovers()
        input.withCovers()
      })
  }

  val inConfig = Axi4Config(20, 64, 4, useBurst = false, useId = false)
  val outConfig = Axi4Config(20, 32, 4, useBurst = false, useId = false)
  test("64_32") {
    tester(inConfig, outConfig)
  }
}
