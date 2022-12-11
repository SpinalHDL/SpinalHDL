package issues

import spinal.tester.SpinalSimFunSuite

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3Decoder, Apb3SlaveFactory}

class PR990TesterDut extends Component {
    val io = new Bundle {
        val apb = slave(Apb3(Apb3Config(16, 32)))

        val otherclk = in(Bool())
    }

    val ourApb = Apb3(Apb3Config(12, 32))
    Apb3SlaveFactory(ourApb).read(U(0xcafe), 0)

    val otherCd = ClockDomain(io.otherclk, reset=ClockDomain.current.reset)
    val otherarea = new ClockingArea(otherCd) {
        val bus = Apb3(Apb3Config(12, 32))
        Apb3SlaveFactory(bus).read(U(0xbeef), 0)
    }

    val otherInputApb = Apb3(Apb3Config(12, 32))
    otherInputApb.crossClockDomainToggle(ClockDomain.current, otherCd) >> otherarea.bus

    Apb3Decoder(
        master = io.apb,
        Seq(
            ourApb -> (0x0000, 4 KiB),
            otherInputApb -> (0x1000, 4 KiB)
        )
    )
}

class Pr990 extends SpinalSimFunSuite {
    test("ccexercise") {
        val sim =SpinalSimConfig().allOptimisation.compile(new PR990TesterDut)
        sim.doSimUntilVoid("ccexercise") { dut =>
            val mainCd = dut.clockDomain
            val otherCd = dut.otherCd
            mainCd.forkStimulus(10)
            otherCd.forkStimulus(17)

            val apb = new Apb3Driver(dut.io.apb, mainCd)

            mainCd.waitSampling()

            /**
              * This is a regression test for PR 990. The first read will trigger
              * the unfixed Apb3CCToggle to perform a transaction on the output side.
              *
              * The second read is to prime PRDATA with data that is wrong.
              *
              * The third read will be terminated too early because the CDC is now
              * ready with the read data for the first transaction. Since we primed
              * PRDATA, the pattern will not be the expected 0xbeef.
              *
              * With the fix, the tests should pass.
              */

            assert(apb.read(0x0000) == 0xcafe)
            apb.read(0x0004)
            mainCd.waitSampling(11)
            assert(apb.read(0x1000) == 0xbeef)

            mainCd.waitSampling(10)

            simSuccess()
        }
    }
}