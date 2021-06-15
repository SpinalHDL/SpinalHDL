package spinal.tester.generator

import spinal.core._
import spinal.core.fiber._
import spinal.lib.generator._

object Play extends App{
  SpinalVerilog(new Component {
    setDefinitionName("toplevel")
    val debugCd = ClockDomainResetGenerator()
    debugCd.holdDuration.load(4095)
    debugCd.enablePowerOnReset()


    val vgaCd = ClockDomainResetGenerator()
    vgaCd.holdDuration.load(63)
    vgaCd.asyncReset(debugCd)

    val sdramCd = ClockDomainResetGenerator()
    sdramCd.holdDuration.load(63)
    sdramCd.asyncReset(debugCd)

    val systemCd = ClockDomainResetGenerator()
    systemCd.holdDuration.load(63)
    systemCd.asyncReset(sdramCd)
    systemCd.setInput(
      debugCd.outputClockDomain,
      omitReset = true
    )

    val systemClk = in Bool()
    val sdramClk = in Bool()
    val vgaClk = in Bool()

    debugCd.setInput(
      ClockDomain(
        clock = systemClk,
        frequency = FixedFrequency(100 MHz)
      )
    )
    sdramCd.setInput(
      ClockDomain(
        clock = systemClk,
        frequency = FixedFrequency(150 MHz)
      )
    )
    vgaCd.setInput(ClockDomain(vgaClk))


    val rawrrr = new Area{
      val xxx = systemCd.outputClockDomain(RegNext(U(1)))
      val yyy = vgaCd.outputClockDomain(RegNext(U(2)))
      val zzz = sdramCd.outputClockDomain(RegNext(U(3)))
    }

    val lock = new Lock
    lock.retain()
    hardFork(lock.release()) //TODO comment me
    val g = new Generator{
      dependencies += lock
      val logic = add task new Area{

      }
    }
  })
}
