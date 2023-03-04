package spinal.tester

import spinal.core._

package object scalatest {
  val simWorkspacePath = spinal.tester.simWorkspacePath

  def SpinalConfig(
      mode: SpinalMode = null,
      dumpWave: DumpWaveConfig = null,
      defaultConfigForClockDomains: ClockDomainConfig = ClockDomainConfig(),
      defaultClockDomainFrequency: IClockDomainFrequency = UnknownFrequency(),
      allowOutOfRangeLiterals: Boolean = false,
      device: Device = Device(),
      verbose: Boolean = false,
      targetDirectory: String = simWorkspacePath
  ) = spinal.core.SpinalConfig(
    mode = mode,
    dumpWave = dumpWave,
    targetDirectory = targetDirectory,
    allowOutOfRangeLiterals = allowOutOfRangeLiterals,
    device = device,
    verbose = verbose,
    defaultConfigForClockDomains = defaultConfigForClockDomains,
    defaultClockDomainFrequency = defaultClockDomainFrequency
  )

  def SpinalVerilog[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig().generateVerilog(gen)
  def SpinalVhdl[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig().generateVerilog(gen)
  def SpinalSystemVerilog[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig().generateVerilog(gen)
}
