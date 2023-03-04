package spinal.tester

import spinal.core._

package object code {    
  val simWorkspacePath = spinal.tester.simWorkspacePath
  
  def SpinalConfig(
      mode: SpinalMode = null,
      dumpWave: DumpWaveConfig = null,
      defaultConfigForClockDomains: ClockDomainConfig = ClockDomainConfig(),
      defaultClockDomainFrequency: IClockDomainFrequency = UnknownFrequency(),
      allowOutOfRangeLiterals: Boolean = false,
      verbose: Boolean = false,
      targetDirectory: String = simWorkspacePath
  ) = spinal.core.SpinalConfig(
    mode = mode,
    dumpWave = dumpWave,
    targetDirectory = targetDirectory,
    allowOutOfRangeLiterals = allowOutOfRangeLiterals,
    verbose = verbose,
    defaultConfigForClockDomains = defaultConfigForClockDomains,
    defaultClockDomainFrequency = defaultClockDomainFrequency
  )
}
