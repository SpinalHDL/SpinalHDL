package spinal.tester

import scala.collection.mutable.ArrayBuffer
import spinal.core._

package object code {
  val simWorkspacePath = spinal.tester.simWorkspacePath
  import spinal.core.internals.Phase

  def SpinalConfig(
      mode: SpinalMode = null,
      keepAll: Boolean = false,
      inlineRom: Boolean = false,
      oneFilePerComponent: Boolean = false,
      genVhdlPkg: Boolean = true,
      netlistFileName: String = null,
      dumpWave: DumpWaveConfig = null,
      defaultConfigForClockDomains: ClockDomainConfig = ClockDomainConfig(),
      defaultClockDomainFrequency: IClockDomainFrequency = UnknownFrequency(),
      removePruned: Boolean = false,
      allowOutOfRangeLiterals: Boolean = false,
      verbose: Boolean = false,
      mergeAsyncProcess: Boolean = false,
      transformationPhases: ArrayBuffer[Phase] = ArrayBuffer[Phase](),
      rtlHeader: String = null,
      randBootFixValue: Boolean = true,
      noAssert: Boolean = false,
      targetDirectory: String = simWorkspacePath
  ) = spinal.core.SpinalConfig(
    mode = mode,
    keepAll = keepAll,
    inlineRom = inlineRom,
    oneFilePerComponent = oneFilePerComponent,
    genVhdlPkg = genVhdlPkg,
    netlistFileName = netlistFileName,
    dumpWave = dumpWave,
    targetDirectory = targetDirectory,
    removePruned = removePruned,
    allowOutOfRangeLiterals = allowOutOfRangeLiterals,
    verbose = verbose,
    mergeAsyncProcess = mergeAsyncProcess,
    transformationPhases = transformationPhases,
    rtlHeader = rtlHeader,
    randBootFixValue = randBootFixValue,
    noAssert = noAssert,
    defaultConfigForClockDomains = defaultConfigForClockDomains,
    defaultClockDomainFrequency = defaultClockDomainFrequency
  )
}
