package spinal.lib.cpu.riscv

import spinal.core.Bool
import spinal.lib.cpu.riscv.debug.DebugHartBus

trait RiscvHart{
  def getXlen(): Int
  def getFlen(): Int
  def getHartId() : Int
  def getIntMachineTimer() : Bool
  def getIntMachineSoftware() : Bool
  def getIntMachineExternal() : Bool
  def getIntSupervisorExternal() : Bool
  def getDebugBus() : DebugHartBus
}