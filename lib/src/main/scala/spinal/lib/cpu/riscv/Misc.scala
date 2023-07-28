package spinal.lib.cpu.riscv

import spinal.core.Bool

trait RiscvHart{
  def getHartId() : Int
  def getIntMachineTimer() : Bool
  def getIntMachineSoftware() : Bool
  def getIntMachineExternal() : Bool
  def getIntSupervisorExternal() : Bool
}