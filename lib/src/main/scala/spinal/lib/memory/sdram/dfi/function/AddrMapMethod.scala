package spinal.lib.memory.sdram.dfi.function

import spinal.core._
import spinal.lib.memory.sdram.dfi.interface._

class AddrMapMethod(
    val dfiConfig: DfiConfig
) {
  val address = new Bundle {
    val column = UInt(dfiConfig.sdram.columnWidth bits)
    val bank = UInt(dfiConfig.sdram.bankWidth bits)
    val row = UInt(dfiConfig.sdram.rowWidth bits)
  }

  def addressMap(addr: UInt) = mapMethod(dfiConfig.addrMap)(addr)

  def mapMethod(addrMap: AddrMap)(addr: UInt) = addrMap match {
    case RowBankColumn => AddrMapMethod.rowBankColumnMap(dfiConfig: DfiConfig, addr: UInt)
    case BankRowColumn => AddrMapMethod.bankRowColumnMap(dfiConfig: DfiConfig, addr: UInt)
    case RowColumnBank => AddrMapMethod.rowColumnBankMap(dfiConfig: DfiConfig, addr: UInt)
  }
}
object AddrMapMethod {
  def rowBankColumnMap(dfiConfig: DfiConfig, addr: UInt) = {
    val addrMapMethod = new AddrMapMethod(dfiConfig)
    import dfiConfig._
    addrMapMethod.address.column := addr(0, sdram.columnWidth bits)
    addrMapMethod.address.bank := addr(sdram.columnWidth, sdram.bankWidth bits)
    addrMapMethod.address.row := addr(sdram.columnWidth + sdram.bankWidth, sdram.rowWidth bits)
    addrMapMethod
  }
  def bankRowColumnMap(dfiConfig: DfiConfig, addr: UInt) = {
    val addrMapMethod = new AddrMapMethod(dfiConfig)
    import dfiConfig._
    addrMapMethod.address.column := addr(0, sdram.columnWidth bits)
    addrMapMethod.address.row := addr(sdram.columnWidth, sdram.rowWidth bits)
    addrMapMethod.address.bank := addr(sdram.columnWidth + sdram.rowWidth, sdram.bankWidth bits)
    addrMapMethod
  }
  def rowColumnBankMap(dfiConfig: DfiConfig, addr: UInt) = {
    assert(dfiConfig.sdram.columnWidth >= dfiConfig.burstWidth)
    val addrMapMethod = new AddrMapMethod(dfiConfig)
    import dfiConfig._
    addrMapMethod.address.column := (addr(burstWidth + sdram.bankWidth, sdram.columnWidth - burstWidth bits) ## addr(
      0,
      burstWidth bits
    )).asUInt
    addrMapMethod.address.bank := addr(burstWidth, sdram.bankWidth bits)
    addrMapMethod.address.row := addr(sdram.columnWidth + sdram.bankWidth, sdram.rowWidth bits)
    addrMapMethod
  }
  def apply(dfiConfig: DfiConfig) = new AddrMapMethod(dfiConfig)
}
