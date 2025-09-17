package spinal.lib.memory.sdram.dfi

import spinal.core._

class AddrMapMethod(
    val dfiConfig: DfiConfig,
    val addrMap: AddrMap
) {
  val address = new Bundle {
    val column = UInt(dfiConfig.sdram.columnWidth bits)
    val bank = UInt(dfiConfig.sdram.bankWidth bits)
    val row = UInt(dfiConfig.sdram.rowWidth bits)
  }

  def addressMap(addr: UInt) = mapMethod(addrMap)(addr)

  def mapMethod(addrMap: AddrMap)(addr: UInt) = addrMap match {
    case RowBankColumn => AddrMapMethod.rowBankColumnMap(dfiConfig: DfiConfig, addr: UInt)
    case BankRowColumn => AddrMapMethod.bankRowColumnMap(dfiConfig: DfiConfig, addr: UInt)
    case RowColumnBank => AddrMapMethod.rowColumnBankMap(dfiConfig: DfiConfig, addr: UInt)
  }
}
object AddrMapMethod {
  def rowBankColumnMap(dfiConfig: DfiConfig, addr: UInt) = {
    val addrMapMethod = new AddrMapMethod(dfiConfig, RowBankColumn)
    addrMapMethod.address.column := addr(0, dfiConfig.sdram.columnWidth bits)
    addrMapMethod.address.bank := addr(dfiConfig.sdram.columnWidth, dfiConfig.sdram.bankWidth bits)
    addrMapMethod.address.row := addr(
      dfiConfig.sdram.columnWidth + dfiConfig.sdram.bankWidth,
      dfiConfig.sdram.rowWidth bits
    )
    addrMapMethod
  }
  def bankRowColumnMap(dfiConfig: DfiConfig, addr: UInt) = {
    val addrMapMethod = new AddrMapMethod(dfiConfig, BankRowColumn)
    addrMapMethod.address.column := addr(0, dfiConfig.sdram.columnWidth bits)
    addrMapMethod.address.row := addr(dfiConfig.sdram.columnWidth, dfiConfig.sdram.rowWidth bits)
    addrMapMethod.address.bank := addr(
      dfiConfig.sdram.columnWidth + dfiConfig.sdram.rowWidth,
      dfiConfig.sdram.bankWidth bits
    )
    addrMapMethod
  }
  def rowColumnBankMap(dfiConfig: DfiConfig, addr: UInt) = {
    val burstNumberWidth = log2Up(dfiConfig.transferPerBurst)
    assert(dfiConfig.sdram.columnWidth >= burstNumberWidth)
    val addrMapMethod = new AddrMapMethod(dfiConfig, RowColumnBank)
    addrMapMethod.address.column := (addr(
      burstNumberWidth + dfiConfig.sdram.bankWidth,
      dfiConfig.sdram.columnWidth - burstNumberWidth bits
    ) ## addr(
      0,
      burstNumberWidth bits
    )).asUInt
    addrMapMethod.address.bank := addr(burstNumberWidth, dfiConfig.sdram.bankWidth bits)
    addrMapMethod.address.row := addr(
      dfiConfig.sdram.columnWidth + dfiConfig.sdram.bankWidth,
      dfiConfig.sdram.rowWidth bits
    )
    addrMapMethod
  }
  def apply(dfiConfig: DfiConfig, addrMap: AddrMap) = new AddrMapMethod(dfiConfig, addrMap)
}
