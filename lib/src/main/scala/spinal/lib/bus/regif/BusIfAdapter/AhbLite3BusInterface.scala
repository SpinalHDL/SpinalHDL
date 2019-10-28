package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.ahblite.AhbLite3
import spinal.lib.bus.misc.SizeMapping

//case class AhbLite3BusInterface(bus: AhbLite3, sizeMap: SizeMapping, readSync: Boolean = true) extends BusIf{
//  val askWrite    = bus.HSEL & bus.HTRANS === 2 & bus.HWRITE
//  val askRead     = bus.HSEL & bus.HTRANS === 2 & !bus.HWRITE
//  val doWrite     = RegNext(askWrite, False )
//  val doRead      = RegNext(askRead, False )
//
//  val readData: Bits
//  val writeData: Bits  = bus.HWDATA
//  val readError: Bool
//
//  val addressDelay = RegNextWhen(bus.HADDR, askRead | askWrite)
//  bus.HREADYOUT := True
//  bus.HRESP     := False
//  bus.HRDATA    := 0
//
//  def readAddress():UInt   =  bus.HADDR
//  def writeAddress(): UInt = bus.HADDR
//
//  def readHalt(): Unit  = {}
//  def writeHalt(): Unit = {}
//
//  def busDataWidth: Int = bus.config.dataWidth
//
//}
