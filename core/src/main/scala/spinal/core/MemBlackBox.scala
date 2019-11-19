/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core


/**
  * Ram 1w 1ra
  */
class Ram_1w_1ra(
  wordWidth      : Int,
  wordCount      : Int,
  technology     : MemTechnologyKind = auto,
  readUnderWrite : ReadUnderWritePolicy = dontCare,

  wrAddressWidth : Int,
  wrDataWidth    : Int,
  wrMaskWidth    : Int = 1,
  wrMaskEnable   : Boolean = false,

  rdAddressWidth : Int,
  rdDataWidth    : Int
) extends BlackBox {

  if (readUnderWrite == readFirst) SpinalError("readFirst mode for asynchronous read is not allowed")

  val generic = new Generic {
    val wordCount      = Ram_1w_1ra.this.wordCount
    val wordWidth      = Ram_1w_1ra.this.wordWidth
    val technology     = Ram_1w_1ra.this.technology.technologyKind
    val readUnderWrite = Ram_1w_1ra.this.readUnderWrite.readUnderWriteString

    val wrAddressWidth = Ram_1w_1ra.this.wrAddressWidth
    val wrDataWidth    = Ram_1w_1ra.this.wrDataWidth
    val wrMaskWidth    = Ram_1w_1ra.this.wrMaskWidth
    val wrMaskEnable   = Ram_1w_1ra.this.wrMaskEnable

    val rdAddressWidth = Ram_1w_1ra.this.rdAddressWidth
    val rdDataWidth    = Ram_1w_1ra.this.rdDataWidth
  }

  val io = new Bundle {
    val clk = in Bool

    val wr = new Bundle {
      val en   = in Bool
      val mask = in Bits(wrMaskWidth bits)
      val addr = in UInt(wrAddressWidth bit)
      val data = in Bits(wrDataWidth bit)
    }

    val rd = new Bundle {
      val addr =  in UInt(rdAddressWidth bit)
      val data = out Bits(rdDataWidth bit)
    }
  }

  mapCurrentClockDomain(io.clk)
  noIoPrefix()
}


/**
  * Ram 1w 1rs
  */
class Ram_1w_1rs(
  wordWidth      : Int,
  wordCount      : Int,
  readUnderWrite : ReadUnderWritePolicy = dontCare,
  technology     : MemTechnologyKind = auto,

  wrClock        : ClockDomain,
  wrAddressWidth : Int,
  wrDataWidth    : Int,
  wrMaskWidth    : Int = 1,
  wrMaskEnable   : Boolean = false,

  rdClock        : ClockDomain,
  rdAddressWidth : Int,
  rdDataWidth    : Int
) extends BlackBox {

  val generic = new Generic {
    val wordCount      = Ram_1w_1rs.this.wordCount
    val wordWidth      = Ram_1w_1rs.this.wordWidth
    var clockCrossing  = wrClock != rdClock
    val technology     = Ram_1w_1rs.this.technology.technologyKind
    val readUnderWrite = Ram_1w_1rs.this.readUnderWrite.readUnderWriteString

    val wrAddressWidth = Ram_1w_1rs.this.wrAddressWidth
    val wrDataWidth    = Ram_1w_1rs.this.wrDataWidth
    val wrMaskWidth    = Ram_1w_1rs.this.wrMaskWidth
    val wrMaskEnable   = Ram_1w_1rs.this.wrMaskEnable

    val rdAddressWidth = Ram_1w_1rs.this.rdAddressWidth
    val rdDataWidth    = Ram_1w_1rs.this.rdDataWidth
  }

  val io = new Bundle {
    val wr = new Bundle {
      val clk  = in Bool
      val en   = in Bool
      val mask = in Bits(wrMaskWidth bits)
      val addr = in UInt(wrAddressWidth bit)
      val data = in Bits(wrDataWidth bit)
    }

    val rd = new Bundle {
      val clk  = in Bool
      val en   = in Bool
      val addr = in  UInt(rdAddressWidth bit)
      val data = out Bits(rdDataWidth bit)
    }
  }

  mapClockDomain(wrClock,io.wr.clk)
  mapClockDomain(rdClock,io.rd.clk)
  noIoPrefix()
}


/**
  * Ram 2c 1w 1rs
  */
class Ram_2c_1w_1rs(
  wordWidth      : Int,
  wordCount      : Int,
  wrClock        : ClockDomain,
  rdClock        : ClockDomain,
  maskWidth      : Int = 1,
  useMask        : Boolean = false,
  readUnderWrite : ReadUnderWritePolicy = dontCare,
  tech           : MemTechnologyKind = auto
) extends BlackBox {

  val generic = new Generic {
    val wordCount      = Ram_2c_1w_1rs.this.wordCount
    val wordWidth      = Ram_2c_1w_1rs.this.wordWidth
    val maskWidth      = Ram_2c_1w_1rs.this.maskWidth
    val readUnderWrite = Ram_2c_1w_1rs.this.readUnderWrite.readUnderWriteString
    val tech           = Ram_2c_1w_1rs.this.tech.technologyKind
    var useReadEnable  = true
    val useMask        = Ram_2c_1w_1rs.this.useMask
  }

  val io = new Bundle {
    val wr = new Bundle {
      val clk  = in Bool
      val en   = in Bool
      val mask = in Bits(maskWidth bits)
      val addr = in UInt(log2Up(wordCount) bit)
      val data = in Bits(wordWidth bit)
    }

    val rd = new Bundle {
      val clk  =  in Bool
      val en   =  in Bool
      val addr =  in UInt(log2Up(wordCount) bit)
      val data = out Bits(wordWidth bit)
    }
  }

  mapClockDomain(wrClock,io.wr.clk)
  mapClockDomain(rdClock,io.rd.clk)
  noIoPrefix()
}


/**
  * Ram 1wors
  */
class Ram_1wors(wordWidth: Int, wordCount: Int, readUnderWrite: ReadUnderWritePolicy = dontCare) extends BlackBox {

  val generic = new Generic {
    val wordCount      = Ram_1wors.this.wordCount
    val wordWidth      = Ram_1wors.this.wordWidth
    val readUnderWrite = Ram_1wors.this.readUnderWrite.readUnderWriteString
    var useReadEnable  = true
  }

  val io = new Bundle {
    val clk = in Bool

    val addr = in UInt(log2Up(wordCount) bit)

    val wr = new Bundle {
      val en   = in Bool
      val data = in Bits(wordWidth bit)
    }

    val rd = new Bundle {
      val en   =  in Bool
      val data = out Bits(wordWidth bit)
    }
  }

  mapCurrentClockDomain(io.clk)
  noIoPrefix()

  //Following is not obligatory, just to describe blackbox logic
  val mem = Mem(io.wr.data, wordCount)
  when(io.wr.en) {
    mem.write(io.addr, io.wr.data)
  }
  io.rd.data := mem.readSync(io.addr, io.rd.en)
}


/**
  * Ram 1wrs
  */
class Ram_1wrs(
  wordWidth      : Int,
  wordCount      : Int,
  technology     : MemTechnologyKind,
  readUnderWrite : ReadUnderWritePolicy = dontCare,
  maskWidth      : Int,
  maskEnable     : Boolean
) extends BlackBox {

  val generic = new Generic {
    val wordCount      = Ram_1wrs.this.wordCount
    val wordWidth      = Ram_1wrs.this.wordWidth
    val readUnderWrite = Ram_1wrs.this.readUnderWrite.readUnderWriteString
    val technology     = Ram_1wrs.this.technology.technologyKind
    val maskWidth      = Ram_1wrs.this.maskWidth
    val maskEnable     = Ram_1wrs.this.maskEnable
  }

  val io = new Bundle {
    val clk = in Bool
    val en     =  in Bool
    val wr     =  in Bool
    val addr   =  in UInt(log2Up(wordCount) bit)
    val mask   =  in Bits(maskWidth bits)
    val wrData =  in Bits(wordWidth bit)
    val rdData = out Bits(wordWidth bit)
  }

  mapCurrentClockDomain(io.clk)
  noIoPrefix()
}


/**
  * Ram 2wrs
  */
class Ram_2wrs(
  wordWidth            : Int,
  wordCount            : Int,
  technology           : MemTechnologyKind,

  portA_readUnderWrite : ReadUnderWritePolicy = dontCare,
  portA_clock          : ClockDomain,
  portA_addressWidth   : Int,
  portA_dataWidth      : Int,
  portA_maskWidth      : Int,
  portA_maskEnable     : Boolean,

  portB_readUnderWrite : ReadUnderWritePolicy = dontCare,
  portB_clock          : ClockDomain,
  portB_addressWidth   : Int,
  portB_dataWidth      : Int,
  portB_maskWidth      : Int,
  portB_maskEnable     : Boolean
) extends BlackBox {

  val generic = new Generic {
    val wordCount     = Ram_2wrs.this.wordCount
    val wordWidth     = Ram_2wrs.this.wordWidth
    var clockCrossing = portA_clock != portB_clock
    val technology    = Ram_2wrs.this.technology.technologyKind

    val portA_readUnderWrite = Ram_2wrs.this.portA_readUnderWrite.readUnderWriteString
    val portA_addressWidth   = Ram_2wrs.this.portA_addressWidth
    val portA_dataWidth      = Ram_2wrs.this.portA_dataWidth
    val portA_maskWidth      = Ram_2wrs.this.portA_maskWidth
    val portA_maskEnable     = Ram_2wrs.this.portA_maskEnable

    val portB_readUnderWrite = Ram_2wrs.this.portB_readUnderWrite.readUnderWriteString
    val portB_addressWidth   = Ram_2wrs.this.portB_addressWidth
    val portB_dataWidth      = Ram_2wrs.this.portB_dataWidth
    val portB_maskWidth      = Ram_2wrs.this.portB_maskWidth
    val portB_maskEnable     = Ram_2wrs.this.portB_maskEnable
  }

  val io = new Bundle {
    val portA = new Bundle {
      val clk    =  in Bool
      val en     =  in Bool
      val wr     =  in Bool
      val mask   =  in Bits(portA_maskWidth bits)
      val addr   =  in UInt(portA_addressWidth bit)
      val wrData =  in Bits(portA_dataWidth bit)
      val rdData = out Bits(portA_dataWidth bit)
    }

    val portB = new Bundle {
      val clk    =  in Bool
      val en     =  in Bool
      val wr     =  in Bool
      val mask   =  in Bits(portB_maskWidth bits)
      val addr   =  in UInt(portB_addressWidth bit)
      val wrData =  in Bits(portB_dataWidth bit)
      val rdData = out Bits(portB_dataWidth bit)
    }
  }

  mapClockDomain(portA_clock,io.portA.clk)
  mapClockDomain(portB_clock,io.portB.clk)
  noIoPrefix()
}