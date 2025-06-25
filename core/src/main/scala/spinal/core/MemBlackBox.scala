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

import spinal.core.internals.{Expression, MemBlackboxOf, MemTopology, PhaseMemBlackBoxingWithPolicy}


object Ram_1w_1ra{
  val efinix = """module Ram_1w_1ra #(
                 |        parameter integer wordCount = 0,
                 |        parameter integer wordWidth = 0,
                 |        parameter technology = "auto",
                 |        parameter readUnderWrite = "dontCare",
                 |        parameter integer wrAddressWidth = 0,
                 |        parameter integer wrDataWidth = 0,
                 |        parameter integer wrMaskWidth = 0,
                 |        parameter wrMaskEnable = 1'b0,
                 |        parameter integer rdAddressWidth = 0,
                 |        parameter integer rdDataWidth  = 0
                 |    )(
                 |        input wire clk,
                 |        input wire wr_en,
                 |        input wire [wrMaskWidth-1:0] wr_mask,
                 |        input wire [wrAddressWidth-1:0] wr_addr,
                 |        input wire [wrDataWidth-1:0] wr_data,
                 |        input wire [rdAddressWidth-1:0] rd_addr,
                 |        output wire [rdDataWidth-1:0] rd_data
                 |    );
                 |
                 |    reg [wrDataWidth-1:0] ram_block [(2**wrAddressWidth)-1:0];
                 |    always @ (posedge clk) begin
                 |        if(wr_en) begin
                 |           ram_block[wr_addr] <= wr_data;
                 |        end
                 |    end
                 |
                 |    assign rd_data = ram_block[rd_addr];
                 |endmodule""".stripMargin
}

class Ram_1w_1ra(
  val wordWidth      : Int,
  val wordCount      : Int,
  val technology     : MemTechnologyKind = auto,
  val readUnderWrite : ReadUnderWritePolicy = dontCare,

  val wrAddressWidth : Int,
  val wrDataWidth    : Int,
  val wrMaskWidth    : Int = 1,
  val wrMaskEnable   : Boolean = false,

  val rdAddressWidth : Int,
  val rdDataWidth    : Int
) extends BlackBox {

  if (readUnderWrite == readFirst) SpinalError("readFirst mode for asynchronous read is not allowed")

  addGenerics(
    "wordCount"      -> Ram_1w_1ra.this.wordCount,
    "wordWidth"      -> Ram_1w_1ra.this.wordWidth,
    "technology"     -> Ram_1w_1ra.this.technology.technologyKind,
    "readUnderWrite" -> Ram_1w_1ra.this.readUnderWrite.readUnderWriteString,
    "wrAddressWidth" -> Ram_1w_1ra.this.wrAddressWidth,
    "wrDataWidth"    -> Ram_1w_1ra.this.wrDataWidth,
    "wrMaskWidth"    -> Ram_1w_1ra.this.wrMaskWidth,
    "wrMaskEnable"   -> Ram_1w_1ra.this.wrMaskEnable,
    "rdAddressWidth" -> Ram_1w_1ra.this.rdAddressWidth,
    "rdDataWidth"    -> Ram_1w_1ra.this.rdDataWidth
  )

  val io = new Bundle {
    val clk = in.Bool()

    val wr = new Bundle {
      val en   = in.Bool()
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



object Ram_1w_1rs{
  val efinix = """module Ram_1w_1rs #(
                 |        parameter integer wordCount = 0,
                 |        parameter integer wordWidth = 0,
                 |        parameter clockCrossing = 1'b0,
                 |        parameter technology = "auto",
                 |        parameter readUnderWrite = "dontCare",
                 |        parameter integer wrAddressWidth = 0,
                 |        parameter integer wrDataWidth = 0,
                 |        parameter integer wrMaskWidth = 0,
                 |        parameter wrMaskEnable = 1'b0,
                 |        parameter integer rdAddressWidth = 0,
                 |        parameter integer rdDataWidth  = 0,
                 |        parameter integer rdLatency = 1
                 |    )(
                 |        input wr_clk,
                 |        input wr_en,
                 |        input [wrMaskWidth-1:0] wr_mask,
                 |        input [wrAddressWidth-1:0] wr_addr,
                 |        input [wrDataWidth-1:0] wr_data,
                 |        input rd_clk,
                 |        input rd_en,
                 |        input rd_dataEn,
                 |        input [rdAddressWidth-1:0] rd_addr,
                 |        output [rdDataWidth-1:0] rd_data
                 |    );
                 |
                 |    reg [wrDataWidth-1:0] ram_block [(2**wrAddressWidth)-1:0];
                 |    integer i;
                 |    localparam COL_WIDTH = wrDataWidth/wrMaskWidth;
                 |    always @ (posedge wr_clk) begin
                 |        if(wr_en) begin
                 |            for(i=0;i<wrMaskWidth;i=i+1) begin
                 |                if(wr_mask[i]) begin // byte-enable
                 |                    ram_block[wr_addr][i*COL_WIDTH +: COL_WIDTH] <= wr_data[i*COL_WIDTH +:COL_WIDTH];
                 |                end
                 |            end
                 |        end
                 |    end
                 |    reg [rdDataWidth-1:0] ram_rd_data;
                 |    always @ (posedge rd_clk) begin
                 |        if(rd_en) begin
                 |            ram_rd_data <= ram_block[rd_addr];
                 |        end
                 |    end
                 |    assign rd_data = ram_rd_data;
                 |
                 |endmodule""".stripMargin
}

class Ram_1w_1rs(
  val wordWidth      : Int,
  val wordCount      : Int,
  val readUnderWrite : ReadUnderWritePolicy = dontCare,
  val technology     : MemTechnologyKind = auto,

  val wrClock        : ClockDomain,
  val wrAddressWidth : Int,
  val wrDataWidth    : Int,
  val wrMaskWidth    : Int = 1,
  val wrMaskEnable   : Boolean = false,

  val rdClock        : ClockDomain,
  val rdAddressWidth : Int,
  val rdDataWidth    : Int,
  val rdLatency      : Int = 1
) extends BlackBox {

  addGenerics(
    "wordCount"      -> Ram_1w_1rs.this.wordCount,
    "wordWidth"      -> Ram_1w_1rs.this.wordWidth,
    "clockCrossing"  -> (wrClock != rdClock),
    "technology"     -> Ram_1w_1rs.this.technology.technologyKind,
    "readUnderWrite" -> Ram_1w_1rs.this.readUnderWrite.readUnderWriteString,
    "wrAddressWidth" -> Ram_1w_1rs.this.wrAddressWidth,
    "wrDataWidth"    -> Ram_1w_1rs.this.wrDataWidth,
    "wrMaskWidth"    -> Ram_1w_1rs.this.wrMaskWidth,
    "wrMaskEnable"   -> Ram_1w_1rs.this.wrMaskEnable,
    "rdAddressWidth" -> Ram_1w_1rs.this.rdAddressWidth,
    "rdDataWidth"    -> Ram_1w_1rs.this.rdDataWidth,
    "rdLatency"      -> Ram_1w_1rs.this.rdLatency
  )


  val io = new Bundle {
    val wr = new Bundle {
      val clk  = in Bool()
      val en   = in Bool()
      val mask = in Bits(wrMaskWidth bits)
      val addr = in UInt(wrAddressWidth bit)
      val data = in Bits(wrDataWidth bit)
    }

    val rd = new Bundle {
      val clk    = in Bool()
      val en     = in Bool()
      val addr   = in UInt(rdAddressWidth bit)
      val dataEn = in Bool() default(True) //Only used if rdLatency > 1
      val data   = out Bits(rdDataWidth bit)
    }
  }

  mapClockDomain(wrClock,io.wr.clk)
  mapClockDomain(rdClock,io.rd.clk)
  noIoPrefix()
}


class Ram_2c_1w_1rs(
  val wordWidth      : Int,
  val wordCount      : Int,
  val wrClock        : ClockDomain,
  val rdClock        : ClockDomain,
  val maskWidth      : Int = 1,
  val useMask        : Boolean = false,
  val readUnderWrite : ReadUnderWritePolicy = dontCare,
  val tech           : MemTechnologyKind = auto
) extends BlackBox {

  addGenerics(
    "wordCount"      -> Ram_2c_1w_1rs.this.wordCount,
    "wordWidth"      -> Ram_2c_1w_1rs.this.wordWidth,
    "maskWidth"      -> Ram_2c_1w_1rs.this.maskWidth,
    "readUnderWrite" -> Ram_2c_1w_1rs.this.readUnderWrite.readUnderWriteString,
    "tech"           -> Ram_2c_1w_1rs.this.tech.technologyKind,
    "useReadEnable"  -> true,
    "useMask"        -> Ram_2c_1w_1rs.this.useMask
  )

  val io = new Bundle {
    val wr = new Bundle {
      val clk  = in Bool()
      val en   = in Bool()
      val mask = in Bits(maskWidth bits)
      val addr = in UInt(log2Up(wordCount) bit)
      val data = in Bits(wordWidth bit)
    }

    val rd = new Bundle {
      val clk  =  in Bool()
      val en   =  in Bool()
      val addr =  in UInt(log2Up(wordCount) bit)
      val data = out Bits(wordWidth bit)
    }
  }

  mapClockDomain(wrClock,io.wr.clk)
  mapClockDomain(rdClock,io.rd.clk)
  noIoPrefix()
}


class Ram_1wors(val wordWidth: Int,
                val wordCount: Int,
                val readUnderWrite: ReadUnderWritePolicy = dontCare) extends BlackBox {

  addGenerics(
    "wordCount"      -> Ram_1wors.this.wordCount,
    "wordWidth"      -> Ram_1wors.this.wordWidth,
    "readUnderWrite" -> Ram_1wors.this.readUnderWrite.readUnderWriteString,
    "useReadEnable"  -> true
  )

  val io = new Bundle {
    val clk = in Bool()

    val addr = in UInt(log2Up(wordCount) bit)

    val wr = new Bundle {
      val en   = in Bool()
      val data = in Bits(wordWidth bit)
    }

    val rd = new Bundle {
      val en   =  in Bool()
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


class Ram_1wrs(
  val wordWidth      : Int,
  val wordCount      : Int,
  val technology     : MemTechnologyKind,
  val readUnderWrite : ReadUnderWritePolicy = dontCare,
  val duringWrite    : DuringWritePolicy = dontCare,
  val maskWidth      : Int,
  val maskEnable     : Boolean
) extends BlackBox {

  addGenerics(
    "wordCount"      -> Ram_1wrs.this.wordCount,
    "wordWidth"      -> Ram_1wrs.this.wordWidth,
    "readUnderWrite" -> Ram_1wrs.this.readUnderWrite.readUnderWriteString,
    "duringWrite"    -> Ram_1wrs.this.duringWrite.duringWriteString,
    "technology"     -> Ram_1wrs.this.technology.technologyKind,
    "maskWidth"      -> Ram_1wrs.this.maskWidth,
    "maskEnable"     -> Ram_1wrs.this.maskEnable
  )

  val io = new Bundle {
    val clk    =  in Bool()
    val en     =  in Bool()
    val wr     =  in Bool()
    val addr   =  in UInt(log2Up(wordCount) bit)
    val mask   =  in Bits(maskWidth bits)
    val wrData =  in Bits(wordWidth bit)
    val rdData = out Bits(wordWidth bit)
  }

  mapCurrentClockDomain(io.clk)
  noIoPrefix()
}


class Ram_2wrs(
  val wordWidth            : Int,
  val wordCount            : Int,
  val technology           : MemTechnologyKind,

  val portA_readUnderWrite : ReadUnderWritePolicy = dontCare,
  val portA_duringWrite    : DuringWritePolicy = dontCare,
  val portA_clock          : ClockDomain,
  val portA_addressWidth   : Int,
  val portA_dataWidth      : Int,
  val portA_maskWidth      : Int,
  val portA_maskEnable     : Boolean,

  val portB_readUnderWrite : ReadUnderWritePolicy = dontCare,
  val portB_duringWrite    : DuringWritePolicy = dontCare,
  val portB_clock          : ClockDomain,
  val portB_addressWidth   : Int,
  val portB_dataWidth      : Int,
  val portB_maskWidth      : Int,
  val portB_maskEnable     : Boolean
) extends BlackBox {

  addGenerics(
    "wordCount"            -> Ram_2wrs.this.wordCount,
    "wordWidth"            -> Ram_2wrs.this.wordWidth,
    "clockCrossing"        -> (portA_clock != portB_clock),
    "technology"           -> Ram_2wrs.this.technology.technologyKind,
    "portA_readUnderWrite" -> Ram_2wrs.this.portA_readUnderWrite.readUnderWriteString,
    "portA_duringWrite"    -> Ram_2wrs.this.portA_duringWrite.duringWriteString,
    "portA_addressWidth"   -> Ram_2wrs.this.portA_addressWidth,
    "portA_dataWidth"      -> Ram_2wrs.this.portA_dataWidth,
    "portA_maskWidth"      -> Ram_2wrs.this.portA_maskWidth,
    "portA_maskEnable"     -> Ram_2wrs.this.portA_maskEnable,
    "portB_readUnderWrite" -> Ram_2wrs.this.portB_readUnderWrite.readUnderWriteString,
    "portB_duringWrite"    -> Ram_2wrs.this.portB_duringWrite.duringWriteString,
    "portB_addressWidth"   -> Ram_2wrs.this.portB_addressWidth,
    "portB_dataWidth"      -> Ram_2wrs.this.portB_dataWidth,
    "portB_maskWidth"      -> Ram_2wrs.this.portB_maskWidth,
    "portB_maskEnable"     -> Ram_2wrs.this.portB_maskEnable
  )

  val io = new Bundle {
    val portA = new Bundle {
      val clk    =  in Bool()
      val en     =  in Bool()
      val wr     =  in Bool()
      val mask   =  in Bits(portA_maskWidth bits)
      val addr   =  in UInt(portA_addressWidth bit)
      val wrData =  in Bits(portA_dataWidth bit)
      val rdData = out Bits(portA_dataWidth bit)
    }

    val portB = new Bundle {
      val clk    =  in Bool()
      val en     =  in Bool()
      val wr     =  in Bool()
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



class Ram_Generic(val topo : MemTopology, utils : PhaseMemBlackBoxingWithPolicy) extends BlackBox {
  def wrapBool(that: Expression): Bool = that match {
    case that: Bool => that
    case that       =>
      val ret = Bool()
      ret.assignFrom(that)
      ret
  }

  def wrapConsumers(oldSource: Expression, newSource: Expression): Unit ={
    utils.wrapConsumers(topo, oldSource, newSource)
  }

  setCompositeName(topo.mem)
  addTag(new MemBlackboxOf(topo.mem.asInstanceOf[Mem[Data]]))

  val w = for(p <- topo.writes) yield new Area{
    val maskWidth = p.getMaskWidth()
    val clk  = in Bool()
    val en   = in Bool()
    val mask = in Bits(maskWidth bits)
    val addr = in UInt(p.getAddressWidth bits)
    val data = in Bits(p.getWidth bits)
    mapClockDomain(p.clockDomain, clk)

    parent.rework{
      en := wrapBool(p.writeEnable) && p.clockDomain.isClockEnableActive
      addr.assignFrom(p.address)
      data.assignFrom(p.data)
      mask.assignFrom((if (p.mask != null) p.mask else B"1"))
    }
  }

  val rs = for(p <- topo.readsSync) yield new Area{
    val clk  = in Bool()
    val en   = in Bool()
    val addr = in UInt(p.getAddressWidth bits)
    val data = out Bits(p.getWidth bits)
    mapClockDomain(p.clockDomain, clk)
    parent.rework{
      en := wrapBool(p.readEnable) && p.clockDomain.isClockEnableActive
      addr.assignFrom(p.address)
      wrapConsumers(p, data)
    }
  }

  val ra = for(p <- topo.readsAsync) yield new Area{
    val addr = in UInt(p.getAddressWidth bits)
    val data = out Bits(p.getWidth bits)
    parent.rework {
      addr.assignFrom(p.address)
      wrapConsumers(p, data)
    }
  }

  val rw = for(p <- topo.readWriteSync) yield new Area{
    val maskWidth = p.getMaskWidth()
    val clk  = in Bool()
    val en   = in Bool()
    val wr   = in Bool()
    val mask = in Bits(maskWidth bits)
    val addr = in UInt(p.getAddressWidth bits)
    val wrData = in Bits(p.getWidth bits)
    val rdData = out Bits(p.getWidth bits)
    mapClockDomain(p.clockDomain, clk)
    parent.rework {
      addr.assignFrom(p.address)
      en.assignFrom(wrapBool(p.chipSelect) && p.clockDomain.isClockEnableActive)
      wr.assignFrom(p.writeEnable)
      wrData.assignFrom(p.data)
      mask.assignFrom((if (p.mask != null) p.mask else B"1"))
      wrapConsumers(p, rdData)
    }
  }
}
