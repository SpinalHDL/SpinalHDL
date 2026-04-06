package spinal.lib.blackbox.ihp.sg13g2

import spinal.core._
import spinal.core.internals.{PhaseContext, PhaseNetlist}
import spinal.lib._

import scala.collection.mutable

case class IhpSramMacro(name: String, ports: Int, depth: Int, width: Int)

// No SRAM compiler available yet. Only some macros are vailable.
object IhpSramMacro {
  val defaults: Seq[IhpSramMacro] = Seq(
    // 1P bm_bist
    IhpSramMacro("RM_IHPSG13_1P_64x64_c2_bm_bist",   1,   64,  64),
    IhpSramMacro("RM_IHPSG13_1P_256x8_c3_bm_bist",   1,  256,   8),
    IhpSramMacro("RM_IHPSG13_1P_256x16_c2_bm_bist",  1,  256,  16),
    IhpSramMacro("RM_IHPSG13_1P_256x32_c2_bm_bist",  1,  256,  32),
    IhpSramMacro("RM_IHPSG13_1P_256x48_c2_bm_bist",  1,  256,  48),
    IhpSramMacro("RM_IHPSG13_1P_256x64_c2_bm_bist",  1,  256,  64),
    IhpSramMacro("RM_IHPSG13_1P_512x8_c3_bm_bist",   1,  512,   8),
    IhpSramMacro("RM_IHPSG13_1P_512x16_c2_bm_bist",  1,  512,  16),
    IhpSramMacro("RM_IHPSG13_1P_512x32_c2_bm_bist",  1,  512,  32),
    IhpSramMacro("RM_IHPSG13_1P_512x64_c2_bm_bist",  1,  512,  64),
    IhpSramMacro("RM_IHPSG13_1P_1024x8_c2_bm_bist",  1, 1024,   8),
    IhpSramMacro("RM_IHPSG13_1P_1024x16_c2_bm_bist", 1, 1024,  16),
    IhpSramMacro("RM_IHPSG13_1P_1024x32_c2_bm_bist", 1, 1024,  32),
    IhpSramMacro("RM_IHPSG13_1P_1024x64_c2_bm_bist", 1, 1024,  64),
    IhpSramMacro("RM_IHPSG13_1P_2048x64_c2_bm_bist", 1, 2048,  64),
    IhpSramMacro("RM_IHPSG13_1P_4096x8_c3_bm_bist",  1, 4096,   8),
    IhpSramMacro("RM_IHPSG13_1P_4096x16_c3_bm_bist", 1, 4096,  16),
    // 2P bm_bist
    IhpSramMacro("RM_IHPSG13_2P_256x8_c2_bm_bist",   2,  256,   8),
    IhpSramMacro("RM_IHPSG13_2P_256x16_c2_bm_bist",  2,  256,  16),
    IhpSramMacro("RM_IHPSG13_2P_256x32_c2_bm_bist",  2,  256,  32),
    IhpSramMacro("RM_IHPSG13_2P_512x8_c2_bm_bist",   2,  512,   8),
    IhpSramMacro("RM_IHPSG13_2P_512x16_c2_bm_bist",  2,  512,  16),
    IhpSramMacro("RM_IHPSG13_2P_512x32_c2_bm_bist",  2,  512,  32),
    IhpSramMacro("RM_IHPSG13_2P_1024x16_c2_bm_bist", 2, 1024,  16),
    IhpSramMacro("RM_IHPSG13_2P_1024x32_c2_bm_bist", 2, 1024,  32)
  )
}


class PhaseIhpSramBlackBox(macros: Seq[IhpSramMacro] = IhpSramMacro.defaults) extends PhaseNetlist {
  override def impl(pc: PhaseContext) = {
    pc.walkComponents{
      // Let's tune the Ram_Generic blackbox to match our desires
      case bb : Ram_Generic => {
        //Collect clocks
        val cds = mutable.LinkedHashSet[ClockDomain]()
        for((p,i) <- bb.topo.writes.zipWithIndex) cds += p.clockDomain
        for((p,i) <- bb.topo.readsSync.zipWithIndex) cds += p.clockDomain
        for((p,i) <- bb.topo.readWriteSync.zipWithIndex) cds += p.clockDomain

        val width     = bb.topo.mem.getWidth
        val addrWidth = log2Up(bb.topo.mem.wordCount)

        (bb.rw.size, bb.w.size, bb.rs.size, bb.ra.size, cds.size) match {
          case (1,0,0,0,1) => {
            macros.find(m => m.ports == 1 && m.depth == bb.topo.mem.wordCount && m.width == width) match {
              case None =>
                SpinalWarning(s"No 1P IHP SRAM macro for depth=${bb.topo.mem.wordCount} width=$width, leaving as generic")
              case Some(sram) => {
                bb.setDefinitionName(sram.name)

                val rw = bb.rw(0)
                rw.clk    setName("A_CLK")
                rw.en.removeAssignments()
                rw.en.removeStatement()
                rw.wr     setName("A_WEN")
                rw.addr   setName("A_ADDR")
                rw.wrData setName("A_DIN")
                rw.rdData setName("A_DOUT")
                if(bb.topo.readWriteSync(0).mask != null) {
                  rw.mask setName("A_BM")
                } else {
                  rw.mask.removeAssignments()
                  rw.mask.removeStatement()
                }

                var a_men, a_dly: Bool = null
                var a_bm: Bits = null
                var a_bist_clk, a_bist_en, a_bist_men, a_bist_wen, a_bist_ren: Bool = null
                var a_bist_addr: UInt = null
                var a_bist_din, a_bist_bm: Bits = null

                bb.rework {
                  a_men       = in(Bool())               setName("A_MEN")
                  a_dly       = in(Bool())               setName("A_DLY")
                  if(bb.topo.readWriteSync(0).mask == null) {
                    a_bm      = in(Bits(width bits))     setName("A_BM")
                  }
                  a_bist_clk  = in(Bool())               setName("A_BIST_CLK")
                  a_bist_en   = in(Bool())               setName("A_BIST_EN")
                  a_bist_men  = in(Bool())               setName("A_BIST_MEN")
                  a_bist_wen  = in(Bool())               setName("A_BIST_WEN")
                  a_bist_ren  = in(Bool())               setName("A_BIST_REN")
                  a_bist_addr = in(UInt(addrWidth bits)) setName("A_BIST_ADDR")
                  a_bist_din  = in(Bits(width bits))     setName("A_BIST_DIN")
                  a_bist_bm   = in(Bits(width bits))     setName("A_BIST_BM")
                }
                bb.parent.rework {
                  a_men := True
                  a_dly := True
                  if(a_bm != null) {
                    a_bm := B(width bits, default -> True)
                  }
                  // BIST disabled
                  a_bist_clk  := False
                  a_bist_en   := False
                  a_bist_men  := False
                  a_bist_wen  := False
                  a_bist_ren  := False
                  a_bist_addr := U(0, addrWidth bits)
                  a_bist_din  := B(width bits, default -> False)
                  a_bist_bm   := B(width bits, default -> False)
                }
              }
            }
          }
          case (0,1,1,0,1) => {
            macros.find(m => m.ports == 2 && m.depth == bb.topo.mem.wordCount && m.width == width) match {
              case None =>
                SpinalWarning(s"No 2P IHP SRAM macro for depth=${bb.topo.mem.wordCount} width=$width, leaving as generic")
              case Some(sram) => {
                bb.setDefinitionName(sram.name)

                val w = bb.w(0)
                w.clk  setName("A_CLK")
                w.en   setName("A_WEN")
                w.addr setName("A_ADDR")
                w.data setName("A_DIN")
                if(bb.topo.writes(0).mask != null) {
                  w.mask setName("A_BM")
                } else {
                  w.mask.removeAssignments()
                  w.mask.removeStatement()
                }

                val r = bb.rs(0)
                r.clk  setName("B_CLK")
                r.en   setName("B_REN")
                r.addr setName("B_ADDR")
                r.data setName("B_DOUT")

                var a_men, a_ren, a_dly: Bool = null
                var a_bm: Bits = null
                var b_men, b_wen, b_dly: Bool = null
                var b_din, b_bm: Bits = null
                var a_bist_clk, a_bist_en, a_bist_men, a_bist_wen, a_bist_ren: Bool = null
                var a_bist_addr: UInt = null
                var a_bist_din, a_bist_bm: Bits = null
                var b_bist_clk, b_bist_en, b_bist_men, b_bist_wen, b_bist_ren: Bool = null
                var b_bist_addr: UInt = null
                var b_bist_din, b_bist_bm: Bits = null

                bb.rework {
                  out(Bits(width bits))              setName("A_DOUT")
                  a_men       = in(Bool())           setName("A_MEN")
                  a_ren       = in(Bool())           setName("A_REN")
                  a_dly       = in(Bool())           setName("A_DLY")
                  if(bb.topo.writes(0).mask == null) {
                    a_bm      = in(Bits(width bits)) setName("A_BM")
                  }
                  b_men       = in(Bool())           setName("B_MEN")
                  b_wen       = in(Bool())           setName("B_WEN")
                  b_dly       = in(Bool())           setName("B_DLY")
                  b_din       = in(Bits(width bits)) setName("B_DIN")
                  b_bm        = in(Bits(width bits)) setName("B_BM")
                  a_bist_clk  = in(Bool())           setName("A_BIST_CLK")
                  a_bist_en   = in(Bool())           setName("A_BIST_EN")
                  a_bist_men  = in(Bool())           setName("A_BIST_MEN")
                  a_bist_wen  = in(Bool())           setName("A_BIST_WEN")
                  a_bist_ren  = in(Bool())           setName("A_BIST_REN")
                  a_bist_addr = in(UInt(addrWidth bits)) setName("A_BIST_ADDR")
                  a_bist_din  = in(Bits(width bits)) setName("A_BIST_DIN")
                  a_bist_bm   = in(Bits(width bits)) setName("A_BIST_BM")
                  b_bist_clk  = in(Bool())           setName("B_BIST_CLK")
                  b_bist_en   = in(Bool())           setName("B_BIST_EN")
                  b_bist_men  = in(Bool())           setName("B_BIST_MEN")
                  b_bist_wen  = in(Bool())           setName("B_BIST_WEN")
                  b_bist_ren  = in(Bool())           setName("B_BIST_REN")
                  b_bist_addr = in(UInt(addrWidth bits)) setName("B_BIST_ADDR")
                  b_bist_din  = in(Bits(width bits)) setName("B_BIST_DIN")
                  b_bist_bm   = in(Bits(width bits)) setName("B_BIST_BM")
                }
                bb.parent.rework {
                  // A port is write-only in this mapping
                  a_men := True
                  a_ren := False
                  a_dly := True
                  if(a_bm != null) {
                    a_bm := B(width bits, default -> True)
                  }
                  // B port is read-only in this mapping
                  b_men  := True
                  b_wen  := False
                  b_dly  := True
                  b_din  := B(width bits, default -> False)
                  b_bm   := B(width bits, default -> True)
                  // BIST disabled on both ports
                  a_bist_clk  := False
                  a_bist_en   := False
                  a_bist_men  := False
                  a_bist_wen  := False
                  a_bist_ren  := False
                  a_bist_addr := U(0, addrWidth bits)
                  a_bist_din  := B(width bits, default -> False)
                  a_bist_bm   := B(width bits, default -> False)
                  b_bist_clk  := False
                  b_bist_en   := False
                  b_bist_men  := False
                  b_bist_wen  := False
                  b_bist_ren  := False
                  b_bist_addr := U(0, addrWidth bits)
                  b_bist_din  := B(width bits, default -> False)
                  b_bist_bm   := B(width bits, default -> False)
                }
              }
            }
          }
          case _ =>
            SpinalWarning(s"Unsupported RAM topology for IHP (rw=${bb.rw.size} w=${bb.w.size} rs=${bb.rs.size} ra=${bb.ra.size} cds=${cds.size}), leaving as generic")
        }
      }
      case _ =>
    }
  }
}
