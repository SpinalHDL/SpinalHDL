/* This 8b10b encoding was originally written by D.W.Pegler in Verilog and ported to SpinalHDL.
 *
 * Source: https://opencores.org/projects/1000base-x
 *
 * Author(s):
 *   - D.W.Pegler Cambridge Broadband Networks Ltd
 *     { peglerd@gmail.com, dwp@cambridgebroadand.com }
 *
 * Notes from the original file header:
 *
 *   This module is based on the coding method described in
 *   IEEE Std 802.3-2008 Section 36.2.4 which is available from:
 *
 *   http//:standards.ieee.org/about/get/802/802.3.html
 *
 *   and the 8B/10B coding scheme from the 1993 IBM publication
 *   "DC-Balanced, Partitioned-Block, 8B/10B Transmission Code"
 *   by A.X. Widmer and P.A. Franasze" see doc/01-581v1.pdf
 *
 *   and US patent #4,486,739 "BYTE ORIENTED DC BALANCED
 *   (0,4) 8B/10B PARTITIONED BLOCK TRANSMISSION CODE "; see:
 *
 *   doc/US4486739.pdf
 *
 *   http://en.wikipedia.org/wiki/8b/10b_encoding
 */

package spinal.lib.com.linecode

import spinal.core._
import spinal.lib._

object Encoding8b10b {

  /*
   * 8b10b encoder based on [1].
   *
   * data [7:0] is ordered as HGFEDCBA.
   * kWord enables the K word encoding.
   * encoded [9:0] is ordered as abcdeifghj.
   * kError will signal whether the control sequence exists.
   *
   * 1: https://github.com/freecores/1000base-x/blob/master/rtl/verilog/encoder_8b10b.v
   */
  case class Encoder() extends Component {
    val io = new Bundle {
      val data = in(Bits(8 bits))
      val kWord = in(Bool())
      val stall = in(Bool())
      val encoded = out(Bits(10 bits))
      val kError = out(Bool())
    }

    val A = io.data(0)
    val B = io.data(1)
    val C = io.data(2)
    val D = io.data(3)
    val E = io.data(4)
    val F = io.data(5)
    val G = io.data(6)
    val H = io.data(7)

    val disparity = Reg(Bool()).init(False)

    val lFunction = new Area {
      val aEqB = A === B
      val cEqD = C === D

      val l40 = A & B & C & D
      val l04 = !A & !B & !C & !D

      val l13 = (!aEqB & !C & !D) | (!cEqD & !A & !B)
      val l31 = (!aEqB & C & D) | (!cEqD & A & B)
      val l22 = (A & B & !C & !D) | (C & D & !A & !B) | (!aEqB & !cEqD)
    }

    val disparityClassification = new Area {
      val pdAnd1s6 = (lFunction.l13 & D & E) | (!E & !lFunction.l22 & !lFunction.l31)
      val ndAnd1s6 = io.kWord | (E & !lFunction.l22 & !lFunction.l13) | (!E & !D & lFunction.l31)

      val pdos6 = io.kWord | (E & !lFunction.l22 & !lFunction.l13)
      val ndos6 = pdAnd1s6
      val dos6 = pdos6 | ndos6

      val pdAnd1s4 = (!F & !G) | (io.kWord & ((F & !G) | (!F & G)))
      val ndAnd1s4 = F & G

      val pdos4 = F & G & H
      val ndos4 = (!F & !G)
      val dos4 = pdos4 | ndos4
    }

    val complementation = new Area {
      val compLs4 = Reg(Bool()).init(False)
      val compLs6 = Reg(Bool()).init(False)

      val illegalK = io.kWord & (A | B | !C | !D | !E) & (!F | !G | !H | !E | !lFunction.l31)

      val disparity6 = disparity ^ disparityClassification.dos6

      when(!io.stall) {
        compLs4 := (disparityClassification.pdAnd1s4 & !disparity6) |
          (disparityClassification.ndAnd1s4 & disparity6)

        compLs6 := (disparityClassification.pdAnd1s6 & !disparity) |
          (disparityClassification.ndAnd1s6 & disparity)
      }
    }

    val encoding5b6b = new Area {

      val stageA = RegNextWhen(A, !io.stall).init(False)
      val stageB0 = RegNextWhen(B & !lFunction.l40, !io.stall).init(False)
      val stageB1 = RegNextWhen(lFunction.l04, !io.stall).init(False)
      val stageC0 = RegNextWhen(lFunction.l04 | C, !io.stall).init(False)
      val stageC1 = RegNextWhen(E & D & !C & !B & !A, !io.stall).init(False)
      val stageD = RegNextWhen(D & !(A & B & C), !io.stall).init(False)
      val stageE0 = RegNextWhen(E | lFunction.l13, !io.stall).init(False)
      val stageE1 = RegNextWhen(!(E & D & !C & !B & !A), !io.stall).init(False)
      val stageI0 = RegNextWhen((lFunction.l22 & !E) | (E & lFunction.l40), !io.stall).init(False)
      val stageI1 = RegNextWhen(E & !D & !C & !(A & B), !io.stall).init(False)
      val stageI2 = RegNextWhen(io.kWord & E & D & C & !B & !A, !io.stall).init(False)
      val stageI3 = RegNextWhen(E & !D & C & !B & !A, !io.stall).init(False)

      val a = stageA
      val b = stageB0 | stageB1
      val c = stageC0 | stageC1
      val d = stageD
      val e = stageE0 & stageE1
      val i = stageI0 | stageI1 | stageI2 | stageI3
    }

    val encoding3b4b = new Area {
      val alt7 = Reg(Bool()).init(False)

      when(!io.stall) {
        when(disparity) {
          alt7 := F & G & H & (io.kWord | (!E & D & lFunction.l31))
        } otherwise {
          alt7 := F & G & H & (io.kWord | (E & !D & lFunction.l13))
        }
      }

      val stageF = RegNextWhen(F, !io.stall).init(False)
      val stageG = RegNextWhen(G | (!F & !G & !H), !io.stall).init(False)
      val stageH = RegNextWhen(H, !io.stall).init(False)
      val stageJ = RegNextWhen(!H & (G ^ F), !io.stall).init(False)

      val f = stageF & !alt7
      val g = stageG
      val h = stageH
      val j = stageJ | alt7
    }

    val outputStage = new Area {
      val encoded = Reg(Bits(10 bits)).init(B"0000000000")
      val kError0 = RegNextWhen(complementation.illegalK, !io.stall).init(False)
      val kError = RegNextWhen(kError0, !io.stall)

      when(!io.stall) {
        disparity := disparityClassification.dos4 ^ complementation.disparity6

        encoded(9) := (encoding5b6b.a ^ complementation.compLs6)
        encoded(8) := (encoding5b6b.b ^ complementation.compLs6)
        encoded(7) := (encoding5b6b.c ^ complementation.compLs6)
        encoded(6) := (encoding5b6b.d ^ complementation.compLs6)
        encoded(5) := (encoding5b6b.e ^ complementation.compLs6)
        encoded(4) := (encoding5b6b.i ^ complementation.compLs6)
        encoded(3) := (encoding3b4b.f ^ complementation.compLs4)
        encoded(2) := (encoding3b4b.g ^ complementation.compLs4)
        encoded(1) := (encoding3b4b.h ^ complementation.compLs4)
        encoded(0) := (encoding3b4b.j ^ complementation.compLs4)
      }
    }

    io.kError := outputStage.kError
    io.encoded := outputStage.encoded
  }

  /*
   * 8b10b decoder based on [1].
   *
   * encoded [9:0] is ordered as abcdeifghj
   * data [7:0] is ordered as HGFEDCBA.
   * kWord is asserted when a K word was received.
   * codeError is asserted when either a coding, "extra coding" or disparity error occurred.
   *
   * 1: https://github.com/freecores/1000base-x/blob/master/rtl/verilog/decoder_8b10b.v
   */

  case class Decoder() extends Component {
    val io = new Bundle {
      val encoded = in Bits (10 bits)
      val stall = in(Bool())
      val data = out Bits (8 bits)
      val kWord = out Bool ()
      val codeError = out Bool ()
    }

    val j = io.encoded(0)
    val h = io.encoded(1)
    val g = io.encoded(2)
    val f = io.encoded(3)
    val i = io.encoded(4)
    val e = io.encoded(5)
    val d = io.encoded(6)
    val c = io.encoded(7)
    val b = io.encoded(8)
    val a = io.encoded(9)

    val disparity = Reg(Bool()).init(False)

    val decoder6b = new Area {
      val aEqB = (a & b) | (!a & !b)
      val cEqD = (c & d) | (!c & !d)
      val p22 = (a & b & !c & !d) | (c & d & !a & !b) | (!aEqB & !cEqD)
      val p13 = (!aEqB & !c & !d) | (!cEqD & !a & !b)
      val p31 = (!aEqB & c & d) | (!cEqD & a & b)
    }

    val decoderK = new Area {
      val eEqI = (e === i)
      val p22AndAAndCAndEEqI = decoder6b.p22 & a & c & eEqI
      val p22AndNotAAndNotCAndEEqI = decoder6b.p22 & !a & !c & eEqI

      val notCAndNotDAndNotEAndNotI = (!c & !d & !e & !i)
      val cAndDAndEAndI = (c & d & e & i)

      val kA = cAndDAndEAndI | notCAndNotDAndNotEAndNotI
      val kB = decoder6b.p13 & (!e & i & g & h & j)
      val kC = decoder6b.p31 & (e & !i & !g & !h & !j)

      val k = kA | kB | kC

      val p22AndBAndCAndEEqI = decoder6b.p22 & b & c & eEqI
      val p22AndNotBAndNotCAndEEqI = decoder6b.p22 & !b & !c & eEqI
      val notAAndNotBAndNotEandNotI = !a & !b & !e & !i
      val aAndBAndEAndI = a & b & e & i
      val p13AndDAndEAndI = decoder6b.p13 & d & e & i
      val p13AndNotI = decoder6b.p13 & !i
      val p13AndNotE = decoder6b.p13 & !e
      val p31AndI = decoder6b.p31 & i
    }

    val decoder6b5b = new Area {
      val or121 = decoderK.p22AndNotAAndNotCAndEEqI | decoderK.p13AndNotE
      val or122 = decoderK.aAndBAndEAndI | decoderK.notCAndNotDAndNotEAndNotI | decoderK.p31AndI
      val or123 = decoderK.p31AndI | decoderK.p22AndBAndCAndEEqI | decoderK.p13AndDAndEAndI
      val or124 = decoderK.p22AndAAndCAndEEqI | decoderK.p13AndNotE
      val or125 = decoderK.p13AndNotE | decoderK.notCAndNotDAndNotEAndNotI |
        decoderK.notAAndNotBAndNotEandNotI
      val or126 = decoderK.p22AndNotAAndNotCAndEEqI | decoderK.p13AndNotI
      val or127 = decoderK.p13AndDAndEAndI | decoderK.p22AndNotBAndNotCAndEEqI

      val decodedA = a ^ (or127 | or121 | or122)
      val decodedB = b ^ (or122 | or123 | or124)
      val decodedC = c ^ (or121 | or123 | or125)
      val decodedD = d ^ (or122 | or124 | or127)
      val decodedE = e ^ (or125 | or126 | or127)

      val decoded = decodedE ## decodedD ## decodedC ## decodedB ## decodedA
    }

    val decoder3b4b = new Area {
      val k28p = !(c | d | e | i)

      val decodedF = ((j & !f & (h | !g | k28p)) | (f & !j & (!h | g | !k28p)) | (k28p & g & h) |
        (!k28p & !g & !h))
      val decodedG = ((j & !f & (h | !g | !k28p)) | (f & !j & (!h | g | k28p)) | (!k28p & g & h) |
        (k28p & !g & !h))
      val decodedH = (((j ^ h) & !((!f & g & !h & j & !k28p) | (!f & g & h & !j & k28p) |
        (f & !g & !h & j & !k28p) | (f & !g & h & !j & k28p))) | (!f & g & h & j) |
        (f & !g & !h & !j))

      val decoded = decodedH ## decodedG ## decodedF
    }

    val disparityError = new Area {
      val feqg = (f & g) | (!f & !g)
      val heqj = (h & j) | (!h & !j)

      val fghjP13 = (!feqg & !h & !j) | (!heqj & !f & !g)
      val fghjP31 = ((!feqg) & h & j) | (!heqj & f & g)
      val fghj22 = (f & g & !h & !j) | (!f & !g & h & j) | (!feqg & !heqj)

      val disparity6p = (decoder6b.p31 & (e | i)) | (decoder6b.p22 & e & i)
      val disparity6n = (decoder6b.p13 & !(e & i)) | (decoder6b.p22 & !e & !i)

      val disparity4p = fghjP31
      val disparity4n = fghjP13

      val disparity6a = decoder6b.p31 | (decoder6b.p22 & disparity)
      val disparity6a2 = decoder6b.p31 & disparity
      val disparity6a0 = decoder6b.p13 & !disparity

      val disparity6b = (e & i & !disparity6a0) | (disparity6a & (e | i)) | disparity6a2

      when(!io.stall) {
        disparity := fghjP31 | (disparity6b & fghj22)
      }

      val error1 = (disparity & disparity6p) | (disparity6n & !disparity)
      val error2 = (disparity & !disparity6n & f & g)
      val error3 = (disparity & a & b & c)
      val error4 = (disparity & !disparity6n & disparity4p)
      val error5 = (!disparity & !disparity6p & !f & !g)
      val error6 = (!disparity & !a & !b & !c)
      val error7 = (!disparity & !disparity6p & disparity4n)
      val error8 = (disparity6p & disparity4p) | (disparity6n & disparity4n)

      val error12 = RegNextWhen(error1 | error2, !io.stall).init(False)
      val error34 = RegNextWhen(error3 | error4, !io.stall).init(False)
      val error56 = RegNextWhen(error5 | error6, !io.stall).init(False)
      val error78 = RegNextWhen(error7 | error8, !io.stall).init(False)

      val error = error12 | error34 | error56 | error78
    }

    val codingError = new Area {
      val error1 = (a & b & c & d) | (!a & !b & !c & !d)
      val error2 = (decoder6b.p13 & !e & !i)
      val error3 = (decoder6b.p31 & e & i)
      val error4 = (f & g & h & j) | (!f & !g & !h & !j)
      val error5 = (e & i & f & g & h) | (!e & !i & !f & !g & !h)
      val error6 = (e & !i & g & h & j) | (!e & i & !g & !h & !j)
      val error7 = (((e & i & !g & !h & !j) | (!e & !i & g & h & j)) & !((c & d & e) |
        (!c & !d & !e)))
      val error8 = (!decoder6b.p31 & e & !i & !g & !h & !j)
      val error9 = (!decoder6b.p13 & !e & i & g & h & j)

      val error = RegNextWhen(
        error1 | error2 | error3 | error4 | error5 | error6 | error7 |
          error8 | error9,
        !io.stall
      ).init(False)
    }

    val extraCodingError = new Area {
      val error1 = (a & b & c & !e & !i & ((!f & !g) | disparityError.fghjP13))
      val error2 = (!a & !b & !c & e & i & ((f & g) | disparityError.fghjP31))
      val error3 = (c & d & e & i & !f & !g & !h)
      val error4 = (!c & !d & !e & !i & f & g & h)

      val error = RegNextWhen(error1 | error2 | error3 | error4, !io.stall).init(False)
    }

    val outputStage = new Area {
      val decoded = Reg(Bits(8 bits)).init(B"00000000")
      val kWord = Reg(Bool()).init(False)

      when(!io.stall) {
        decoded := decoder3b4b.decoded ## decoder6b5b.decoded
        kWord := decoderK.k
      }
    }

    io.data := outputStage.decoded
    io.kWord := outputStage.kWord
    io.codeError := disparityError.error | codingError.error | extraCodingError.error
  }
}
