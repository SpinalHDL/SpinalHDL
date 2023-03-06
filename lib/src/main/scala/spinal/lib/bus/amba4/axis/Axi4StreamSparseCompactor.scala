package spinal.lib.bus.amba4.axis

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axis.Axi4Stream.{Axi4Stream, Axi4StreamBundle}

object Axi4StreamSparseCompactor {
  def apply(stream: Axi4Stream): Axi4Stream = {
    val compactor = new Axi4StreamSparseCompactor(stream.config)
    compactor.io.axis_s << stream
    compactor.io.axis_m
  }
}

class Axi4StreamSparseCompactor(config: Axi4StreamConfig) extends Component {
  val io = new Bundle {
    val axis_s = slave(Axi4Stream(config))
    val axis_m = master(Axi4Stream(config))
  }

  val inStage = io.axis_s.pipelined(m2s = true, s2m = true, halfRate = false)

  val invalidByte_data = B(0, 8 bit)
  val invalidByte_keep = False
  val invalidByte_strb = config.useStrb generate False
  val invalidByte_user = config.useUser generate B(0, config.userWidth bit)

  val outStage = inStage.map(compactAxisBundle(_)).pipelined(m2s = true, s2m = true, halfRate = false)

  io.axis_m << outStage

  /**
    * Generates all possible permutations of N bits which fit within the width
    *
    * For example: walkLowBits(4, 2) would generate:
    *     11
    *    101
    *    110
    *   1001
    *   1010
    *   1100
    * @param width Maximum bit width
    * @param n Number of set bits
    * @return A sequence of BigInts representing each permutation
    */
  def walkLowBits(width: Int, n: Int): Seq[BigInt] = {
    // Early exit case, preserves map recursion
    if (width <= 0 || n <= 0) {
      Seq(0)
    } else {
      // Work up the width as to make lower permutations first
      (n - 1 until width).flatMap { i =>
        walkLowBits(i, n - 1).map { o =>
          (BigInt(1) << i) + o
        }
      }
    }
  }

  /**
    * Calculates the position of the Nth set bit in the bit set
    * @param bits Bits
    * @param n Lowest set bit to search for, 0 indexed
    * @return Position of the Nth set bit or widthOf(bits) if not found
    */
  def indexOfBoolN(bits: Bits, n: Int): UInt = {
    val index = U(widthOf(bits), log2Up(widthOf(bits)+1) bit)

    for(matchInt <- walkLowBits(widthOf(bits), n+1)) {
      val matchBits = B(matchInt)
      when(bits.takeLow(widthOf(matchBits)) === matchBits) {
        index := U(widthOf(matchBits)-1)
      }
    }

    index
  }

  def compactAxisBundle(bundle: Axi4StreamBundle): Axi4StreamBundle = {
    val outBundle = bundle.clone
    val dataWidth = bundle.config.dataWidth

    for (idx <- 0 until dataWidth) {
      val index = indexOfBoolN(bundle.keep, idx)

      val mux_data = invalidByte_data ## bundle.data
      val mux_keep = invalidByte_keep ## bundle.keep
      val mux_strb = bundle.config.useStrb generate { invalidByte_strb ## bundle.strb }
      val mux_user = bundle.config.useUser generate { invalidByte_user ## bundle.user }

      outBundle.data.subdivideIn(dataWidth slices)(idx) := mux_data.subdivideIn(dataWidth+1 slices)(index)
      outBundle.keep.subdivideIn(dataWidth slices)(idx) := mux_keep.subdivideIn(dataWidth+1 slices)(index)
      bundle.config.useStrb generate { outBundle.strb.subdivideIn(dataWidth slices)(idx) := mux_strb.subdivideIn(dataWidth+1 slices)(index) }
      bundle.config.useUser generate { outBundle.user.subdivideIn(dataWidth slices)(idx) := mux_user.subdivideIn(dataWidth+1 slices)(index) }
    }

    bundle.config.useId generate { outBundle.id := bundle.id }
    bundle.config.useDest generate { outBundle.dest := bundle.dest }
    bundle.config.useLast generate { outBundle.last := bundle.last }

    outBundle
  }
}
