package spinal.lib.bus.amba4.axis

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axis.Axi4Stream._

object Axi4StreamSimpleWidthAdapter {
  def apply(in: Axi4Stream, out: Axi4Stream): Axi4StreamSimpleWidthAdapter = {
    val streamWidthAdapter = new Axi4StreamSimpleWidthAdapter(in.config, out.config.dataWidth)
    streamWidthAdapter.io.axis_s << in
    streamWidthAdapter.io.axis_m >> out
    streamWidthAdapter
  }
}

/**
 * Adapts the width of a sparse Axi4Stream. Input and output configurations should be direct assignment compatible.
 * @param inConfig The input stream configuration
 * @param outWidth The output stream width
 */
class Axi4StreamSimpleWidthAdapter(inConfig: Axi4StreamConfig, outWidth: Int) extends Component {
  val inWidth = inConfig.dataWidth
  assert(inWidth % outWidth == 0 || inWidth % outWidth == inWidth || inConfig.useKeep, "Input and output widths must be integer multiples or must support TKEEP!")

  val outConfig = inConfig.copy(dataWidth = outWidth)

  val io = new Bundle {
    val axis_s = slave(Axi4Stream(inConfig))
    val axis_m = master(Axi4Stream(outConfig))
  }

  if (inWidth == outWidth) {
    io.axis_m << io.axis_s
  } else if (inWidth > outWidth) {
    val maxCount = (inWidth.floatValue()/outWidth).ceil.intValue()
    val padBytes = inWidth % outWidth
    val counter = Counter(maxCount, inc = io.axis_m.fire)

    io.axis_m.data := (B((0 until padBytes*8) -> False) ## io.axis_s.data)(counter*outWidth*8, outWidth*8 bit)
    inConfig.useKeep generate { io.axis_m.keep := (B((0 until padBytes) -> False) ## io.axis_s.keep)(counter*outWidth, outWidth bit) }
    inConfig.useStrb generate { io.axis_m.strb := (B((0 until padBytes) -> False) ## io.axis_s.strb)(counter*outWidth, outWidth bit) }
    inConfig.useUser generate { io.axis_m.user := (B((0 until padBytes*inConfig.userWidth) -> False) ## io.axis_s.user)(counter*outWidth*inConfig.userWidth, outWidth*inConfig.userWidth bit) }
    inConfig.useDest generate { io.axis_m.dest := io.axis_s.dest }
    inConfig.useId   generate { io.axis_m.id := io.axis_s.id }
    inConfig.useLast generate { io.axis_m.last := io.axis_s.last && counter.willOverflowIfInc }

    io.axis_s.ready := io.axis_m.ready && counter.willOverflowIfInc
    io.axis_m.valid := io.axis_s.valid
  } else {
    val maxCount = (outWidth.floatValue()/inWidth).floor.intValue()
    val padBytes = (inWidth*maxCount) % outWidth
    if (maxCount > 1) {
      val counter = Counter(maxCount+1, inc = io.axis_s.fire)

      val buffer = Reg(Axi4StreamBundle(outConfig.copy(dataWidth=(maxCount)*inWidth)))
      buffer.data init(0)
      inConfig.useKeep generate { buffer.keep init(0) }
      inConfig.useStrb generate { buffer.strb init(0) }
      inConfig.useUser generate { buffer.user init(0) }
      inConfig.useDest generate { buffer.dest init(0) }
      inConfig.useId generate { buffer.id init(0) }
      inConfig.useLast generate { buffer.last init(False) }

      val start = Reg(Bool) init(True)
      start clearWhen start && io.axis_s.fire && counter === 0
      if (inConfig.useLast) {
        start setWhen io.axis_m.lastFire
      } else {
        start setWhen io.axis_m.fire
      }

      val bufferLast = if (inConfig.useLast) buffer.last else False

      when(io.axis_s.fire) {
        buffer.data(counter*inWidth*8, inWidth*8 bit) := io.axis_s.data
        inConfig.useKeep generate { buffer.keep(counter*inWidth, inWidth bit) := io.axis_s.keep }
        inConfig.useStrb generate { buffer.strb(counter*inWidth, inWidth bit) := io.axis_s.strb }
        inConfig.useUser generate { buffer.user(counter*inWidth*inConfig.userWidth, inWidth*inConfig.userWidth bit) := io.axis_s.user }
      }

      inConfig.useLast generate {
        buffer.last setWhen io.axis_s.lastFire
        buffer.last clearWhen io.axis_m.fire
      }

      when (start && io.axis_s.fire) {
        inConfig.useDest generate { buffer.dest := io.axis_s.dest }
        inConfig.useId generate { buffer.id := io.axis_s.id }
      }

      when (io.axis_m.fire) {
        buffer.data.clearAll()
        inConfig.useStrb generate buffer.strb.clearAll()
        inConfig.useKeep generate buffer.keep.clearAll()
        inConfig.useUser generate buffer.user.clearAll()
        inConfig.useDest generate buffer.dest.clearAll()
        inConfig.useId   generate buffer.id.clearAll()
        counter.clear()
      }

      io.axis_m.data := buffer.data.resized
      inConfig.useKeep generate { io.axis_m.keep := buffer.keep.resized }
      inConfig.useStrb generate { io.axis_m.strb := buffer.strb.resized }
      inConfig.useUser generate { io.axis_m.user := buffer.user.resized }
      inConfig.useDest generate { io.axis_m.dest := buffer.dest }
      inConfig.useId   generate { io.axis_m.id := buffer.id }
      inConfig.useLast generate { io.axis_m.last := bufferLast }

      io.axis_s.ready := !counter.willOverflowIfInc && !bufferLast
      io.axis_m.valid := (counter.willOverflowIfInc || bufferLast)
    } else {
      io.axis_m << io.axis_s
    }
  }
}
