package spinal.lib.bus.amba4.axis

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axis.Axi4Stream._

object Axi4StreamWidthAdapter {
  def apply(in: Axi4Stream, out: Axi4Stream, compact: Boolean = false): Axi4StreamWidthAdapter = {
    val streamWidthAdapter = new Axi4StreamWidthAdapter(in.config, out.config, compact = compact)
    streamWidthAdapter.io.axis_s << in
    streamWidthAdapter.io.axis_m >> out
    streamWidthAdapter
  }
}

/**
 * Adapts the width of a sparse Axi4Stream. Input and output configurations should be direct assignment compatible.
 * @param inConfig The input stream configuration
 * @param outConfig The output stream configuration
 */
class Axi4StreamWidthAdapter(inConfig: Axi4StreamConfig, outConfig: Axi4StreamConfig, compact: Boolean = false) extends Component {

  val needsValid = !inConfig.useKeep || !compact

  /*
   * TODO list of possible optimizations:
   * - Convert sliding window bitvector slices into mux trees
   *    This should hint to the synthesizer to use hardware muxes such as in Xilinx parts (F7, F8) when more optimal
   * - Rework logic relying on both buffer keep bits AND numeric fill level
   *    These values are redundant to simplify logic but they must stay in perfect sync. Removing one might be more resilient?
   */

  val io = new Bundle {
    val axis_s = slave(Axi4Stream(inConfig))
    val axis_m = master(Axi4Stream(outConfig))
  }

  val MAX_SIZE = Math.max(inConfig.dataWidth*2, outConfig.dataWidth*2)

  val buffer     = Reg(Axi4StreamBundle(inConfig.copy(dataWidth = MAX_SIZE, useLast = false, useId = false, useDest = false)))
  if (inConfig.useKeep) { buffer.keep.init(B(buffer.keep.bitsRange -> False)) }
  // Store valid byte bits OR wire in keep as it's functionally the same
  val bufferValid = if (needsValid) { Reg(Bits(MAX_SIZE bit)) init(0) } else buffer.keep
  val bufferLast = RegInit(False)
  val bufferId   = inConfig.useId   generate Reg(io.axis_s.id)
  val bufferDest = inConfig.useDest generate Reg(io.axis_s.dest)
  val start = Reg(Bool()) init(True)

  // Maps inputs into buffer slices given the current fill level
  def doWriteStage(inBundle: Axi4StreamBundle, bufBundle: Axi4StreamBundle, bufValid: Bits, doWrite: Bool, fillLevel: UInt): (Axi4StreamBundle, Bits) = {
    val inDataWidth = inBundle.config.dataWidth
    val outDataWidth = outConfig.dataWidth
    val bufDataWidth = bufBundle.config.dataWidth
    val bufUserWidth = bufBundle.config.userWidth
    val bufExtDataWidth = bufDataWidth+Math.min(inDataWidth, outDataWidth)
    val bufferExt_data = Bits(8*bufExtDataWidth bit)
    val bufferExt_valid = needsValid generate { Bits(bufExtDataWidth bit) }
    val bufferExt_keep = inConfig.useKeep generate Bits(bufExtDataWidth bit)
    val bufferExt_strb = inConfig.useStrb generate Bits(bufExtDataWidth bit)
    val bufferExt_user = inConfig.useUser generate Bits(bufExtDataWidth*bufUserWidth bit)

    val invalidByte_data = B(0, 8 bit)
    val invalidByte_valid = False
    val invalidByte_keep = False
    val invalidByte_strb = inConfig.useStrb generate False
    val invalidByte_user = inConfig.useUser generate B(0, bufUserWidth bit)


    for (bufIdx <- 0 until bufExtDataWidth) {

      val thisByte_data = if (bufIdx < bufDataWidth) bufBundle.data.subdivideIn(bufDataWidth slices)(bufIdx) else invalidByte_data
      val thisByte_valid = if (bufIdx < bufDataWidth) bufValid.subdivideIn(bufDataWidth slices)(bufIdx) else invalidByte_valid
      val thisByte_keep = inConfig.useKeep generate { if (bufIdx < bufDataWidth) bufBundle.keep.subdivideIn(bufDataWidth slices)(bufIdx) else invalidByte_keep }
      val thisByte_strb = inConfig.useStrb generate { if (bufIdx < bufDataWidth) bufBundle.strb.subdivideIn(bufDataWidth slices)(bufIdx) else invalidByte_strb }
      val thisByte_user = inConfig.useUser generate { if (bufIdx < bufDataWidth) bufBundle.user.subdivideIn(bufDataWidth slices)(bufIdx) else invalidByte_user }

      val mapping = for(i <- 0 until bufExtDataWidth) yield {
        if (bufIdx-i >= inDataWidth) {
          // Use invalid byte
          inDataWidth+1
        } else if (bufIdx-i >= 0) {
          // Select input byte
          bufIdx-i
        } else {
          // Keep current byte
          inDataWidth
        }
      }
      val mappingVec = Vec(mapping.map(IntToUInt))

      val muxInput_data = invalidByte_data ## thisByte_data ## inBundle.data
      val muxInput_valid = needsValid generate { invalidByte_valid ## thisByte_valid ## B((0 until inDataWidth) -> True) }
      val muxInput_keep = inConfig.useKeep generate { invalidByte_keep ## thisByte_keep ## inBundle.keep }
      val muxInput_strb = inConfig.useStrb generate { invalidByte_strb ## thisByte_strb ## inBundle.strb }
      val muxInput_user = inConfig.useUser generate { invalidByte_user ## thisByte_user ## inBundle.user }

      val muxSelect = UInt(log2Up(inDataWidth+2) bit)
      when(doWrite) {
        muxSelect := mappingVec(fillLevel.resized).resized
      } otherwise {
        muxSelect := inBundle.config.dataWidth
      }
      bufferExt_data.subdivideIn(bufExtDataWidth slices)(bufIdx) := muxInput_data.subdivideIn(inDataWidth+2 slices)(muxSelect)
      needsValid generate { bufferExt_valid.subdivideIn(bufExtDataWidth slices)(bufIdx) := muxInput_valid.subdivideIn(inDataWidth+2 slices)(muxSelect) }
      inConfig.useKeep generate { bufferExt_keep.subdivideIn(bufExtDataWidth slices)(bufIdx) := muxInput_keep.subdivideIn(inDataWidth+2 slices)(muxSelect) }
      inConfig.useStrb generate { bufferExt_strb.subdivideIn(bufExtDataWidth slices)(bufIdx) := muxInput_strb.subdivideIn(inDataWidth+2 slices)(muxSelect) }
      inConfig.useUser generate { bufferExt_user.subdivideIn(bufExtDataWidth slices)(bufIdx) := muxInput_user.subdivideIn(inDataWidth+2 slices)(muxSelect) }
    }
    val bundle = Axi4StreamBundle(inConfig.copy(dataWidth = bufExtDataWidth, useLast = false, useId = false, useDest = false))
    bundle.data := bufferExt_data
    inConfig.useKeep generate { bundle.keep := bufferExt_keep }
    inConfig.useStrb generate { bundle.strb := bufferExt_strb }
    inConfig.useUser generate { bundle.user := bufferExt_user }

    (bundle, bufferExt_valid)
  }

  // Shifts the buffer slices down by one read size
  def doReadStage(bufBundle: Axi4StreamBundle, bufValid: Bits, readSize: Int, doRead: Bool): (Axi4StreamBundle, Bits) = {
    val bufDataWidth = bufBundle.config.dataWidth
    val bufUserWidth = bufBundle.config.userWidth
    val outBufDataWidth = bufBundle.config.dataWidth-readSize

    val buffer_data = Bits(outBufDataWidth*8 bit)
    val buffer_valid = needsValid generate { Bits(outBufDataWidth bit) }
    val buffer_keep = inConfig.useKeep generate Bits(outBufDataWidth bit)
    val buffer_strb = inConfig.useStrb generate Bits(outBufDataWidth bit)
    val buffer_user = inConfig.useUser generate Bits(outBufDataWidth*bufUserWidth bit)

    for (idx <- 0 until outBufDataWidth) {
      val readIdx = UInt(log2Up(bufDataWidth) bit)
      when(doRead) {
        readIdx := idx + readSize
      } otherwise {
        readIdx := idx
      }

      buffer_data.subdivideIn(outBufDataWidth slices)(idx) := bufBundle.data.subdivideIn(bufDataWidth slices)(readIdx)
      needsValid generate { buffer_valid.subdivideIn(outBufDataWidth slices)(idx) := bufValid.subdivideIn(bufDataWidth slices)(readIdx) }
      inConfig.useKeep generate { buffer_keep.subdivideIn(outBufDataWidth slices)(idx) := bufBundle.keep.subdivideIn(bufDataWidth slices)(readIdx) }
      inConfig.useStrb generate { buffer_strb.subdivideIn(outBufDataWidth slices)(idx) := bufBundle.strb.subdivideIn(bufDataWidth slices)(readIdx) }
      inConfig.useUser generate { buffer_user.subdivideIn(outBufDataWidth slices)(idx) := bufBundle.user.subdivideIn(bufDataWidth slices)(readIdx) }
    }

    val bundle = Axi4StreamBundle(bufBundle.config.copy(dataWidth = outBufDataWidth, useLast = false, useId = false, useDest = false))
    bundle.data := buffer_data
    inConfig.useKeep generate { bundle.keep := buffer_keep }
    inConfig.useStrb generate { bundle.strb := buffer_strb }
    inConfig.useUser generate { bundle.user := buffer_user }

    (bundle, buffer_valid)
  }

  val inCompact = if (compact) Axi4StreamSparseCompactor(io.axis_s) else io.axis_s.stage()

  // Compute next write size
  val inStage = inCompact.stage()

  // Buffer the input write size
  var writeBytes_preReg = U(0, log2Up(inConfig.dataWidth+1) bit)
  if (compact && !needsValid) {
    inCompact.keep.asBools.foreach(bit => {
      when(bit) {
        writeBytes_preReg \= writeBytes_preReg+1
      }
    })
  } else {
    when(inCompact.valid) {
      writeBytes_preReg \= inCompact.config.dataWidth
    }
  }
  val writeBytes = RegNextWhen(writeBytes_preReg, inCompact.fire)

  // Output stream signals
  val outStream = Axi4Stream(inConfig.copy(dataWidth = outConfig.dataWidth))
  outStream.valid := bufferValid(outConfig.dataWidth-1) || bufferLast
  outStream.data  := buffer.data(outConfig.dataWidth*8-1 downto 0)
  outStream.config.useKeep generate { outStream.keep := buffer.keep(outConfig.dataWidth-1 downto 0) }
  outStream.config.useStrb generate { outStream.strb := buffer.strb(outConfig.dataWidth-1 downto 0) }
  outStream.config.useUser generate { outStream.user := buffer.user(outConfig.dataWidth*outConfig.userWidth-1 downto 0) }
  inConfig.useId   generate { outStream.id := bufferId }
  inConfig.useDest generate { outStream.dest := bufferDest }
  outStream.config.useLast generate { outStream.last := bufferLast && !bufferValid(outConfig.dataWidth) }

  // Buffer update
  val fillLevel = Reg(UInt(log2Up(MAX_SIZE) bit)) init(0)
  var fillLevel_next = CombInit(fillLevel)
  when(outStream.fire) {
    fillLevel_next \= fillLevel - outConfig.dataWidth
  }
  when(inStage.fire) {
    fillLevel_next \= fillLevel_next + writeBytes
  }
  when(outStream.lastFire) {
    fillLevel_next := 0
  }
  fillLevel := fillLevel_next

  val (writeBuffer, writeBufferValid) = doWriteStage(inStage.payload, buffer, bufferValid, inStage.fire, fillLevel)
  bufferLast setWhen(inStage.lastFire)

  start clearWhen start && inStage.fire
  if (inConfig.useLast) {
    start setWhen outStream.lastFire
  } else {
    start setWhen outStream.fire && !bufferValid(0)
  }

  when(start && inStage.fire) {
    inConfig.useId generate { bufferId := inStage.id }
    inConfig.useDest generate { bufferDest := inStage.dest }
  }

  val readWriteBuffer = doReadStage(writeBuffer, writeBufferValid, outConfig.dataWidth, outStream.fire)
  bufferLast clearWhen(outStream.lastFire)

  when(inStage.fire || outStream.fire) {
    val (readBuffer, readBufferValid) = readWriteBuffer
    buffer := readBuffer
    needsValid generate { bufferValid := readBufferValid.resized }
  }

  // Input stream signals
  val canWrite = !bufferValid.reversed.apply(writeBytes)
  val canWriteWhenRead = if (inConfig.dataWidth > outConfig.dataWidth) (writeBytes <= outConfig.dataWidth) else True
  inStage.ready := !bufferLast && (canWrite || (canWriteWhenRead && outStream.fire))

  // Wire output stage
  val outBuffer = outStream.pipelined(m2s = true, s2m = true)
  io.axis_m << outBuffer

}

class Axi4StreamWidthAdapter_8_8 extends Component {
  val config = Axi4StreamConfig(dataWidth = 8, useStrb = true, useKeep = true, useLast = true)

  val io = new Bundle {
    val s_axis = slave(Axi4Stream(config))
    val m_axis = master(Axi4Stream(config))
  }

  Axi4StreamWidthAdapter(io.s_axis, io.m_axis)

  Axi4SpecRenamer(io.s_axis)
  Axi4SpecRenamer(io.m_axis)
}

object Axi4StreamWidthAdapter_8_8 {
  def main(args: Array[String]): Unit = {
    SpinalVhdl(new Axi4StreamWidthAdapter_8_8())
  }
}