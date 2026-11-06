package spinal.lib.ethernet

import spinal.core._

import UserConfiguration._

object rotateLeftByte {
  def apply(data: Bits, bias: UInt): Bits = {
    val result = cloneOf(data)
    val byteNum: Int = data.getWidth / BYTE_WIDTH
    switch(bias) {
      for (idx <- 0 until byteNum) {
        is(idx) {
          result := data.takeLow((byteNum - idx) * 8) ## data.takeHigh(idx * 8)
        }
      }
    }
    result
  }
}

object rotateLeftBit {
  def apply(data: Bits, bias: UInt): Bits = {
    val result = cloneOf(data)
    val bitWidth = data.getWidth
    switch(bias) {
      for (idx <- 0 until bitWidth) {
        is(idx) {
          result := data.takeLow(bitWidth - idx) ## data.takeHigh(idx)
        }
      }
    }
    result
  }
}

object byteMaskData {
  def apply(byteMask: Bits, data: Bits): Bits = {
    val dataWidth = DATA_BYTE_CNT
    val maskWidth = byteMask.getWidth
    val sliceWidth = data.getWidth / dataWidth
    require(
      maskWidth == dataWidth,
      s"ByteMaskData maskWidth${maskWidth} != dataWidth${dataWidth}"
    )
    val spiltAsSlices = data.subdivideIn(maskWidth slices)
    val arrMaskedByte = Array.tabulate(spiltAsSlices.length) { idx =>
      byteMask(idx) ? B(0, sliceWidth bits) | spiltAsSlices(idx)
    }
    val maskedData = arrMaskedByte.reverse.reduceLeft(_ ## _)
    maskedData
  }
}

object generateByteMask {
  def apply(len: UInt): Bits = {
    val res = Bits(DATA_BYTE_CNT bits)
    switch(len) {
      for (idx <- 0 until DATA_BYTE_CNT) {
        if (idx == 0) {
          is(idx) {
            res := Bits(DATA_BYTE_CNT bits).setAll()
          }
        } else {
          is(idx) {
            res := B(
              DATA_BYTE_CNT bits,
              (DATA_BYTE_CNT - 1 downto DATA_BYTE_CNT - idx) -> true,
              default -> false
            )
          }
        }
      }
    }
    res

  }
}
