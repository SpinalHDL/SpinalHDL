package spinal.lib.misc

import java.io.FileInputStream
import spinal.core.{Data, Mem}

object BinTools {
  def initRam[T <: Data](ram: Mem[T], path: String, swapEndianness: Boolean = false): Unit ={
    val initContent = Array.fill[BigInt](ram.wordCount)(0)
    val readTmp = Array.fill[Byte](ram.width / 8)(0)
    val initFile = new FileInputStream(path)
    for ((e, i) <- initContent.zipWithIndex)
      if (initFile.read(readTmp) > 0)
        /* read() stores the data in reserved order */
        initContent(i) = BigInt(1, if (swapEndianness) readTmp else readTmp.reverse)
    ram.initBigInt(initContent)
  }
}
