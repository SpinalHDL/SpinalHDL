package spinal.lib.misc

import spinal.core.{Data, Mem}

object HexTools{
  def readHexFile(path : String, hexOffset : Long, callback : (Long, Int) => Unit) : Unit ={
    import scala.io.Source
    def hToI(that : String, start : Int, size : Int) = Integer.parseInt(that.substring(start,start + size), 16)
    def hToL(that : String, start : Int, size : Int):Long = java.lang.Long.parseLong(that.substring(start,start + size), 16)

    var offset:Long = 0
    for (line <- Source.fromFile(path).getLines) {
      if (line.charAt(0) == ':'){
        val key = hToI(line, 7, 2)
        key match {
          case 0 =>
            val byteCount = hToI(line, 1, 2)
            val nextAddr: Long = hToL(line, 3, 4) + offset - hexOffset
            for(i <- 0 until byteCount){
              callback(nextAddr + i , hToI(line, 9 + i * 2, 2))
            }
          case 2 =>
            offset = hToL(line, 9, 4) << 4
          case 4 =>
            offset = hToL(line, 9, 4) << 16
          case 3 =>
          case 5 =>
          case 1 =>
        }
      }
    }
  }

  def readHexFile(path : String, hexOffset : Long): Array[BigInt] ={
    var onChipRomSize = 0
    readHexFile(path, hexOffset ,(address, _) => {
      assert(address <= Int.MaxValue, s"Address exceeds the Max value of Int, please check the hexOffset parameter.")
      onChipRomSize = Math.max((address).toInt, onChipRomSize) + 1
    })

    val initContent = Array.fill[BigInt]((onChipRomSize+3)/4)(0)
    readHexFile(path, hexOffset,(address,data) => {
      val addressWithoutOffset = (address).toInt
      if(addressWithoutOffset < onChipRomSize)
        initContent(addressWithoutOffset >> 2) |= BigInt(data) << ((addressWithoutOffset & 3)*8)
    })
    initContent
  }

  def initRam[T <: Data](ram : Mem[T], onChipRamHexFile : String, hexOffset : BigInt): Unit ={
    val wordSize = ram.wordType.getBitsWidth/8
    val initContent = Array.fill[BigInt](ram.wordCount)(0)
    HexTools.readHexFile(onChipRamHexFile, 0,(address,data) => {
      val addressWithoutOffset = (address - hexOffset).toInt
      initContent(addressWithoutOffset/wordSize) |= BigInt(data) << ((addressWithoutOffset % wordSize)*8)
    })
    ram.initBigInt(initContent)
  }
}
