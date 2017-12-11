package spinal.sim

object SimVerilator{
  final val bigInt32b = BigInt("FFFFFFFFFFFFFFFF",16)
}

class SimVerilator(backend : VerilatorBackend, handle : Long) extends SimRaw(){
  override def getInt(signal : Signal) : Int = {
    assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
    signal.dataType.raw64ToInt(backend.native.wrapperGetU64(handle, signal.id), signal : Signal)
  }
  override def getLong(signal : Signal) : Long = {
    assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
    signal.dataType.raw64ToLong(backend.native.wrapperGetU64(handle, signal.id), signal : Signal)
  }
  override def setLong(signal : Signal, value : Long) : Unit = {
    assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
    backend.native.wrapperSetU64(handle, signal.id, signal.dataType.longToRaw64(value, signal : Signal))
  }

  override def getBigInt(signal: Signal) = {
    if(signal.dataType.width < 64 || (signal.dataType.width == 64 && signal.dataType.isInstanceOf[SIntDataType])) {
      getLong(signal)
    } else if(signal.dataType.width == 64){
      val rawValue = backend.native.wrapperGetU64(handle, signal.id)
      if(rawValue >= 0 ) {
        BigInt(rawValue)
      }else{
        BigInt(rawValue + 1) + SimVerilator.bigInt32b
      }
    } else {
      if(signal.dataType.isInstanceOf[SIntDataType]){
        val array = new Array[Byte]((signal.dataType.width+31)/32*4)
        backend.native.wrapperGetAU8(handle, signal.id, array)
        BigInt(array)
      }else{
        val array = new Array[Byte]((signal.dataType.width+31)/32*4 + 1)
        backend.native.wrapperGetAU8(handle, signal.id, array)
        array(0) = 0
        BigInt(array)
      }
    }
  }

  override def setBigInt(signal: Signal, value: BigInt): Unit = {
    val valueBitLength = value.bitLength + (if(value.signum == -1) 1 else 0)
    if(valueBitLength <= 63) {
      setLong(signal, value.toLong)
    } else if(valueBitLength == 64 && signal.dataType.width == 64) {
      assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
      val valueLong = value.toLong
      signal.dataType.checkIs64(valueLong, signal : Signal)
      backend.native.wrapperSetU64(handle, signal.id, valueLong)
    } else {
      signal.dataType.checkBigIntRange(value, signal)
      val array = value.toByteArray
      backend.native.wrapperSetAU8(handle, signal.id, array, array.length)
    }
  }

  override def eval() = backend.native.wrapperEval(handle)
  override def sleep(cycles : Long) = backend.native.wrapperSleep(handle, cycles)
  override def end() = backend.native.wrapperDeleteHandle(handle)
  override def isBufferedWrite : Boolean = false
}

