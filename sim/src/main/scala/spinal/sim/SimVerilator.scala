package spinal.sim

object SimVerilator{
  final val bigInt32b = BigInt("FFFFFFFFFFFFFFFF",16)
}

class SimVerilator(backend : VerilatorBackend, 
                   handle : Long) extends SimRaw(){
  
  override def getIntMem(signal : Signal,
                      index : Long) : Int = {
    assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
    signal.dataType.raw64ToInt(backend.nativeInstance.getU64_mem(handle, 
                                                                 signal.id,
                                                                 index), signal : Signal)
  }

  def setIntMem(signal : Signal,
                 value : Int,
                 index : Long) : Unit = {
    setLongMem(signal, value, index)
  }

  override def getLongMem(signal : Signal,
                          index : Long) : Long = {
    assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
    signal.dataType.raw64ToLong(backend.nativeInstance.getU64_mem(handle, 
                                                                  signal.id, 
                                                                  index), signal : Signal)
  }
  override def setLongMem(signal : Signal, 
                          value : Long,
                          index : Long) : Unit = {
    assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
    backend.nativeInstance.setU64_mem(handle, 
                                      signal.id, 
                                      signal.dataType.longToRaw64(value, signal : Signal),
                                      index)
  }

  override def getBigIntMem(signal: Signal,
                         index : Long) = {
    if(signal.dataType.width < 64 || (signal.dataType.width == 64 && signal.dataType.isInstanceOf[SIntDataType])) {
      getLongMem(signal, index)
    } else if(signal.dataType.width == 64){
      val rawValue = backend.nativeInstance.getU64_mem(handle, 
                                                       signal.id,
                                                       index)
      if(rawValue >= 0 ) {
        BigInt(rawValue)
      }else{
        BigInt(rawValue + 1) + SimVerilator.bigInt32b
      }
    } else {
      if(signal.dataType.isInstanceOf[SIntDataType]){
        val array = new Array[Byte]((signal.dataType.width+31)/32*4)
        backend.nativeInstance.getAU8_mem(handle, 
                                          signal.id, 
                                          array,
                                          index)
        BigInt(array)
      }else{
        val array = new Array[Byte]((signal.dataType.width+31)/32*4 + 1)
        backend.nativeInstance.getAU8_mem(handle, 
                                          signal.id, 
                                          array,
                                          index)
        array(0) = 0
        BigInt(array)
      }
    }
  }

  override def setBigIntMem(signal : Signal, 
                         value : BigInt,
                         index : Long): Unit = {
    val valueBitLength = value.bitLength + (if(value.signum == -1) 1 else 0)
    if(valueBitLength <= 63) {
      setLongMem(signal, 
                 value.toLong, 
                 index)
    } else if(valueBitLength == 64 && signal.dataType.width == 64) {
      assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
      val valueLong = value.toLong
      signal.dataType.checkIs64(valueLong, signal : Signal)
      backend.nativeInstance.setU64_mem(handle, signal.id, valueLong, index)
    } else {
      signal.dataType.checkBigIntRange(value, signal)
      val array = value.toByteArray
      backend.nativeInstance.setAU8_mem(handle, 
                                        signal.id, 
                                        array, 
                                        array.length, 
                                        index)
    }
  }

  override def getInt(signal : Signal) : Int = { getIntMem(signal, 0) }
  def setInt(signal : Signal, value : Int) : Unit = { setLongMem(signal, value, 0) }
  override def getLong(signal : Signal) : Long = { getLongMem(signal, 0) }
  override def setLong(signal : Signal, value : Long) : Unit = { setLongMem(signal, value, 0) }
  override def getBigInt(signal : Signal) : BigInt = { getBigIntMem(signal, 0) }
  override def setBigInt(signal : Signal, value : BigInt) : Unit = { setBigIntMem(signal, value, 0) }

  override def eval() : Boolean = backend.nativeInstance.eval(handle)
  override def getTimePrecision(): Int = backend.nativeInstance.get_time_precision(handle)
  override def sleep(cycles : Long) = backend.nativeInstance.sleep(handle, cycles)
  override def end() = backend.nativeInstance.synchronized(backend.nativeInstance.deleteHandle(handle))
  override def isBufferedWrite : Boolean = false
  override def enableWave(): Unit = backend.nativeInstance.enableWave(handle)
  override def disableWave(): Unit =  backend.nativeInstance.disableWave(handle)
}

