package spinal.sim


class SimVerilator(backend : VerilatorBackend, handle : Long) extends SimRaw(){
  override def getLong(signal : Signal) : Long = {
    assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
    signal.dataType.raw64ToLong(backend.native.wrapperGetU64(handle, signal.id))
  }
  override def setLong(signal : Signal, value : Long) : Unit = {
    assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
    backend.native.wrapperSetU64(handle, signal.id, signal.dataType.longToRaw64(value))
  }
  override def eval() = backend.native.wrapperEval(handle)
  override def sleep(cycles : Long) = backend.native.wrapperSleep(handle, cycles)
  override def end() = backend.native.wrapperDeleteHandle(handle)
  override def isBufferedWrite : Boolean = false
}

