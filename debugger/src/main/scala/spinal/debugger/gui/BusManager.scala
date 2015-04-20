package spinal.debugger.gui


class BusManager(hal : BytePacketHal,guiTreeViewManager: IGuiTreeViewManager) extends IBytePacketHalObserver{
  implicit def b(x: Int) = x.toByte

  
  hal.setObserver(this)
  hal.open



  passportCall



  def passportCall: Unit = {
    hal.tx(Seq(0xFF, 0xFF, 0xFF, 0xFF))
  }
  override def packetHalEvent(in: IndexedSeq[Byte]): Unit = {
    guiTreeViewManager.add(Seq(in.mkString(" ")))
  }
}
