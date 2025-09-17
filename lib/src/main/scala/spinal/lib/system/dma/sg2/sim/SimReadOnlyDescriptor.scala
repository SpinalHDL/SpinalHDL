package spinal.lib.system.dma.sg2.sim

import spinal.lib.system.dma.sg2.DmaSgReadOnly
import spinal.core._
import spinal.lib.sim.SparseMemory

import java.nio.{ByteBuffer, ByteOrder}

class SimReadOnlyDescriptor(var physicalAddress : Long = 0l) {
  var next, from = 0l
  var controlBytes = 0
  var statusCompleted = false
  var controlLast = false
  var controlIrq = false


  def read(mem : SparseMemory) : Unit = read(mem.readBytes(physicalAddress, 32))
  def read(array : Array[Byte]): Unit = {
    import DmaSgReadOnly._
    val wrapped = ByteBuffer.wrap(array)
    wrapped.order(ByteOrder.LITTLE_ENDIAN);
    val status = wrapped.getInt(statusAt)
    val control = wrapped.getInt(controlAt)
    statusCompleted = (status & (1 << statusCompletedAt)) != 0
    controlLast = (control & (1 << controlLastAt)) != 0
    controlIrq = (control & (1 << controlIrqAt)) != 0
    controlBytes = control & 0x7FFFFFFF
    next = wrapped.getLong(nextAt)
    from = wrapped.getLong(fromAt)
  }

  def write(mem : SparseMemory): Unit = {
    val array = Array.fill[Byte](32)(0)
    write(array)
    mem.write(physicalAddress, array)
  }
  def write(array : Array[Byte]): Unit = {
    import DmaSgReadOnly._
    val wrapped = ByteBuffer.wrap(array)
    wrapped.order(ByteOrder.LITTLE_ENDIAN);
    val status = statusCompleted.toInt << statusCompletedAt
    val control = (controlLast.toInt << controlLastAt) | (controlIrq.toInt << controlIrqAt) | controlBytes.toInt
    wrapped.putInt(statusAt, status)
    wrapped.putInt(controlAt, control)
    wrapped.putLong(nextAt, next)
    wrapped.putLong(fromAt, from)
  }
}

