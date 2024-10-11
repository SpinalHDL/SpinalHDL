package spinal.lib.system.dma.sg2.sim

import spinal.core._
import spinal.lib.sim.SparseMemory
import spinal.lib.system.dma.sg2.DmaSgWriteOnly

import java.nio.{ByteBuffer, ByteOrder}

class SimWriteOnlyDescriptor(var physicalAddress : Long = 0l) {
  var next, to = 0l
  var controlBytes = 0
  var controlIrqAll = false
  var controlIrqLast = false
  var statusCompleted = false
  var statusLast = false
  var statusBytes = 0


  def read(mem : SparseMemory) : Unit = read(mem.readBytes(physicalAddress, 32))
  def read(array : Array[Byte]): Unit = {
    import DmaSgWriteOnly._
    val wrapped = ByteBuffer.wrap(array)
    wrapped.order(ByteOrder.LITTLE_ENDIAN);
    val status = wrapped.getInt(statusAt)
    val control = wrapped.getInt(controlAt)
    statusCompleted = (status & (1 << statusCompletedAt)) != 0
    statusBytes = (status >> statusBytesAt) & 0x7FFFFFF
    statusLast = (status & (1 << statusLastAt)) != 0
    controlBytes = control & 0x7FFFFFF
    controlIrqLast = (control & (1 << controlIrqLastAt)) != 0
    controlIrqAll = (control & (1 << controlIrqAllAt)) != 0
    next = wrapped.getLong(nextAt)
    to = wrapped.getLong(toAt)
  }

  def write(mem : SparseMemory): Unit = {
    val array = Array.fill[Byte](32)(0)
    write(array)
    mem.write(physicalAddress, array)
  }
  def write(array : Array[Byte]): Unit = {
    import DmaSgWriteOnly._
    val wrapped = ByteBuffer.wrap(array)
    wrapped.order(ByteOrder.LITTLE_ENDIAN);
    val status = (statusLast.toInt << statusLastAt) |  (statusBytes << statusBytesAt) | (statusCompleted.toInt << statusCompletedAt)
    val control = controlBytes.toInt | (controlIrqAll.toInt << controlIrqAllAt) | (controlIrqLast.toInt << controlIrqLastAt)
    wrapped.putInt(statusAt, status)
    wrapped.putInt(controlAt, control)
    wrapped.putLong(nextAt, next)
    wrapped.putLong(toAt, to)
  }
}

