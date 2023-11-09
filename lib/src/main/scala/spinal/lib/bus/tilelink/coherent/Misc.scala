package spinal.lib.bus.tilelink.coherent

import spinal.core._
import spinal.lib._
import spinal.lib.bus.tilelink._
import spinal.lib.pipeline._


case class DataPayload(bytes : Int) extends Bundle {
  val mask = Bits(bytes bits)
  val data = Bits(bytes*8 bits)
}

class ChannelDataBuffer(entries: Int,
                        blockSize: Int,
                        dataBytes : Int) extends Area {
  val ram = Mem.fill(entries * blockSize / dataBytes)(DataPayload(dataBytes))
  val allocated = Reg(Bits(entries bits)) init (0)
  val set, clear = B(0, entries bits)
  val firstFree = OHToUInt(OHMasking.firstV2(~allocated))
  val full = allocated.andR
  val write = ram.writePort()
  allocated := (allocated | set) & ~clear

  def push(stage : Stage,
           CMD : Stageable[ChannelA],
           PAYLOAD : Stageable[DataPayload],
           LAST : Stageable[Bool]) = new Area {
    import stage._
    val withBeats = CMD.withBeats
    val hazard = withBeats && full
    haltIt(hazard)
    throwIt(!hazard && !LAST)


    for (i <- log2Up(dataBytes) until log2Up(blockSize)) CMD.addressNull(i) clearWhen (i < CMD.size)

    val locked = RegInit(False) setWhen (write.valid)
    val lockedValue = RegNextWhen(firstFree, !locked)
    val BUFFER_ID = insert(locked ? lockedValue | firstFree)
    write.valid := valid && withBeats && !hazard
    write.address := BUFFER_ID @@ CMD.address(log2Up(blockSize)-1 downto log2Up(dataBytes))
    write.data := PAYLOAD
    when(isFireing && LAST && withBeats) {
      set(BUFFER_ID) := True
      locked := False
    }
  }

  def push[T <: BusFragment](up : Stream[T]) = new Area {
    val withBeats = up.withBeats
    val hazard = withBeats && full
    val dataLess = up.translateWith(up.asNoData())
    val filtred = dataLess.throwWhen(!hazard && !up.isLast())
    val down = filtred.haltWhen(hazard)
    val locked = RegInit(False) setWhen (write.valid)
    val lockedValue = RegNextWhen(firstFree, !locked)
    val bufferId = locked ? lockedValue | firstFree

    for(i <- log2Up(dataBytes) until log2Up(blockSize)) down.addressNull(i) clearWhen(i < down.size)

    write.valid := up.valid && withBeats && !hazard
    write.address := bufferId @@ up.addressNull(log2Up(blockSize) - 1 downto log2Up(dataBytes))
    write.data.mask := up.maskNull
    write.data.data := up.data
    when(down.fire && up.isLast() && withBeats) {
      set(bufferId) := True
      locked := False
    }
  }
}


