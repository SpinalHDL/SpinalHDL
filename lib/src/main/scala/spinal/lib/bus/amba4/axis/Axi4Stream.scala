package spinal.lib.bus.amba4.axis

import spinal.core._
import spinal.lib._

case class Axi4StreamConfig(dataBytes: Int,
                            idWidth:   Int = -1,
                            destWidth: Int = -1,
                            userWidth: Int = -1,
                            useStrb:   Boolean = false,
                            useKeep:   Boolean = false,
                            useLast:   Boolean = false,
                            useId:     Boolean = false,
                            useDest:   Boolean = false,
                            useUser:   Boolean = false)

case class Axi4StreamBundle(val config: Axi4StreamConfig) extends Bundle {
  val tdata = Bits(8*config.dataBytes bit)
  val tid =   (config.useId)   generate UInt(config.idWidth bit)
  val tstrb = (config.useStrb) generate Bits(config.dataBytes bit)
  val tkeep = (config.useKeep) generate Bits(config.dataBytes bit)
  val tlast = (config.useLast) generate Bool()
  val tdest = (config.useDest) generate UInt(config.destWidth bit)
  val tuser = (config.useUser) generate Bits(config.userWidth*config.dataBytes bit)

  def isLast = if (this.tlast != null) this.tlast else False

  override def clone: Axi4StreamBundle = Axi4StreamBundle(config)
}

class Axi4Stream(val config: Axi4StreamConfig) extends Stream[Axi4StreamBundle](Axi4StreamBundle(config)) {
  override def connectFrom(that: Stream[Axi4StreamBundle]): Stream[Axi4StreamBundle] = Axi4StreamPriv.connectFrom(that, this)

  def toStream[T <: Data](newType: HardType[T], alignment: BitAlignment = LSB): Stream[TupleBundle4[T, UInt, UInt, Bits]] = {
    val that = Stream(TupleBundle4(newType(),
      if (this.tid != null) this.tid else UInt(0 bit),
      if (this.tdest != null) this.tdest else UInt(0 bit),
      if (this.tuser != null) this.tuser else Bits(0 bit)
    ))
    assert(!config.useStrb, s"Can't directly convert Axi4Stream $this to Stream of ${that.payloadType} because the Axi4Stream supports TSTRB.")
    assert(config.dataBytes == Math.ceil(that.payload.getBitsWidth/8.0).toInt, s"Can't directly convert Axi4Stream $this (${this.config.dataBytes} bytes) to Stream of ${that.payloadType} (${Math.ceil(that.payloadType.getBitsWidth/8.0).toInt} bytes) because the payload sizes do not match.")

    that.arbitrationFrom(this)
    if (alignment == LSB)
      that.payload._1.assignFromBits(this.tdata.takeLow(that.payload.getBitsWidth))
    else
      that.payload._1.assignFromBits(this.tdata.takeHigh(that.payload.getBitsWidth))

    if (this.tid != null)
      that._2 := this.tid
    if (this.tdest != null)
      that._3 := this.tdest
    if (this.tuser != null)
      that._4 := this.tuser

    that
  }

  override def clone: Stream[Axi4StreamBundle] = new Axi4Stream(config)
}


object Axi4StreamPriv {
  def driveWeak[T <: Data](source : Bundle,sink : Bundle, by : T,to : T,defaultValue : () => T,allowResize : Boolean,allowDrop : Boolean) : Unit = {
    (to != null,by != null) match {
      case (false,false) =>
      case (true,false) => if(defaultValue != null) to := defaultValue() else LocatedPendingError(s"$source can't drive $to because the corresponding source signal does not exist")
      case (false,true) => if(!allowDrop) LocatedPendingError(s"$by can't drive $sink because the corresponding sink signal does not exist")
      case (true,true) => to := (if(allowResize) by.resized else by)
    }
  }

  def resizeOrTrim(source: Bits, sink: Bits): Unit = sink(Math.min(source.getBitsWidth, sink.getBitsWidth) downto 0) := source.resize(Math.max(source.getBitsWidth, sink.getBitsWidth))

  def connectFrom[T <: Axi4StreamBundle](source: Stream[T], sink: Stream[T]): Stream[T] = {
    sink.arbitrationFrom(source)
    assert(source.config.dataBytes <= sink.config.dataBytes, s"Axi4Stream $source directly drives stream $sink with smaller data width! (${source.config.dataBytes} > ${sink.config.dataBytes})")
    assert(source.config.idWidth <= sink.config.idWidth, s"Axi4Stream $source directly drives stream $sink with smaller ID width! (${source.config.idWidth} > ${sink.config.idWidth})")
    assert(source.config.destWidth <= sink.config.destWidth, s"Axi4Stream $source directly drives stream $sink with smaller destination width! (${source.config.destWidth} > ${sink.config.destWidth})")
    assert(source.config.userWidth <= sink.config.userWidth, s"Axi4Stream $source directly drives stream $sink with smaller user width! (${source.config.userWidth} > ${sink.config.userWidth})")

    sink.tdata := source.tdata.resized
    driveWeak(source,sink,source.tid,sink.tid, () => U(sink.tid.bitsRange -> false), allowResize = true, allowDrop = false)
    driveWeak(source,sink,source.tstrb,sink.tstrb, () => B(sink.tstrb.bitsRange -> true), allowResize = true, allowDrop = false)
    driveWeak(source,sink,source.tkeep,sink.tkeep, () => B(sink.tkeep.bitsRange -> true), allowResize = true, allowDrop = false)
    driveWeak(source,sink,source.tlast,sink.tlast, () => False, allowResize = false,allowDrop = false)
    driveWeak(source,sink,source.tdest,sink.tdest, () => B(sink.tdest.bitsRange -> true), allowResize = true, allowDrop = false)

    (sink.tuser != null, source.tuser != null) match {
      case (false, false) =>
      case (true, false) => B(sink.tuser.bitsRange -> false)
      case (false, true) => LocatedPendingError(s"${source.tuser} can't drive $sink because the corresponding sink signal does not exist")
      case (true, true) => {
        val sourceSlices = source.tuser.subdivideIn(source.config.dataBytes slices)
        val sinkSlices = sink.tuser.subdivideIn(sink.config.dataBytes slices)
        val sourceSlicesPad = sourceSlices.padTo(sinkSlices.length, null)
        sourceSlicesPad.zip(sinkSlices).foreach(pair => {
          val (sourceSlice, sinkSlice) = pair
          if (sourceSlice != null)
            resizeOrTrim(sourceSlice, sinkSlice)
          else
            sinkSlice := B(sinkSlice.bitsRange -> false)
        })
      }
    }

    sink
  }
}

case class Axi4Repacker(stream: Axi4Stream) extends Component {
  val io = new Bundle {
    val axis_s = slave(stream.clone)
    val axis_m = master(new Axi4Stream(stream.config.copy(useStrb = false, useKeep = false)))
  }
}


sealed trait BitAlignment
object LSB extends BitAlignment
object MSB extends BitAlignment

object Axi4Stream {
  implicit class RichStream[T <: Data](val source: Stream[T]) {
    /**
      * Maps a source as a AXI4-Stream. Supports optional MSB bit alignment
      * @param alignment Align the data to LSB (left expansion) or MSB (shifting left)
      * @return Stream with Axi4Stream payload
      */
    def toAxi4Stream(alignment: BitAlignment = LSB): Axi4Stream = {
      val payloadBytes = (source.payload.getBitsWidth/8.0).ceil.intValue()
      val axisStream = new Axi4Stream (
        Axi4StreamConfig(dataBytes = payloadBytes)
      )
      alignment match {
        case LSB => axisStream.tdata := source.payload.asBits.resize(payloadBytes*8)
        case MSB => axisStream.tdata := source.payload.asBits.resizeLeft(payloadBytes*8)
      }
      axisStream.arbitrationFrom(source)
      axisStream
    }
  }

  implicit class RichStreamFragment[T <: Data](val source: Stream[Fragment[T]]) {
    /**
      * Maps a source as a AXI4-Stream with TLAST. Supports optional MSB bit alignment
      * @param alignment Align the data to LSB (left expansion) or MSB (shifting left)
      * @return Stream with Axi4Stream payload
      */
    def toAxi4Stream(alignment: BitAlignment = LSB): Axi4Stream = {
      source.map(_.fragment).toAxi4Stream(alignment).map(axisPayload => {
        val axisWithLast = new Axi4Stream(axisPayload.config.copy(useLast = true))
        axisWithLast.assignSomeByName(axisPayload)
        axisWithLast.tlast := source.last
        axisWithLast
      })
    }
  }
}