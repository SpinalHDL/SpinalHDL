package spinal.lib.bus.amba4.axis

import spinal.core._
import spinal.lib._

object Axi4Stream {

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

  type Axi4Stream = Stream[Axi4StreamBundle]

  def apply(config: Axi4StreamConfig): Stream[Axi4StreamBundle] = Stream(Axi4StreamBundle(config))

  implicit class Axi4StreamRich(stream: Stream[Axi4StreamBundle]) {
     def connectFrom(that: Stream[Axi4StreamBundle]): Stream[Axi4StreamBundle] = Axi4StreamPriv.connectFrom(that, stream)

    /**
      * Converts the Axi4Stream into a Stream of a specified type.
      * Does not support TSTRB or TKEEP. Will only convert continuous and aligned streams.
      * If TLAST was present it will be dropped. Consider toStreamFragment
      * @param newType Resulting stream payload type
      * @param endianness Extract bits from LSB or MSB, drops additional bits
      * @tparam T Stream payload type
      * @return Stream of type T
      */
    def toStream[T <: Data](newType: HardType[T], endianness: Endianness = LITTLE): Stream[T] = {
      val that = Stream(newType())
      assert(!stream.config.useStrb, s"Can't convert Axi4Stream $this to Stream of ${that.payloadType} because the Axi4Stream supports TSTRB.")
      assert(stream.config.dataBytes == Math.ceil(that.payload.getBitsWidth/8.0).toInt, s"Can't convert Axi4Stream $this (${stream.config.dataBytes} bytes) to Stream of ${that.payloadType} (${Math.ceil(that.payloadType.getBitsWidth/8.0).toInt} bytes) because the payload sizes do not match.")
      if (stream.config.useLast)
        SpinalWarning(s"Axi4Stream $this converted to Stream of ${that.payloadType} discards TLAST. Consider using toFragmentStream instead.")

      that.arbitrationFrom(stream)
      if (endianness == LITTLE)
        that.payload.assignFromBits(stream.tdata.takeLow(that.payload.getBitsWidth))
      else
        that.payload.assignFromBits(stream.tdata.takeHigh(that.payload.getBitsWidth))

      that
    }

    /**
      * Converts the Axi4Stream into a Stream Fragment of of a specified type.
      * Does not support TSTRB or TKEEP. Will only convert continuous and aligned streams.
      * @param newType Resulting stream payload type
      * @param endianness Extract bits from LSB or MSB, drops additional bits
      * @tparam T Stream payload type
      * @return Stream of type T
      */
    def toStreamFragment[T <: Data](newType: HardType[T], endianness: Endianness = LITTLE): Stream[Fragment[T]] = {
      val that = Stream(Fragment(newType()))
      assert(!stream.config.useStrb, s"Can't convert Axi4Stream $this to Stream of ${that.payloadType} because the Axi4Stream supports TSTRB.")
      assert(stream.config.dataBytes == Math.ceil(that.payload.getBitsWidth/8.0).toInt, s"Can't convert Axi4Stream $this (${stream.config.dataBytes} bytes) to Stream of ${that.payloadType} (${Math.ceil(that.payloadType.getBitsWidth/8.0).toInt} bytes) because the payload sizes do not match.")
      assert(stream.config.useLast, "Can't convert Axi4Stream $this to Fragment Stream of ${that.payloadType} because the Axi4Stream doesn't support TLAST")

      that.arbitrationFrom(stream)
      if (endianness == LITTLE)
        that.payload.assignFromBits(stream.tdata.takeLow(that.payload.getBitsWidth))
      else
        that.payload.assignFromBits(stream.tdata.takeHigh(that.payload.getBitsWidth))
      that.last := stream.tlast

      that
    }

    /**
      * Results a Flow of TID. To be used with toStream functions.
      * @return Flow of Axi4Stream TID field
      */
    def getIdFlow(): Flow[UInt] = {
      val idFlow = Flow(if (stream.tid != null) stream.tid else UInt(0 bit))
      idFlow.valid := stream.valid
      idFlow.payload := stream.tid
      idFlow
    }

    /**
      * Results a Flow of TDEST. To be used with toStream functions.
      * @return Flow of Axi4Stream TDEST field
      */
    def getDestFlow(): Flow[UInt] = {
      val destFlow = Flow(if (stream.tdest != null) stream.tdest else UInt(0 bit))
      destFlow.valid := stream.valid
      destFlow.payload := stream.tdest
      destFlow
    }

    /**
      * Results a Flow of TUSER. To be used with toStream functions.
      * @return Flow of Axi4Stream TUSER field
      */
    def getUserFlow(): Flow[Bits] = {
      val userFlow = Flow(if (stream.tuser != null) stream.tuser else Bits(0 bit))
      userFlow.valid := stream.valid
      userFlow.payload := stream.tuser
      userFlow
    }
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

  implicit class RichStream[T <: Data](val source: Stream[T]) {
    /**
      * Maps a source as a AXI4-Stream. Supports optional MSB bit alignment
      * @param endianness Align the data to LSB (left expansion) or MSB (shifting left)
      * @return Stream with Axi4Stream payload
      */
    def toAxi4Stream(endianness: Endianness = LITTLE): Axi4Stream = {
      val payloadBytes = (source.payload.getBitsWidth/8.0).ceil.intValue()
      val axisStream = Axi4Stream(Axi4StreamConfig(dataBytes = payloadBytes))
      endianness match {
        case LITTLE => axisStream.tdata := source.payload.asBits.resize(payloadBytes*8)
        case BIG => axisStream.tdata := source.payload.asBits.resizeLeft(payloadBytes*8)
      }
      axisStream.arbitrationFrom(source)
      axisStream
    }
  }

  implicit class RichStreamFragment[T <: Data](val source: Stream[Fragment[T]]) {
    /**
      * Maps a source as a AXI4-Stream with TLAST. Supports optional MSB bit alignment
      * @param endianness Align the data to LSB (left expansion) or MSB (shifting left)
      * @return Stream with Axi4Stream payload
      */
    def toAxi4Stream(endianness: Endianness = LITTLE): Axi4Stream = {
      source.map(_.fragment).toAxi4Stream(endianness).map(axisPayload => {
        val axisWithLast = Axi4Stream(axisPayload.config.copy(useLast = true))
        axisWithLast.assignSomeByName(axisPayload)
        axisWithLast.tlast := source.last
        axisWithLast
      })
    }
  }
}