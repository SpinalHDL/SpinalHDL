package spinal.lib

import spinal.core._

import scala.collection.mutable.ArrayBuffer

class FragmentFactory {
  def apply[T <: Data](fragmentType: HardType[T]): Fragment[T] = new Fragment(fragmentType)
}

object Fragment extends FragmentFactory


class FlowFragmentPimped[T <: Data](pimped: Flow[Fragment[T]]) {
  def toFlowOfFragment : Flow[T] = pimped.translateWith(pimped.fragment)


  def filterHeader(header: T): Flow[Fragment[T]] = {
    val takeIt = RegInit(False)

    when(pimped.isFirst) {
      when(pimped.fragment === header) {
        takeIt := True
      }
    }
    when(pimped.isLast) {
      takeIt := False
    }

    return pimped.takeWhen(takeIt)
  }

  def pulseOn(header: T): Bool = pimped.isFirst && pimped.fragment === header
  def eventOn(header: T): Event = {
    val ret = Event

    val state = RegInit(False)
    when(pimped.isFirst && pimped.fragment === header) {
      state := True
    }
    when(ret.ready) {
      state := False
    }

    ret.valid := state
    ret
  }


}


class StreamFragmentPimped[T <: Data](pimped: Stream[Fragment[T]]) {
  //  def last : Bool = pimped.last
  //  def fragment : T = pimped.data.fragment

  def toStreamOfFragment : Stream[T] = pimped.translateWith(pimped.fragment)

/**
 * Stepwise reduction of all fragments into a single value. With this, you can calculate
 * the (check)sum of every n elements or parity bits.
 * @param accumulator A combinatorial function that accumulates the current (param 3) and the previous
 * 	(param 2) value into the next one (param 1)
 * @example {{{ outStream = inStream.reduce[SInt]((a, b, c) => { a := b + c }) }}}
 */
  def reduce[U <: Data](identity: U, accumulator: (U, U, T) => Unit): Stream[U] = {
    val next = new Stream(identity).setCompositeName(pimped, "reduced", true)
    val acc = Reg(identity)
    
    accumulator(next.payload, acc, pimped.fragment)
    
    when(pimped.fire) {
      acc := Mux(pimped.last, identity, next.payload)
    }
    when(pimped.last) {
      pimped.ready := next.ready
      next.valid := pimped.valid
    } otherwise {
      pimped.ready := True
      next.valid := False
    }
    next
  }

  /** 
   * Insert a given header value at the begin of each new packet. This may stall
   * upstream during the insertion.
   */
  def insertHeader(header: T): Stream[Fragment[T]] = {
    val ret = cloneOf(pimped)
    val waitPacket = RegInit(True)

    pimped.ready := ret.ready && !waitPacket
    ret.valid := False
    ret.last := False
    ret.fragment := header

    when(pimped.valid) {
      ret.valid := True
      when(!waitPacket) {
        ret.payload := pimped.payload
      }
    }

    when(ret.fire) {
      waitPacket := False
      when(ret.last) {
        waitPacket := True
      }
    }

    ret
  }

  //TODOTEST not tested
  /** 
   * Insert a given header value at the begin of each new packet. This may stall
   * upstream during the insertion.
   */
  def insertHeader(header: Vec[T]): Stream[Fragment[T]] = {
    val ret = cloneOf(pimped)
    val packetWait = RegInit(True)
    val counter = Counter(header.size)

    pimped.ready := ret.ready && !packetWait
    ret.valid := False
    ret.last := False
    ret.fragment := header(counter)

    when(pimped.valid) {
      ret.valid := True
      when(!packetWait) {
        ret.payload := pimped.payload
      }
    }

    when(ret.fire) {
      when(packetWait){
        counter.increment()
        when(counter.willOverflowIfInc) {
          packetWait := False
        }
      }
      when(ret.last) {
        packetWait := True
      }
    }

    ret
  }

  def toFragmentBits(bitsWidth: Int): Stream[Fragment[Bits]] = {
    val pimpedWidhoutLast = (Stream(pimped.fragment)).translateFrom(pimped)((to, from) => {
      to := from.fragment
    })

    val fragmented = pimpedWidhoutLast.fragmentTransaction(bitsWidth)

    return (Stream Fragment (Bits(bitsWidth bit))).translateFrom(fragmented)((to, from) => {
      to.last := from.last && pimped.last
      to.fragment := from.fragment
    })
  }
}


class FlowBitsPimped(pimped: Flow[Bits]) {
  def toFlowFragmentBits(cMagic: Bits = B"x74", cLast: Bits = B"x53"): Flow[Fragment[Bits]] = {
    toFlowFragmentBitsAndReset(cMagic, cLast)._1
  }

  def toFlowFragmentBitsAndReset(cMagic: Bits = B"x74", cLast: Bits = B"x53", cResetSet: Bits = B"x54", cResetClear: Bits = B"x55"): (Flow[Fragment[Bits]], Bool) = {
    val ret = Flow Fragment (pimped.payloadType)
    val softReset = RegInit(True)
    val inMagic = RegInit(False)
    val buffer = Reg(pimped)
    val newData = False
    val isLast = False
    val isMagic = pimped.payload === cMagic

    ret.valid := False
    ret.last := isLast
    ret.fragment := buffer.payload
    when(isLast || newData) {
      ret.valid := buffer.valid
      buffer.valid := False
    }



    buffer.valid.init(False)
    when(pimped.valid) {
      when(inMagic) {
        inMagic := False
        when(pimped.payload === cLast) {
          isLast := True
        }
        when(pimped.payload === cResetSet) {
          softReset := True
        }
        when(pimped.payload === cResetClear) {
          softReset := False
        }
      }elsewhen(isMagic) {
        inMagic := True
      }

      when((inMagic && isMagic) || (!inMagic && !isMagic)) {
        buffer.valid := True
        buffer.payload := pimped.payload
        newData := True
      }
    }


    (ret, softReset)
  }
}


object FragmentToBitsStates extends SpinalEnum {
  val eDefault, eFinish0, eFinish1, eMagicData = newElement()
}



class StreamBundlePimped[T <: Bundle](self: Stream[T]) {
  def connectFromRelaxed(that: Stream[T]): Unit = {
    self.valid := that.valid
    that.ready := self.ready
    self.payload assignSomeByName(that.payload)
    self.payload.assignDontCareToUnasigned()
  }
}

class StreamBitsPimped(pimped: Stream[Bits]) {
  //  def toStreamFragmentBits(cMagic: Bits = "x74", cLast: Bits = "x53"): Stream[Fragment[Bits]] = {
  //    val ret = Stream Fragment (pimped.data)
  //    val inMagic = RegInit(False)
  //    when(pimped.fire){
  //      inMagic := pimped.data === cMagic && !inMagic
  //    }
  //
  //    val buffer = pimped.throwWhen(inMagic).m2sPipe
  //
  //    ret.connectFrom(buffer.haltWhen(!pimped.valid).throwWhen(inMagic && (pimped.data !== cMagic)))((to,from) => {
  //      to.last := inMagic && pimped.data === cLast
  //      to.fragment := buffer.data
  //    })
  //
  //    ret
  //  }




}

class StreamFragmentBitsPimped(pimped: Stream[Fragment[Bits]]) {
  def toStreamBits(cMagic: Bits = 0x74, cLast: Bits = 0x53): Stream[Bits] = {
    import FragmentToBitsStates._
    val ret = Stream(pimped.fragment)
    val state = RegInit(eDefault)


    pimped.ready := False
    switch(state) {
      default {
        ret.valid := pimped.valid
        ret.payload := pimped.fragment
        pimped.ready := ret.ready

        when(pimped.fragment === cMagic) {
          pimped.ready := False
          when(ret.fire) {
            state := eMagicData
          }
        }
      }
      is(eMagicData) {
        ret.valid := True
        ret.payload := pimped.fragment
        when(ret.ready) {
          pimped.ready := True
          state := eDefault
        }
      }
      is(eFinish0) {
        ret.valid := True
        ret.payload := cMagic
        when(ret.ready) {
          state := eFinish1
        }
      }
      is(eFinish1) {
        ret.valid := True
        ret.payload := cLast
        when(ret.ready) {
          state := eDefault
        }
      }
    }

    when(pimped.fire && pimped.last) {
      state := eFinish0
    }

    ret
  }


  //Little endian, not tested
  def toStreamOf[T <: Data](toDataType: T): Stream[T] = {
    val fromWidth = pimped.fragment.getWidth
    val toWidth = toDataType.getBitsWidth
    val ret = Stream(toDataType)

    if (toWidth <= fromWidth) {
      ret.valid := pimped.fire && pimped.last
      ret.payload.assignFromBits(pimped.fragment.resize(ret.payload.getBitsWidth))
      pimped.ready := ret.ready
    } else {
      val missingBitsCount = toWidth - fromWidth

      val buffer = Reg(Bits((missingBitsCount + fromWidth - 1) / fromWidth * fromWidth bit))
      when(pimped.fire) {
        buffer := pimped.fragment ## (buffer >> fromWidth)
      }

      ret.valid := pimped.isLast
      pimped.ready := ret.ready || ! pimped.isLast
      ret.payload.assignFromBits(pimped.fragment ## buffer)
    }
    ret
  }
}

case class StreamFragmentBitsDispatcherElement(sink : Stream[Bits],header : Int)

class StreamFragmentBitsDispatcher(headerWidth : Int,input : Stream[Fragment[Bits]],outputs : Seq[(Int,Stream[Data])]) extends Area{
  val sourceWidth = input.fragment.getWidth
  val dataMaxWidth = outputs.map(_._2.payload.getBitsWidth).reduce(Math.max(_,_))
  val dataWidth =  roundUp(dataMaxWidth,by=sourceWidth).toInt
  val dataPacketCount = dataWidth / sourceWidth
  val dataShifter = Reg(Bits(dataWidth bit))
  val dataLoaded = RegInit(False)

  val headerShifter = Reg(Bits(roundUp(headerWidth,by=sourceWidth) bit))
  val headerPacketCount = headerShifter.getWidth / sourceWidth
  val header = headerShifter(headerWidth-1 downto 0)
  val headerLoaded = RegInit(False)

  val counter = Reg(UInt(log2Up(headerPacketCount) bit)) init(0)

  when(input.valid) {
    when(headerLoaded === False) {
      headerShifter := (input.fragment ## headerShifter) >> sourceWidth
      counter := counter + 1
      when(counter === headerPacketCount-1){
        headerLoaded := True
      }
    }otherwise{
      dataShifter := (input.fragment ## dataShifter) >> sourceWidth
    }

    when(input.last){
      headerLoaded := True
      dataLoaded := True
      counter := 0
    }
  }

  input.ready := !dataLoaded

  for((portHeader,port) <- outputs){
    val sinkWidth = port.payload.getBitsWidth
    val offset = dataWidth - roundUp(sinkWidth,by=sourceWidth).toInt
    port.payload.assignFromBits(dataShifter(offset,sinkWidth bit))
    port.valid := dataLoaded && header === portHeader
  }

  when(headerLoaded && dataLoaded && outputs.map(!_._2.isStall).reduce(_ && _)){
    headerLoaded := False
    dataLoaded := False
  }
}



class DataCarrierFragmentPimped[T <: Data](pimped: DataCarrier[Fragment[T]]) {
  def first: Bool = signalCache(pimped, "first")(RegNextWhen(pimped.last, pimped.fire, True).setCompositeName(pimped, "first", true))
  def tail: Bool = !first
  def isFirst: Bool = pimped.valid && first
  def isTail : Bool = pimped.valid && tail
  def isLast: Bool = pimped.valid && pimped.last
  def lastFire : Bool = pimped.fire && pimped.last
  def firstFire : Bool = pimped.fire && pimped.first
}


class DataCarrierFragmentBitsPimped(pimped: DataCarrier[Fragment[Bits]]) {


  //safeTransition => when false, the design is smaller, but the register is a Shift Register (unwanted state during loading)
  def toRegOf[T <: Data](dataType: T, safeTransition: Boolean = true): T = {
    if (safeTransition)
      toFlowOf(dataType).toReg()
    else {
      val fromWidth = pimped.fragment.getWidth
      val toWidth = dataType.getBitsWidth
      val missingBitsCount = toWidth - fromWidth
      val bufferLowWidth = (missingBitsCount + fromWidth - 1) / fromWidth * fromWidth
      val buffer = Reg(Bits(toWidth bit))

      when(pimped.fire) {
        when(pimped.last) {
          buffer(buffer.high downto bufferLowWidth) := pimped.fragment
        } otherwise {
          buffer(bufferLowWidth - 1 downto 0) := pimped.fragment ## buffer(bufferLowWidth - 1 downto fromWidth)
        }
      }

      buffer.toDataType(dataType)
    }
  }

  //Little endian
  def toFlowOf[T <: Data](toDataType: T): Flow[T] = {
    val fromWidth = pimped.fragment.getWidth
    val toWidth = toDataType.getBitsWidth
    val ret = Flow(toDataType)

    pimped.freeRun()

    if (toWidth <= fromWidth) {
      ret.valid := pimped.fire && pimped.last
      ret.payload.assignFromBits(pimped.fragment)
    } else {
      val missingBitsCount = toWidth - fromWidth

      val buffer = Reg(Bits((missingBitsCount + fromWidth - 1) / fromWidth * fromWidth bit))
      when(pimped.fire) {
        buffer := pimped.fragment ## (buffer >> fromWidth)
      }

      ret.valid := pimped.isLast
      ret.payload.assignFromBits(pimped.fragment ## buffer)
    }
    ret
  }


}


class FlowFragmentFactory extends MSFactory {
  def apply[T <: Data](dataType: HardType[T]): Flow[Fragment[T]] = {
    val ret = new Flow(Fragment(dataType))
    postApply(ret)
    ret
  }
}

class StreamFragmentFactory extends MSFactory {
  def apply[T <: Data](fragmentType: HardType[T]): Stream[Fragment[T]] = {
    val ret = new Stream(Fragment(fragmentType))
    postApply(ret)
    ret
  }
}


class Fragment[T <: Data](val fragmentType: HardType[T]) extends Bundle {
  val last = Bool()
  val fragment: T = fragmentType()

  def dataType = fragmentType()
  override def clone: this.type = {
    new Fragment(fragmentType).asInstanceOf[this.type]
  }
}


//object FlowFragmentRouter {
//  def apply(input: Flow[Fragment[Bits]], outputSize: Int): Vec[Flow[Fragment[Bits]]] = {
//    FlowFragmentRouter(input, (0 until outputSize).map(BigInt(_)))
//  }
//
//  def apply(input: Flow[Fragment[Bits]], mapTo: Iterable[BigInt]): Vec[Flow[Fragment[Bits]]] = {
//    val router = new FlowFragmentRouter(input, mapTo)
//    return router.outputs
//  }
//}
//
//class FlowFragmentRouter(input: Flow[Fragment[Bits]], mapTo: Iterable[BigInt]) extends Area {
//  val outputs = Vec(mapTo.size, cloneOf(input))
//  val enables = Vec(mapTo.size, Reg(Bool()))
//
//  outputs.foreach(_.data := input.data)
//  when(input.isNotInTail) {
//    (enables, mapTo).zipped.foreach((en, filter) => en := B(filter) === input.fragment)
//  } otherwise {
//    (outputs, enables).zipped.foreach(_.valid := _)
//  }
//}

object FlowFragmentBitsRouter {
  def apply(input: Flow[Fragment[Bits]], allowBroadcast: Boolean = false) = new FlowFragmentBitsRouter(input, allowBroadcast)
}

class FlowFragmentBitsRouter(input: Flow[Fragment[Bits]], allowBroadcast: Boolean = false) {
  val broadcast = if (allowBroadcast) input.fragment === (BigInt(1) << widthOf(input.fragment)) - 1 else False
  val isFirst = input.isFirst
  val isLast = input.isLast

  def filter(header: Bits): Flow[Fragment[Bits]] = {
    val enable = RegInit(False)

    when(isFirst) {
      enable := input.fragment === header || broadcast
    }
    when(isLast) {
      enable := False
    }

    input.takeWhen(enable)
  }
}


class StreamToStreamFragmentBits[T <: Data](dataType: T, bitsWidth: Int) extends Component {
  val io = new Bundle {
    val input = slave Stream (dataType)
    val output = master Stream Fragment(Bits(bitsWidth bit))
  }
  val counter = Counter((widthOf(dataType) - 1) / bitsWidth + 1)
  val inputBits = B(0, bitsWidth bit) ## B(io.input.payload) //The ## allow to mux inputBits

  io.input.ready := counter.willOverflow
  io.output.last := counter.willOverflowIfInc
  io.output.valid := io.input.valid
  io.output.fragment := inputBits(counter * U(bitsWidth), bitsWidth bit)
  when(io.output.fire) {
    counter.increment()
  }
}



object StreamFragmentGenerator {
  def apply(event: Event, packetData: Vec[Bits], bitsWidth: Int): Stream[Fragment[Bits]] = {
    apply(event, packetData, Bits(bitsWidth bit))
  }

  def apply[T <: Data](event: Event, packetData: Vec[T], dataType: T): Stream[Fragment[T]] = {
    val ret = Stream Fragment (packetData.dataType())
    val counter = Counter(packetData.size)

    event.ready := Bool(false)
    ret.valid := event.valid
    ret.last := counter.willOverflowIfInc
    ret.fragment := packetData(counter)
    when(ret.fire) {
      counter.increment()
    }

    when(counter.willOverflow) {
      event.ready := Bool(true)
    }

    ret
  }

}


object StreamFragmentArbiter {
   def apply[T <: Data](dataType: T)(inputs: Seq[Stream[Fragment[T]]]): Stream[Fragment[T]] = {
    val arbiter = new StreamArbiter(Fragment(dataType), inputs.size)(StreamArbiter.Arbitration.lowerFirst, StreamArbiter.Lock.fragmentLock)
    (inputs, arbiter.io.inputs).zipped.foreach(_ >> _)
    arbiter.io.output
  }
}

object StreamFragmentArbiterAndHeaderAdder {
  def apply[T <: Data](dataType: T)(inputs: Seq[Tuple2[Stream[Fragment[T]], T]]): Stream[Fragment[T]] = {
    val arbiter = new StreamArbiter(Fragment(dataType), inputs.size)(StreamArbiter.Arbitration.lowerFirst, StreamArbiter.Lock.fragmentLock)
    (inputs, arbiter.io.inputs).zipped.foreach(_._1 >> _)

    val ret = Stream Fragment (dataType)
    def first = ret.first
    ret.valid := arbiter.io.output.valid
    ret.last := arbiter.io.output.last && !first
    ret.fragment := Mux(first, Vec(inputs.map(_._2))(arbiter.io.chosen), arbiter.io.output.fragment)
    arbiter.io.output.ready := ret.ready && !first

    ret
    // arbiter.io.output
  }
}