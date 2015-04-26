package spinal.lib

import spinal.core._

class FragmentFactory {
  def apply[T <: Data](dataType: T): Fragment[T] = new Fragment(dataType)
}

object Fragment extends FragmentFactory

//object FlowFragment extends FlowFragmentFactory

//object HandshakeFragment extends HandshakeFragmentFactory


class FlowFragmentPimped[T <: Data](pimped: Flow[Fragment[T]]) {
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
    val ret = slave Event

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


class HandshakeFragmentPimped[T <: Data](pimped: Handshake[Fragment[T]]) {
  //  def last : Bool = pimped.last
  //  def fragment : T = pimped.data.fragment


  def insertHeader(header: T): Handshake[Fragment[T]] = {
    val ret = cloneOf(pimped)
    val waitPacket = RegInit(True)

    pimped.ready := ret.ready && !waitPacket
    ret.valid := False
    ret.last := False
    ret.fragment := header

    when(pimped.valid) {
      ret.valid := True
      when(!waitPacket) {
        ret.data := pimped.data
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


  def toFragmentBits(bitsWidth: Int): Handshake[Fragment[Bits]] = {
    val pimpedWidhoutLast = (Handshake(pimped.fragment)).translateFrom(pimped)((to, from) => {
      to := from.fragment
    })

    val fragmented = pimpedWidhoutLast.fragmentTransaction(bitsWidth)

    return (Handshake Fragment (Bits(bitsWidth bit))).translateFrom(fragmented)((to, from) => {
      to.last := from.last && pimped.last
      to.fragment := from.fragment
    })
  }
}


class FlowBitsPimped(pimped: Flow[Bits]) {
  def toFlowFragmentBits(cMagic: Bits = "x74", cLast: Bits = "x53"): Flow[Fragment[Bits]] = {
    toFlowFragmentBitsAndReset(cMagic, cLast)._1
  }

  def toFlowFragmentBitsAndReset(cMagic: Bits = "x74", cLast: Bits = "x53", cResetSet: Bits = "x54", cResetClear: Bits = "x55"): (Flow[Fragment[Bits]], Bool) = {
    val ret = Flow Fragment (pimped.dataType)
    val softReset = RegInit(True)
    val inMagic = RegInit(False)
    val buffer = Reg(pimped)
    val newData = False
    val isLast = False
    val isMagic = pimped.data === cMagic

    ret.valid := False
    ret.last := isLast
    ret.fragment := buffer.data
    when(isLast || newData) {
      ret.valid := buffer.valid
      buffer.valid := False
    }



    buffer.valid.init(False)
    when(pimped.valid) {
      when(inMagic) {
        inMagic := False
        when(pimped.data === cLast) {
          isLast := True
        }
        when(pimped.data === cResetSet) {
          softReset := True
        }
        when(pimped.data === cResetClear) {
          softReset := False
        }
      }.elsewhen(isMagic) {
        inMagic := True
      }

      when((inMagic && isMagic) || (!inMagic && !isMagic)) {
        buffer.valid := True
        buffer.data := pimped.data
        newData := True
      }
    }


    (ret, softReset)
  }
}


object FragmentToBitsStates extends SpinalEnum {
  val eDefault, eFinish0, eFinish1, eMagicData = Value
}


class HandshakeBitsPimped(pimped: Handshake[Bits]) {
  //  def toHandshakeFragmentBits(cMagic: Bits = "x74", cLast: Bits = "x53"): Handshake[Fragment[Bits]] = {
  //    val ret = Handshake Fragment (pimped.data)
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

class HandshakeFragmentBitsPimped(pimped: Handshake[Fragment[Bits]]) {
  def toHandshakeBits(cMagic: Bits = 0x74, cLast: Bits = 0x53): Handshake[Bits] = {
    import FragmentToBitsStates._
    val ret = Handshake(pimped.fragment)
    val state = RegInit(eDefault)


    pimped.ready := False
    switch(state) {
      default {
        ret.valid := pimped.valid
        ret.data := pimped.fragment
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
        ret.data := pimped.fragment
        when(ret.ready) {
          pimped.ready := True
          state := eDefault
        }
      }
      is(eFinish0) {
        ret.valid := True
        ret.data := cMagic
        when(ret.ready) {
          state := eFinish1
        }
      }
      is(eFinish1) {
        ret.valid := True
        ret.data := cLast
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
}

class DataCarrierFragmentPimped[T <: Data](pimped: DataCarrier[Fragment[T]]) {
  def fragment: T = pimped.data.fragment
  def first: Bool = signalCache(pimped, "first", () => RegNextWhen(pimped.last, pimped.fire, True))
  def tail: Bool = !first
  def last: Bool = pimped.data.last
  def isFirst: Bool = pimped.valid && first
  def isLast: Bool = pimped.valid && pimped.last
}


class DataCarrierFragmentBitsPimped(pimped: DataCarrier[Fragment[Bits]]) {


  //safeTransition => when false, the design is smaller, but the register is a Shift Register (unwanted state during loading)
  def toRegOf[T <: Data](dataType: T, safeTransition: Boolean = true): T = {
    if (safeTransition)
      toFlowOf(dataType).toReg
    else {
      val fromWidth = pimped.fragment.getWidth
      val toWidth = dataType.getBitsWidth
      val missingBitsCount = toWidth - fromWidth

      val bufferLowWidth = (missingBitsCount + fromWidth - 1) / fromWidth * fromWidth
      val bufferHighWidth = toWidth - bufferLowWidth
      val buffer = Reg(Bits(toWidth bit))

      when(pimped.fire) {
        when(pimped.last) {
          buffer(buffer.high, bufferLowWidth) := pimped.fragment
        } otherwise {
          buffer(bufferLowWidth - 1, 0) := pimped.fragment ## buffer(bufferLowWidth - 1, fromWidth)
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

    pimped.freeRun

    if (toWidth <= fromWidth) {
      ret.valid := pimped.fire && pimped.last
      ret.data.assignFromBits(pimped.fragment)
    } else {
      val missingBitsCount = toWidth - fromWidth

      val buffer = Reg(Bits((missingBitsCount + fromWidth - 1) / fromWidth * fromWidth bit))
      when(pimped.fire) {
        buffer := pimped.fragment ## (buffer >> fromWidth)
      }

      ret.valid := pimped.isLast
      ret.data.assignFromBits(pimped.fragment ## buffer)
    }
    ret
  }


}


class FlowFragmentFactory extends MSFactory {
  def apply[T <: Data](dataType: T): Flow[Fragment[T]] = {
    val ret = new Flow(Fragment(dataType))
    postApply(ret)
    ret
  }
}

class HandshakeFragmentFactory extends MSFactory {
  def apply[T <: Data](dataType: T): Handshake[Fragment[T]] = {
    val ret = new Handshake(Fragment(dataType))
    postApply(ret)
    ret
  }
}


class Fragment[T <: Data](dataType: T) extends Bundle {
  val last = Bool
  val fragment: T = dataType.clone

  override def clone: this.type = {
    new Fragment(dataType).asInstanceOf[this.type];
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
//  val enables = Vec(mapTo.size, Reg(Bool))
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


class HandshakeToHandshakeFragmentBits[T <: Data](dataType: T, bitsWidth: Int) extends Component {
  val io = new Bundle {
    val input = slave Handshake (dataType)
    val output = master Handshake Fragment(Bits(bitsWidth bit))
  }
  val counter = Counter((widthOf(dataType) - 1) / bitsWidth + 1)
  val inputBits = B(0, bitsWidth bit) ## toBits(io.input.data) //The ## allow to mux inputBits

  io.input.ready := counter.overflow
  io.output.last := counter.overflowIfInc
  io.output.valid := io.input.valid
  io.output.fragment := inputBits(counter * U(bitsWidth), bitsWidth bit)
  when(io.output.fire) {
    counter ++
  }
}



object HandshakeFragmentGenerator {
  def apply(event: Event, packetData: Vec[Bits], bitsWidth: Int): Handshake[Fragment[Bits]] = {
    apply(event, packetData, Bits(bitsWidth bit))
  }

  def apply[T <: Data](event: Event, packetData: Vec[T], dataType: T): Handshake[Fragment[T]] = {
    val ret = Handshake Fragment (packetData.dataType)
    val counter = Counter(packetData.size)

    event.ready := Bool(false)
    ret.valid := event.valid
    ret.last := counter.overflowIfInc
    ret.fragment := packetData(counter)
    when(ret.fire) {
      counter ++
    }

    when(counter.overflow) {
      event.ready := Bool(true)
    }

    ret
  }

}


object HandshakeFragmentArbiter {

  // def apply[T <: Data](dataType: T,portCount: Int) = new HandshakeArbiterCore(dataType,2)(HandshakeArbiterCore.arbitration_lowIdPortFirst,HandshakeArbiterCore.lock_fragmentLock)
  def apply[T <: Data](dataType: T)(inputs: Seq[Handshake[Fragment[T]]]): Handshake[Fragment[T]] = {
    val arbiter = new HandshakeArbiterCore(Fragment(dataType), inputs.size)(HandshakeArbiterCore.arbitration_lowIdPortFirst, HandshakeArbiterCore.lock_fragmentLock)
    (inputs, arbiter.io.inputs).zipped.foreach(_ >> _)
    arbiter.io.output
  }


}

object HandshakeFragmentArbiterAndHeaderAdder {
  def apply[T <: Data](dataType: T)(inputs: Seq[Tuple2[Handshake[Fragment[T]], T]]): Handshake[Fragment[T]] = {
    val arbiter = new HandshakeArbiterCore(Fragment(dataType), inputs.size)(HandshakeArbiterCore.arbitration_lowIdPortFirst, HandshakeArbiterCore.lock_fragmentLock)
    (inputs, arbiter.io.inputs).zipped.foreach(_._1 >> _)

    val ret = Handshake Fragment (dataType)
    def first = ret.first
    ret.valid := arbiter.io.output.valid
    ret.last := arbiter.io.output.last && !first
    ret.fragment := Mux(first, Vec(inputs.map(_._2))(arbiter.io.chosen), arbiter.io.output.fragment)
    arbiter.io.output.ready := ret.ready && !first

    ret
    // arbiter.io.output
  }
}