package spinal.lib

import spinal.core._

class FragmentFactory {
  def apply[T <: Data](dataType: T): Fragment[T] = new Fragment(dataType)
}

object Fragment extends FragmentFactory

object FlowFragment extends FlowFragmentFactory

object HandshakeFragment extends HandshakeFragmentFactory


class FlowFragmentPimped[T <: Data](that: Flow[Fragment[T]]) {
  //  def last : Bool = that.last
  //  def fragment : T = that.data.fragment

  def filterHeader(header: T): Flow[Fragment[T]] = {
    val takeIt = RegInit(False)

    when(that.isFirst) {
      when(that.fragment === header) {
        takeIt := True
      }
    }

    when(that.isLast) {
      takeIt := False
    }

    return that.takeWhen(takeIt)
  }

  def eventOn(header: T):Bool = that.isFirst && that.fragment === header
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


  //Todo prety impl
  def toFragmentBits(bitsWidth: Int): Handshake[Fragment[Bits]] = {
    val pimpedWidhoutLast = Handshake(pimped.fragment)
    pimpedWidhoutLast.translateFrom(pimped)((to, from) => {
      to := from.fragment
    })

    val fragmented = pimpedWidhoutLast.fragmentTransaction(bitsWidth)

    val ret = Handshake Fragment (Bits(bitsWidth bit))
    ret.translateFrom(fragmented)((to, from) => {
      to.last := from.last && pimped.last
      to.fragment := from.fragment
    })
    ret
  }
}


class FlowBitsPimped(pimped: Flow[Bits]) {
  def toFlowFragmentBits(cMagic: Bits = "x74", cLast: Bits = "x53"): Flow[Fragment[Bits]] = {
    val ret = Flow Fragment (pimped.dataType)

    val inMagic = RegInit(False)
    val buffer = Reg(pimped)
    val newData = False
    val isLast = False
    val isMagic = pimped.data !== cMagic
    buffer.valid.init(False)
    when(pimped.valid) {
      when(!inMagic || isMagic) {
        buffer.valid := True
        buffer.data := pimped.data
        newData := True
      }
      when(!inMagic && isMagic) {
        inMagic := True
      }
      when(inMagic && pimped.data === cLast) {
        isLast := True
      }
    }

    ret.valid := False
    ret.last := isLast
    ret.fragment := buffer.data
    when(isLast || newData) {
      ret.valid := buffer.valid
      buffer.valid := False
    }

    ret
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

        when(pimped.valid && pimped.fragment === cMagic) {
          pimped.ready := False
          state := eMagicData
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
  def last: Bool = pimped.data.last

  def fragment: T = pimped.data.fragment


  def isNotInTail = RegNextWhen(pimped.last, pimped.fire, True)

  def isInTail = !isNotInTail

  def isFirst = pimped.valid && isNotInTail

  def isLast = pimped.valid && pimped.last
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

      val bufferLowWidth = ((missingBitsCount - 1) / fromWidth + 1) * fromWidth
      val bufferHighWidth = toWidth - bufferLowWidth
      val buffer = Reg(Bits(toWidth bit))

      when(pimped.fire) {
        when(pimped.last) {
          buffer(buffer.high,bufferLowWidth) := pimped.fragment
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

      val buffer = Reg(Bits(((missingBitsCount - 1) / fromWidth + 1) * fromWidth bit))
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


object FlowFragmentRouter {
  def apply(input: Flow[Fragment[Bits]], outputSize: Int): Vec[Flow[Fragment[Bits]]] = {
    FlowFragmentRouter(input, (0 until outputSize).map(BigInt(_)))
  }

  def apply(input: Flow[Fragment[Bits]], mapTo: Iterable[BigInt]): Vec[Flow[Fragment[Bits]]] = {
    val router = new FlowFragmentRouter(input, mapTo)
    return router.outputs
  }
}

class FlowFragmentRouter(input: Flow[Fragment[Bits]], mapTo: Iterable[BigInt]) extends Area {
  val outputs = Vec(mapTo.size, cloneOf(input))
  val enables = Vec(mapTo.size, Reg(Bool))

  outputs.foreach(_.data := input.data)
  when(input.isNotInTail) {
    (enables, mapTo).zipped.foreach((en, filter) => en := B(filter) === input.fragment)
  } otherwise {
    (outputs, enables).zipped.foreach(_.valid := _)
  }
}

//TODO fix it
class HandshakeToHandshakeFragmentBits[T <: Data](dataType: T, bitsWidth: Int) extends Component {
  val io = new Bundle {
    val input = slave Handshake (dataType)
    val output = master Handshake Fragment(Bits(bitsWidth bit))
  }
  val counter = Counter((widthOf(dataType) - 1) / bitsWidth + 1)
  val inputBits = B(0, bitsWidth bit) ## toBits(io.input.data) //The cat allow to mux inputBits

  io.input.ready := counter.overflow
  io.output.last := counter.overflow
  io.output.valid := io.input.valid
  io.output.fragment := inputBits(counter * U(bitsWidth), bitsWidth bit)
  when(io.output.fire) {
    counter ++
  }
}

//class HandshakeFragmentToHandshakeFragmentBits[T <: Data](dataType: T, bitsWidth: Int) extends Component {
//  val io = new Bundle {
//    val input = slave Handshake Fragment (dataType)
//    val output = master Handshake Fragment(Bits(bitsWidth bit))
//  }
//
//  io.input.
//
//}




