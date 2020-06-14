package spinal.lib.com.eth

import spinal.core._
import spinal.lib._



case class MacTxManagedStreamFifoCc[T <: Data](payloadType : HardType[T],
                                          depth : Int,
                                          pushCd : ClockDomain,
                                          popCd : ClockDomain) extends Component {
  assert(isPow2(depth))


  val ram = Mem(payloadType, depth)
  val ptrWidth = ram.addressWidth + 1

  val io = new Bundle {
    val push = new Bundle {
      val stream = slave(Stream(payloadType))
      val clear = in Bool()
      val commit = in Bool()
      val availability = out UInt (ptrWidth bits)
    }

    val pop = new Bundle {
      val stream = master(Stream(payloadType))
      val clear = in Bool()
      val redo = in Bool()
      val commit = in Bool()
    }
  }


  val popToPushGray = Bits(ptrWidth bits)
  val pushToPopGray = Bits(ptrWidth bits)

  def isFull(a: Bits, b: Bits) = a(ptrWidth - 1 downto ptrWidth - 2) === ~b(ptrWidth - 1 downto ptrWidth - 2) && a(ptrWidth - 3 downto 0) === b(ptrWidth - 3 downto 0)
  def isEmpty(a: Bits, b: Bits) = a === b

  val push = pushCd on new Area {
    val currentPtr, oldPtr = Reg(UInt(ptrWidth bits))
    val popPtrGray = BufferCC(popToPushGray)
    pushToPopGray := RegNext(toGray(oldPtr))

    io.push.stream.ready := !isFull(toGray(currentPtr), popPtrGray)
    when(io.push.stream.fire) {
      ram(currentPtr.resized) := io.push.stream.payload
      currentPtr := currentPtr + 1
    }

    io.push.availability := depth - (currentPtr - fromGray(popPtrGray))

    when(io.push.commit) {
      oldPtr := currentPtr
    }

    when(io.push.clear){
      currentPtr := 0
      oldPtr := 0
    }
  }

  val pop = popCd on new Area {
    val currentPtr, oldPtr = Reg(UInt(ptrWidth bits))
    val pushPtrGray = BufferCC(pushToPopGray)
    val popPtrGray = toGray(oldPtr)
    popToPushGray := RegNext(popPtrGray)


    val cmd = Stream(ram.addressType())
    cmd.valid := !isEmpty(toGray(currentPtr), pushPtrGray) && !io.pop.redo
    cmd.payload := currentPtr.resized

    val commitPtr = RegNextWhen(currentPtr, io.pop.stream.fire)

    io.pop.stream << ram.streamReadSync(cmd).throwWhen(io.pop.redo)


    when(cmd.fire){
      currentPtr := currentPtr + 1
    }
    when(io.pop.redo){
      currentPtr := oldPtr
    }
    when(io.pop.commit){
      oldPtr := commitPtr
    }
    when(io.pop.clear){
      currentPtr := 0
      oldPtr := 0
    }
  }
}

case class MacTxBuffer(pushCd : ClockDomain,
                       popCd : ClockDomain,
                       pushWidth : Int,
                       popWidth : Int,
                       byteSize : Int,
                       lengthMax : Int) extends Component {
  assert(isPow2(byteSize))
  assert(isPow2(pushWidth / popWidth))
  assert(popWidth <= pushWidth)

  val ptrWidth = log2Up(byteSize * 8 / pushWidth) + 1

  val io = new Bundle {
    val push = new Bundle {
      val stream = slave(Stream(Bits(pushWidth bits)))
      val clear = in Bool()
      val availability = out UInt (ptrWidth bits)
    }

    val pop = new Bundle {
      val stream = master(Stream(Fragment(Bits(popWidth bits))))
      val clear = in Bool()
      val redo = in Bool()
      val commit = in Bool()
    }
  }


  val fifo = MacTxManagedStreamFifoCc(
    payloadType = Bits(pushWidth bits),
    depth = byteSize * 8 / pushWidth,
    pushCd = pushCd,
    popCd = popCd
  )

  fifo.io.push.clear := io.push.clear
  fifo.io.pop.clear := io.pop.clear

  val wordCountMax = (lengthMax+pushWidth-1)/pushWidth

  io.push.availability := fifo.io.push.availability

  val push = pushCd on new Area {
    fifo.io.push.stream << io.push.stream

    val commit = RegNext(False)
    fifo.io.push.commit := commit


    val State = new SpinalEnum{
      val LENGTH, DATA = newElement()
    }
    val state = Reg(State())
    val length = Reg(UInt(log2Up(lengthMax+1) bits))
    val wordCountMinusOne = length-1 >> log2Up(pushWidth)
    val wordCounter = Reg(UInt(log2Up(wordCountMax) bits))
    switch(state){
      is(State.LENGTH){
        wordCounter := 0
        when(fifo.io.push.stream.fire){
          state := State.DATA
          length := io.push.stream.payload.asUInt.resized
        }
      }
      is(State.DATA){
        when(io.push.stream.fire){
          wordCounter := wordCounter + 1
          when(wordCounter === wordCountMinusOne){
            commit := True
            state := State.LENGTH
          }
        }
      }
    }

    when(io.push.clear){
      state := State.LENGTH
    }
  }

  val pop = popCd on new Area {
    val State = new SpinalEnum{
      val LENGTH, DATA, WAIT = newElement()
    }
    val state = RegInit(State.LENGTH())
    val length = Reg(UInt(log2Up(lengthMax+1) bits))
    val lengthMinusOne = length-1
    val wordCounter = Reg(UInt(log2Up(wordCountMax) bits))
    val wordCountEndAt = lengthMinusOne >> log2Up(pushWidth)
    val spliterEndAt = lengthMinusOne(log2Up(pushWidth)-1 downto log2Up(popWidth))
    val ratio = pushWidth/popWidth
    val spliter = Reg(UInt(log2Up(ratio) bits))


    fifo.io.pop.commit := io.pop.commit
    fifo.io.pop.redo := io.pop.redo
    fifo.io.pop.stream.ready := False

    io.pop.stream.valid := False
    io.pop.stream.last := False
    io.pop.stream.payload := fifo.io.pop.stream.payload.subdivideIn(popWidth bits).read(spliter)

    switch(state){
      is(State.LENGTH){
        wordCounter := 0
        spliter := 0
        when(fifo.io.pop.stream.valid){
          state := State.DATA
          length := fifo.io.pop.stream.payload.asUInt.resized
          fifo.io.pop.stream.ready := True
        }
      }
      is(State.DATA) {
        io.pop.stream.valid := True
        when(io.pop.stream.ready) {
          spliter := spliter + 1
          when(wordCounter === wordCountEndAt && spliter === spliterEndAt) {
            state := State.WAIT
            fifo.io.pop.stream.ready := True
            io.pop.stream.last := True
          }
          when(spliter === spliter.maxValue) {
            wordCounter := wordCounter + 1
            fifo.io.pop.stream.ready := True
          }
        }
      }
      is(State.WAIT){
        when(io.pop.commit){
          state := State.LENGTH
        }
      }
    }

    when(io.pop.redo || io.pop.clear){
      state := State.LENGTH
    }
  }
}

case class MacTxCrc(dataWidth : Int) extends Component{
  val io = new Bundle{
    val input = slave(Stream(Fragment(Bits(dataWidth bits))))
    val output = master(Stream(Fragment(Bits(dataWidth bits))))
    val clear = in Bool()
  }

  val emitCrc = Reg(Bool()) setWhen(io.input.lastFire) clearWhen(io.output.lastFire)
  val counter = Reg(UInt(log2Up(32/dataWidth) bits))

  val crc = Crc(CrcKind.Crc32, dataWidth)
  crc.io.input << io.input.toFlowFire.toFlowOfFragment
  crc.io.flush := io.output.lastFire

  io.output.last := False
  when(!emitCrc) {
    io.output.valid := io.input.valid
    io.output.payload := io.input.payload
    io.input.ready := io.output.ready
  } otherwise {
    io.input.ready := False
    io.output.valid := True
    io.output.payload := crc.io.result.subdivideIn(dataWidth bits).read(counter)
    when(counter === counter.maxValue) {
      io.output.last := True
      when(io.output.ready) {
        emitCrc := False
      }
    }
    when(io.output.ready) {
      counter := counter + 1
    }
  }

  when(io.clear){
    emitCrc := False
    counter := 0
  }
}

case class MacTxHeader(dataWidth : Int) extends Component{
  val io = new Bundle{
    val input = slave(Stream(Fragment(Bits(dataWidth bits))))
    val output = master(Stream(Fragment(Bits(dataWidth bits))))
    val clear = in Bool()
  }

  val header = B"x555555555555555D"
  val headerWords = widthOf(header)/dataWidth
  val state = Reg(UInt(log2Up(headerWords + 1) bits))
  io.output.valid := io.input.valid
  io.output.last := False
  io.input.ready := False
  when(state === headerWords){
    io.input.ready := io.output.ready
    io.output.payload := io.input.payload
    io.output.last := io.input.last
  } otherwise {
    io.output.payload := header.subdivideIn(dataWidth bits).reverse.read(state.resized)
    when(io.output.fire) {
      state := state + 1
    }
  }
  when(io.clear || io.input.lastFire){
    state := 0
  }
}

case class MacTxPadder(dataWidth : Int) extends Component{
  val io = new Bundle{
    val input = slave(Stream(Fragment(Bits(dataWidth bits))))
    val output = master(Stream(Fragment(Bits(dataWidth bits))))
    val clear = in Bool()
  }

  val byteCount = 64-4
  val cycles = (byteCount*8 + dataWidth-1)/dataWidth
  val counter = Reg(UInt(log2Up(cycles) bits))
  val ok = counter === cycles-1
  val fill = counter =/= 0 && (io.input.first || !io.input.valid)

  when(!ok && (counter =/= 0 || io.output.fire)){
    counter := counter + 1
  }
  when(io.output.lastFire){
    counter := 0
  }
  val first =  RegNextWhen(io.input.last, io.input.fire)
  io.output << io.input.haltWhen(counter =/= 0 && first)
  when(!ok){
    io.output.last := False
  }
  when(fill){
    io.output.valid := True
    io.output.payload := 0
    io.output.last := ok
  }

  when(io.clear){
    first := True
    counter := 0
  }

}

case class MacTxInterFrame(dataWidth : Int) extends Component{
  val io = new Bundle{
    val input = slave(Stream(Fragment(Bits(dataWidth bits))))
    val output = master(Stream(Fragment(Bits(dataWidth bits))))
    val clear = in Bool()
  }

  val byteCount = 12
  val cycles = (byteCount*8 + dataWidth-1)/dataWidth
  val counter = Counter(cycles)
  when(counter =/= 0 || io.input.valid.fall(False)){
    counter.increment()
  }
  io.output << io.input.haltWhen(counter =/= 0)
}