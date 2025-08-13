package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping

object Axi4Bridge{
  def getAxi4Config(p : NodeParameters, withAxi3 : Boolean): Axi4Config ={
    assert(!p.withBCE)
    assert(p.m.emits.isOnlyGetPut())
    Axi4Config(
      addressWidth = p.m.addressWidth,
      dataWidth    = p.m.dataWidth,
      idWidth      = p.m.sourceWidth,
      useLock      = false,
      useCache     = false,
      useQos       = false,
      useProt      = false,
      useRegion    = false,
      useAllStrb   = true,
      withAxi3     = withAxi3
    )
  }

  def getSupported(proposed : M2sSupport) = proposed.intersect(
    M2sTransfers.allGetPut
  )
}

class Axi4Bridge(p : NodeParameters, withAxi3 : Boolean = false) extends Component{
  val axiConfig = Axi4Bridge.getAxi4Config(p, withAxi3)
  val io = new Bundle{
    val up = slave port Bus(p)
    val down = master port Axi4(axiConfig)
  }

  val a = new Area {
    val ctxFull = False
    val halted = io.up.a.haltWhen(ctxFull)

    val ctx = ContextAsyncBufferFull(p.m.sourceWidth, io.up.p.size())
    ctx.io.bind(ctxFull, io.up.a, io.up.d)
    ctx.io.add.context := io.up.a.size

    val (cmdFork, dataFork) = StreamFork2(halted)
    val cmd = new Area {
      val filtred = cmdFork.takeWhen(cmdFork.isFirst())
      val buffered = filtred.pipelined(halfRate = true)
      val isGet = buffered.opcode === Opcode.A.GET

      io.down.aw.valid := buffered.valid && !isGet
      io.down.ar.valid := buffered.valid &&  isGet
      buffered.ready := isGet.mux(io.down.ar.ready, io.down.aw.ready)

      val len = buffered.sizeToBeatMinusOne().resize(axiConfig.lenWidth bits)
      val sizeMapping = (0 to log2Up(p.m.sizeBytes)).map(_ min log2Up(p.m.dataBytes))
      val sizePerBeat = sizeMapping.map(U(_, axiConfig.sizeWidth bits)).read(buffered.size)
      for (ax <- List(io.down.aw, io.down.ar)) {
        ax.addr := buffered.address
        ax.id := buffered.source
        ax.len := len
        ax.size := sizePerBeat
        ax.setBurstINCR()
      }

      io.down.aw.allStrb := buffered.opcode === Opcode.A.PUT_FULL_DATA
    }
    val data = new Area{
      val filtred = dataFork.takeWhen(dataFork.opcode === Opcode.A.PUT_FULL_DATA || dataFork.opcode === Opcode.A.PUT_PARTIAL_DATA)
      val buffer = filtred.pipelined(m2s = true)
      io.down.w.arbitrationFrom(buffer)
      io.down.w.data := buffer.data
      io.down.w.strb := buffer.mask
      io.down.w.last := buffer.isLast()
      if(withAxi3) io.down.w.id   := buffer.source
    }
  }

  val d = new Area{
    case class Ctx() extends Bundle{
      val source = io.up.p.source()
      val denied = Bool()
      val last = Bool()
    }

    val arbiter = StreamArbiterFactory().roundRobin.lambdaLock[Ctx](_.last).build(Ctx(), 2)
    arbiter.io.inputs(0).arbitrationFrom(io.down.b)
    arbiter.io.inputs(0).source  :=  io.down.b.id
    arbiter.io.inputs(0).denied  := !io.down.b.isOKAY()
    arbiter.io.inputs(0).last    :=  True

    arbiter.io.inputs(1).arbitrationFrom(io.down.r)
    arbiter.io.inputs(1).source  :=  io.down.r.id
    arbiter.io.inputs(1).denied  := !io.down.r.isOKAY()
    arbiter.io.inputs(1).last    :=  io.down.r.last

    a.ctx.io.query.id := io.up.d.source
    io.up.d.arbitrationFrom(arbiter.io.output)
    io.up.d.opcode := arbiter.io.chosen(0).mux(Opcode.D.ACCESS_ACK_DATA(), Opcode.D.ACCESS_ACK())
    io.up.d.param := 0
    io.up.d.source := arbiter.io.output.source
    io.up.d.sink := 0
    io.up.d.denied := arbiter.io.output.denied
    io.up.d.data := io.down.r.data
    io.up.d.corrupt := False
    io.up.d.size := a.ctx.io.query.context
  }
}


object Axi4BridgeGen extends App{
  var addressWidth = 32
  var dataWidth = 32
  var sourceWidth = 4
  var sizeWidth = 3
  assert(new scopt.OptionParser[Unit]("Axi4BridgeGen") {
    help("help").text("prints this usage text")
    opt[Int]("address-width") action { (v, c) => addressWidth = v }
    opt[Int]("data-width") action { (v, c) => dataWidth = v }
    opt[Int]("source-width") action { (v, c) => sourceWidth = v }
    opt[Int]("size-width") action { (v, c) => sizeWidth = v }
  }.parse(args, ()).isDefined)

  val sizeMax = 1<<(1<<(sizeWidth-1))
  SpinalVerilog(new Axi4Bridge(
    new M2sParameters(
      addressWidth = addressWidth,
      dataWidth = dataWidth,
      masters = List.fill(1)(
        M2sAgent(
          name = null,
          M2sSource(
            id = SizeMapping(0, 1<<sourceWidth),
            emits = M2sTransfers(
              get = SizeRange.upTo(sizeMax),
              putFull = SizeRange.upTo(sizeMax),
              putPartial = SizeRange.upTo(sizeMax)
            )
          )
        )
      )
    ).toNodeParameters()
  ))
}