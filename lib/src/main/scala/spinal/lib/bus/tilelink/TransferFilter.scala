package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.fiber.Elab
import spinal.lib._
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinal.lib.logic.{Masked, Symplify}
import spinal.lib.system.tag.{MappedNode, MappedTransfers, MemoryConnection, MemoryTransfers}

import scala.collection.mutable.ArrayBuffer

class TransferFilter(ip : NodeParameters, op : NodeParameters, spec : Seq[MappedTransfers]) extends Component{
  val io = new Bundle {
    val input = slave(Bus(ip))
    val output = master(Bus(op))
  }
  if(io.input.p.withBCE) assert(io.input.p.sinkWidth == io.output.p.sinkWidth + 1)
  val ipEmits = ip.m.emits
  val addressHits = spec.map(_.mapping.hit(io.input.a.address)).asBits()
  val instruction = io.input.a.param ## io.input.a.size ## io.input.a.opcode
  val argsHits = spec.map{spec =>
    val st = spec.transfers.asInstanceOf[M2sTransfers]
    val falseTerms, trueTerms = ArrayBuffer[Masked]()
    def simple(opcode : Int, getter : M2sTransfers => SizeRange): Unit = simpleImpl(opcode, getter(ipEmits), getter(st))
    def simpleImpl(opcode : Int, is: SizeRange, os: SizeRange): Unit ={
      is.foreach{ size =>
        val term = Masked(opcode | (log2Up(size) << 3), (1 << io.input.p.sizeWidth+3)-1)
        val target = if(os.contains(size)) trueTerms else falseTerms
        target += term
      }
    }
    def aquire(param : Int, getter : M2sTransfers => SizeRange): Unit = aquireImpl(param, getter(ipEmits), getter(st))
    def aquireImpl(param : Int, is: SizeRange, os: SizeRange): Unit ={
      ipEmits.acquireB.foreach{ size =>
        val term = Masked(6 | (log2Up(size) << 3) | (param << 3 + io.input.p.sizeWidth), (1 << io.input.p.sizeWidth+3+3)-2) //-2 to cover opcode 6 and 7
        val target = if(os.contains(size)) trueTerms else falseTerms
        target += term
      }
    }

    simple(0, _.putFull)
    simple(1, _.putPartial)
    simple(2, _.arithmetic)
    simple(3, _.logical)
    simple(4, _.get)
    simple(5, _.hint)
    aquire(Param.Grow.NtoB, _.acquireB)
    aquire(Param.Grow.NtoT, _.acquireT)
    aquire(Param.Grow.BtoT, _.acquireT)
    Symplify(instruction, trueTerms, falseTerms)
  }.asBits

  val hits = addressHits & argsHits
  val hit = hits.orR

  val start = io.input.a.valid && io.input.a.isLast() && !hit
  val errored = RegInit(False) setWhen(start)
  val size = RegNextWhen(io.input.a.size, start)
  val source = RegNextWhen(io.input.a.source, start)
  val beats = RegNextWhen(sizeToBeatMinusOne(io.input.p, io.input.a.size), start)
  val counter = Reg(beats)

  io.output.a << io.input.a.haltWhen(errored).throwWhen(!hit)
  io.input.d << io.output.d.haltWhen(errored)
  io.input.d.sink.removeAssignments() := io.output.d.sink.resized
  when(errored){
    io.input.d.valid := True
    io.input.d.size := size
    io.input.d.source := source
    io.input.d.denied := True
    io.input.d.corrupt := False
    if(ip.withBCE) io.input.d.sink.msb := True
    when(io.input.d.ready) {
      counter := counter + 1
      when(counter === beats){
        errored := False
      }
    }
  } otherwise {
    counter := 0
  }
  if(ip.withBCE) {
    io.output.b >> io.input.b
    io.output.c << io.input.c
    io.output.e.arbitrationFrom(io.input.e.throwWhen(io.input.e.sink.msb))
    io.output.e.sink := io.input.e.sink.resized
  }
}

class TransferFilterIntegrator()(implicit val i : Interconnect) extends Area{
  val up = i.createSlave()
  val down = i.createMaster()

  new MemoryConnection {
    override def m = up
    override def s = down
    override def mapping = DefaultMapping
    override def sToM(downs: MemoryTransfers, args: MappedNode) = downs
    populate()
  }

  val logic = Elab build new Area{
    down.m2s.proposed.load(up.m2s.proposed)
    up.m2s.supported.load(M2sSupport(
      transfers = up.m2s.proposed.transfers,
      addressWidth = down.m2s.supported.addressWidth,
      dataWidth = down.m2s.supported.dataWidth
    ))
    down.m2s.parameters.load(up.m2s.parameters)

    up.s2m.proposed.load(down.s2m.proposed)
    down.s2m.supported.load(up.s2m.supported)
    up.s2m.parameters.load(up.m2s.parameters.withBCE match {
      case true => S2mParameters(
        down.s2m.parameters.slaves :+ S2mAgent(
          name = TransferFilterIntegrator.this,
          sinkId = SizeMapping(1 << down.s2m.parameters.sinkWidth, 1 << down.s2m.parameters.sinkWidth),
          emits = S2mTransfers.none
        )
      )
      case false => S2mParameters.none()
    })

    val spec = MemoryConnection.getMemoryTransfers(up)
    val core = new TransferFilter(
      up.bus.p.node,
      down.bus.p.node,
      spec
    )
    core.io.input << up.bus
    core.io.output >> down.bus
  }
}
