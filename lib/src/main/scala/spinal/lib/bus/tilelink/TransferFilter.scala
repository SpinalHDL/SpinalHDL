package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib._
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinal.lib.bus.tilelink.fabric._
import spinal.lib.logic.{Masked, Symplify}
import spinal.lib.system.tag.{MappedNode, MappedTransfers, MemoryConnection, MemoryTransfers}

import scala.collection.mutable.ArrayBuffer

class TransferFilter(unp : NodeParameters, dnp : NodeParameters, spec : Seq[MappedTransfers]) extends Component{
  val io = new Bundle {
    val up = slave(Bus(unp))
    val down = master(Bus(dnp))
  }
  if(io.up.p.withBCE) assert(io.up.p.sinkWidth == io.down.p.sinkWidth + 1)
  val ipEmits = unp.m.emits
  val addressHits = spec.map(_.mapping.map(_.hit(io.up.a.address)).orR).asBits()
  val instruction = io.up.a.param ## io.up.a.size ## io.up.a.opcode
  val argsHits = spec.map{spec =>
    val st = spec.transfers.asInstanceOf[M2sTransfers]
    val falseTerms, trueTerms = ArrayBuffer[Masked]()
    def simple(opcode : Int, getter : M2sTransfers => SizeRange): Unit = simpleImpl(opcode, getter(ipEmits), getter(st))
    def simpleImpl(opcode : Int, is: SizeRange, os: SizeRange): Unit ={
      is.foreach{ size =>
        val term = Masked(opcode | (log2Up(size) << 3), (1 << io.up.p.sizeWidth+3)-1)
        val target = if(os.contains(size)) trueTerms else falseTerms
        target += term
      }
    }
    def aquire(param : Int, getter : M2sTransfers => SizeRange): Unit = aquireImpl(param, getter(ipEmits), getter(st))
    def aquireImpl(param : Int, is: SizeRange, os: SizeRange): Unit ={
      ipEmits.acquireB.foreach{ size =>
        val term = Masked(6 | (log2Up(size) << 3) | (param << 3 + io.up.p.sizeWidth), (1 << io.up.p.sizeWidth+3+3)-2) //-2 to cover opcode 6 and 7
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

  val start = io.up.a.valid && io.up.a.isLast() && !hit
  val errored = RegInit(False) setWhen(start)
  val size = RegNextWhen(io.up.a.size, start)
  val source = RegNextWhen(io.up.a.source, start)
  val beats = RegNextWhen(sizeToBeatMinusOne(io.up.p, io.up.a.size), start)
  val counter = Reg(beats)

  io.down.a << io.up.a.haltWhen(errored).throwWhen(!hit)
  io.up.d << io.down.d.haltWhen(errored)
  io.up.d.sink.removeAssignments() := io.down.d.sink.resized
  when(errored){
    io.up.d.valid := True
    io.up.d.size := size
    io.up.d.source := source
    io.up.d.denied := True
    io.up.d.corrupt := False
    if(unp.withBCE) io.up.d.sink.msb := True
    when(io.up.d.ready) {
      counter := counter + 1
      when(counter === beats){
        errored := False
      }
    }
  } otherwise {
    counter := 0
  }
  if(unp.withBCE) {
    io.down.b >> io.up.b
    io.down.c << io.up.c
    io.down.e.arbitrationFrom(io.up.e.throwWhen(io.up.e.sink.msb))
    io.down.e.sink := io.up.e.sink.resized
  }
}