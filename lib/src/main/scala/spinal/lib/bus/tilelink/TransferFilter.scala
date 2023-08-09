package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.bus.tilelink.fabric._
import spinal.lib.logic.{Masked, Symplify}
import spinal.lib.system.tag.{MappedNode, MappedTransfers, MemoryConnection, MemoryTransfers}
import scala.collection.Seq

import scala.collection.mutable.ArrayBuffer

class TransferFilter(unp : NodeParameters, dnp : NodeParameters, spec : Seq[MappedTransfers]) extends Component{
  val io = new Bundle {
    val up = slave(Bus(unp))
    val down = master(Bus(dnp))
  }
  if(io.up.p.withBCE) assert(io.up.p.sinkWidth == io.down.p.sinkWidth + 1)
  val ipEmits = unp.m.emits

  val instruction = io.up.a.param ## io.up.a.size ## io.up.a.opcode
  val addrKey = io.up.a.address.asBits
  val argsHits = spec.map(s => new Area{
    val falseTerms, trueTerms = ArrayBuffer[Masked]()
    val st = s.transfers.asInstanceOf[M2sTransfers]
    val addressMasked = AddressMapping.terms(s.mapping, widthOf(io.up.a.address)).map(_.shiftedLeft(3+3+widthOf(io.up.a.size)))
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

    val argHit = Symplify(instruction, trueTerms, falseTerms)
    val addrHit = AddressMapping.decode(addrKey, s.mapping)
    val hit = argHit && addrHit
  })

  val hit = argsHits.map(_.hit).orR

  val start = io.up.a.fire && io.up.a.isLast() && !hit
  val errored = RegInit(False) setWhen(start)
  val doIt = RegInit(False) setWhen(errored && !io.down.d.valid && io.down.d.isFirst()) clearWhen(io.up.d.fire && io.up.d.isLast())
  val opcode = RegNextWhen(io.up.a.opcode, start)
  val size = RegNextWhen(io.up.a.size, start)
  val source = RegNextWhen(io.up.a.source, start)
  val counter = Reg(UInt(log2Up(unp.sizeBytes) bits))

  io.down.a << io.up.a.haltWhen(errored || doIt).throwWhen(!hit)
  io.up.d << io.down.d.haltWhen(doIt)
  io.up.d.sink.removeAssignments() := io.down.d.sink.resized
  when(doIt){
    io.up.d.valid := True
    io.up.d.opcode := opcode.mux(
      Opcode.A.PUT_FULL_DATA -> Opcode.D.ACCESS_ACK(),
      Opcode.A.PUT_PARTIAL_DATA -> Opcode.D.ACCESS_ACK(),
      Opcode.A.GET -> Opcode.D.ACCESS_ACK_DATA(),
      Opcode.A.ACQUIRE_BLOCK -> Opcode.D.GRANT_DATA(),
      Opcode.A.ACQUIRE_PERM -> Opcode.D.GRANT()
    )
    io.up.d.size := size
    io.up.d.source := source
    io.up.d.denied := True
    io.up.d.corrupt := False
    io.up.d.param := 0
    if(unp.withBCE) io.up.d.sink.msb := True
    when(io.up.d.ready) {
      counter := counter + 1
      when(io.up.d.isLast()){
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


/*
  val falseTerms, trueTerms = ArrayBuffer[Masked]()
  for(s <- spec){
    val st = s.transfers.asInstanceOf[M2sTransfers]
    val addressMasked = AddressMapping.terms(s.mapping, widthOf(io.up.a.address)).map(_.shiftedLeft(3+3+widthOf(io.up.a.size)))
    def simple(opcode : Int, getter : M2sTransfers => SizeRange): Unit = simpleImpl(opcode, getter(ipEmits), getter(st))
    def simpleImpl(opcode : Int, is: SizeRange, os: SizeRange): Unit ={
      is.foreach{ size =>
        val term = Masked(opcode | (log2Up(size) << 3), (1 << io.up.p.sizeWidth+3)-1)
        val target = if(os.contains(size)) trueTerms else falseTerms
        target ++= addressMasked.map(_ fuse term)
      }
    }
    def aquire(param : Int, getter : M2sTransfers => SizeRange): Unit = aquireImpl(param, getter(ipEmits), getter(st))
    def aquireImpl(param : Int, is: SizeRange, os: SizeRange): Unit ={
      ipEmits.acquireB.foreach{ size =>
        val term = Masked(6 | (log2Up(size) << 3) | (param << 3 + io.up.p.sizeWidth), (1 << io.up.p.sizeWidth+3+3)-2) //-2 to cover opcode 6 and 7
        val target = if(os.contains(size)) trueTerms else falseTerms
        target ++= addressMasked.map(_ fuse term)
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
  }

  val instruction = io.up.a.address ## io.up.a.param ## io.up.a.size ## io.up.a.opcode
  val hit = Symplify.trueAndDontCare(instruction, trueTerms, falseTerms)
 */