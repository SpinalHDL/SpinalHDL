package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc._
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer

case class Interleaver(blockSize : Int, ratio : Int, sel : Int) extends Area{
  val up = Node.slave()
  val down = Node.master()

  val addressMappings = ArrayBuffer[(Connection, BigInt)]()
  def at(mapping: AddressMapping) = up.at(InterleavedMapping(mapping, blockSize, ratio, sel))
  def at(address : BigInt) = new{
    def of(m : Node): Unit ={
      val c = new Connection(m, up)
      addressMappings += c -> address
    }
  }

  val transformer = InterleaverTransformer(blockSize, ratio, sel)
  new MemoryConnection {
    override def m = up
    override def s = down
    override def transformers = List(transformer)
    override def mapping = InterleavedMapping(
      mapping = SizeMapping(0, BigInt(1) << up.m2s.parameters.addressWidth),
      blockSize = blockSize,
      ratio = ratio,
      sel = sel
    )
    override def sToM(down: MemoryTransfers, args: MappedNode) = down
    populate()
  }

  val logic = Fiber build new Area{
    assert(isPow2(ratio))
    assert(sel < ratio)
    val patternBits = log2Up(ratio)

    down.m2s.proposed load up.m2s.proposed.copy(addressWidth = up.m2s.proposed.addressWidth-patternBits)
    up.m2s.supported load down.m2s.supported.copy(addressWidth = down.m2s.supported.addressWidth+patternBits)
    for((c,a) <- addressMappings) c.mapping.value load {
      InterleavedMapping(SizeMapping(a, BigInt(1) << up.m2s.supported.addressWidth), blockSize, ratio, sel)
    }
    down.m2s.parameters load up.m2s.parameters.copy(addressWidth = up.m2s.parameters.addressWidth-patternBits)
    up.s2m.from(down.s2m)

    assert(!down.bus.p.withBCE)
    down.bus << up.bus
    down.bus.a.address.removeAssignments() := transformer(up.bus.a.address)
  }
}
