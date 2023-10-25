package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc._
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer

case class Interleaver(blockSize : Int, ratio : Int, sel : Int) extends Area{
  val up = Node.slave()
  val down = Node.master()

  def at(mapping: AddressMapping) = up.at(InterleavedMapping(mapping, blockSize, ratio, sel))
  def at(base : BigInt, size : BigInt) = up.at(InterleavedMapping(SizeMapping(base, size), blockSize, ratio, sel))

  val transformer = InterleaverTransformer(blockSize, ratio, sel)
  new MemoryConnection {
    override def up = Interleaver.this.up
    override def down = Interleaver.this.down
    override def transformers = List(transformer)
    override def mapping = InterleavedMapping(
      mapping = SizeMapping(0, BigInt(1) << Interleaver.this.up.m2s.parameters.addressWidth),
      blockSize = blockSize,
      ratio = ratio,
      sel = sel
    )
    populate()
  }

  val logic = Fiber build new Area{
    assert(isPow2(ratio))
    assert(sel < ratio)
    val patternBits = log2Up(ratio)

    down.m2s.proposed load up.m2s.proposed.copy(addressWidth = up.m2s.proposed.addressWidth-patternBits)
    up.m2s.supported load down.m2s.supported.copy(addressWidth = down.m2s.supported.addressWidth+patternBits)
    down.m2s.parameters load up.m2s.parameters.copy(addressWidth = up.m2s.parameters.addressWidth-patternBits)
    up.s2m.from(down.s2m)

    assert(!down.bus.p.withBCE)
    down.bus << up.bus
    down.bus.a.address.removeAssignments() := transformer(up.bus.a.address)
  }
}
