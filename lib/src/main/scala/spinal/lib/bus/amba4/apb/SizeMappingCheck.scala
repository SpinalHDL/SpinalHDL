package spinal.lib.bus.amba4.apb

import spinal.lib._
import spinal.core._
import spinal.lib.bus.misc.SizeMapping

import scala.collection.mutable.ListBuffer

object SizeMappingCheck{
  def apply(maps: Seq[SizeMapping]) = {
    def x(b: BigInt, n: Int) = s"0x${b.hexString(n)}"
    def map2string(m: SizeMapping) = {
      val sa = m.base
      val ea = m.base + m.size
      s"[${x(ea, 32)} ~ ${x(sa, 32)}](${m.size.byteUnit()})"
    }
    val msg = new ListBuffer[String]()
    for(i1 <- maps.indices; i2 <- maps.indices if i1 != i2){ // fix when some SizeMappings are completely overlap.
      val m1 = maps(i1)
      val m2 = maps(i2)
      if(m1.overlap(m2)){
        msg.append(s"[Error] address overlap:  ${m2}:${map2string(m2)} -OVERLAP- ${m1}:${map2string(m1)}")
      }
    }
    maps.foreach{  map =>
      if (map.base % map.size != 0) {
        msg.append(f"[Error] mapping at 0x${map.base.hexString(32)} is not aligned to its size (0x${map.size.hexString(16)}) bytes)")
      }
    }
    assert(msg.isEmpty, s"\nAddress Overlap and Align check failed:\n" +msg.mkString("\n") + "\nCheck please!")
  }
}
