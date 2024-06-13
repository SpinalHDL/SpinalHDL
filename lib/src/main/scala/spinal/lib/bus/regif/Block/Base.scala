package spinal.lib.bus.regif

import spinal.lib.BigIntRicher

class Section(val max: Int, val min: Int){
  override def toString(): String = {
    if(this.max == this.min) {
      s"[${this.min}]"
    } else {
      s"[${this.max}:${this.min}]"
    }
  }
}

object Section{
  def apply(x: Range): Section = new Section(x.max, x.min)
  implicit def tans(x: Range) : Section = Section(x)
}

case class GrpTag(id: Int, name: String){
  override def toString: String = s"$id|$name"
}

case class ReuseTag(id: Int, partName: String, baseAddr: BigInt = 0, instName: String = ""){
  override def toString: String = s"$id|$partName|0x${baseAddr.hexString()}"

  def offset(x: BigInt): BigInt = x - baseAddr
}