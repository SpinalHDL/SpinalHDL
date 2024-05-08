package spinal.lib.bus.regif

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

case class grpTag(id: Int, name: String)
