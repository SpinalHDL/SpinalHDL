package spinal.lib.dsptool

case class QFormat(width: Int, fraction: Int, signed: Boolean) {
  val nonFraction: Int = width - fraction
  val amplify: Int = if(signed) (nonFraction - 1) else nonFraction
  val capcity: Double = scala.math.pow(2, width)
  val halfCapcity: Double = capcity/2
  val resolution: Double  = 1/scala.math.pow(2, fraction)
  val minValue: Double = if(signed) - halfCapcity*resolution else 0
  val maxValue: Double = if(signed) (halfCapcity - 1)*resolution else (capcity -1)*resolution

  def alignHex: Int = scala.math.ceil(width/4.0).toInt
  def alignOct: Int = scala.math.ceil(width/3.0).toInt

  def *(right: QFormat): QFormat = {
    QFormat(this.width + right.width, this.fraction + right.fraction, this.signed | right.signed)
  }

  def +(right: QFormat): QFormat = {
    val fraction = scala.math.max(this.fraction, right.fraction)
    val noneFraction = scala.math.max(this.nonFraction, right.nonFraction)
    QFormat(noneFraction + fraction, fraction, this.signed | right.signed)
  }

  def -(right: QFormat): QFormat = this.+(- right)

  def unary_- : QFormat = {
    if(signed) this else QFormat(this.width+1, this.fraction, true)
  }

  def >>(n: Int): QFormat = {
    val newFrac  = this.fraction + n
    val newWidth = if(this.amplify > n) this.width else (this.width + n - this.amplify )
    this.copy(newWidth, newFrac)
  }

  def <<(n: Int): QFormat = {
    val newFrac  = if(this.fraction>n) (this.fraction - n) else 0
    val newWidth = if(this.fraction>n) this.width else (this.width + n - this.fraction)
    this.copy(newWidth, newFrac)
  }

  override def toString : String = {
    s"""${getClass().getName.split('.').last} : Q($width,$fraction,${if(signed) "signed" else "unsigned"})
resolution: $resolution
maxValue  : $maxValue
minValue  : $minValue"""
  }
}

object UQ {
  def apply(width: Int, fraction: Int): QFormat = new QFormat(width, fraction,false)
}

object SQ {
  def apply(width: Int, fraction: Int): QFormat = new QFormat(width, fraction,true)
}
