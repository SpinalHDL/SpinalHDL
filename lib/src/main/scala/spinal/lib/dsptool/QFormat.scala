package spinal.lib.dsptool

case class QFormat(width: Int, fraction: Int, signed: Boolean, symmetric: Boolean = false ) {
  val noneFraction: Int = width - fraction
  val amp: Int = if(signed) (noneFraction - 1) else noneFraction
  val capcity: Double = if(signed) scala.math.pow(2, width) else scala.math.pow(2, width-1)
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
    val noneFraction = scala.math.max(this.noneFraction, right.noneFraction)
    QFormat(noneFraction + fraction, fraction, this.signed | right.signed)
  }

  def -(right: QFormat): QFormat = this.+(- right)

  def unary_- : QFormat = {
    if(signed) this else new QFormat(this.width+1, this.fraction, true)
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
  def apply(width: Int, fraction: Int, symmetric: Boolean): QFormat = new QFormat(width, fraction,false, symmetric)
}

object SQ {
  def apply(width: Int, fraction: Int): QFormat = new QFormat(width, fraction,true)
  def apply(width: Int, fraction: Int, symmetric: Boolean): QFormat = new QFormat(width, fraction,true, symmetric)
}
