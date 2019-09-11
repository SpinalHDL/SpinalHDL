package spinal.lib.dsptool

/**
  * Fixnum
  * @example{{{ val x = FixData(-3.785333,SQ(8,4))}}}
  * @return {{{FixData: -3.8125, Quantized by QFormat: Q(8,4,signed) }}}
  *        x.bin => 11000011
  *        x.oct =>      103
  *        x.hex =>       c3
  */
case class FixData(data: Double, initq: QFormat, roundType: RoundType = RoundToInf)
                  (implicit button: FixSwitch = FixSwitchOn.fixButton ) {

  private var q: QFormat = initq
  private var v: Double = data
  fixTo(initq)

  def value: Double = v
  def Q: QFormat    = q

  private def scalaValue = value / q.resolution

  def isSigned: Boolean = q.signed
  def isNegtive: Boolean = value < 0
  def sign: Int = if(isNegtive) -1 else 1

  def fixTo(qtag: QFormat) : FixData ={
    button match {
      case FixSwitchOff.fixButton => this
      case FixSwitchOn.fixButton  => {
        q = qtag
        val rounded = this.roundType match {
          case Ceil          => ceil
          case Floor         => floor
          case FloorToZero   => floorToZero
          case CeilToInf     => ceilToInf
          case RoundUp       => roundUp
          case RoundDown     => roundDown
          case RoundToZero   => roundToZero
          case RoundToInf    => roundToInf
          case _ => roundToInf
        }
        v = this.saturated(rounded * q.resolution)
        if(qtag.symmetric) this.symmetry
        this
      }
    }
  }

  private def abs   = scala.math.abs(this.scalaValue)
  private def ceil  = scala.math.ceil(this.scalaValue)
  private def floor = scala.math.floor(this.scalaValue)
  private def floorToZero = this.sign * scala.math.floor(this.abs)
  private def ceilToInf   = this.sign * scala.math.ceil(this.abs)
  private def roundUp     = scala.math.floor(this.scalaValue + 0.5)
  private def roundDown   = scala.math.ceil(this.scalaValue - 0.5)
  private def roundToZero = this.sign * scala.math.ceil(this.abs - 0.5)
  private def roundToInf  = this.sign * scala.math.floor(this.abs + 0.5)

  private def saturated(x: Double) = x match {
    case d if d > q.maxValue => q.maxValue
    case d if d < q.minValue => q.minValue
    case _ => x
    }

  private def symmetry = if(this.value == q.minValue) - q.maxValue else this.value

  def asLong: Long = this.value / q.resolution toLong
  def asLongPostive: Long = if(isNegtive) q.capcity.toLong + this.asLong else this.asLong

  def hex: String = s"%${q.alignHex}s".format(this.asLongPostive.toHexString).replace(' ','0')
  def bin: String = s"%${q.width}s".format(this.asLongPostive.toBinaryString).replace(' ','0')
  def oct: String = s"%${q.alignOct}s".format(this.asLongPostive.toOctalString).replace(' ','0')

  override def toString  = button match {
    case FixSwitchOn.fixButton  => getClass().getName().split('.').last +  s" : ${this.value}, Quantized by" +  s"\n${this.q}"
    case FixSwitchOff.fixButton => getClass().getName().split('.').last +  s" : ${this.value}, FixSwitchOff"
  }

  def *(right: FixData): FixData ={
    FixData(this.value * right.value, this.q * right.q, roundType)(button)
  }

  def +(right: FixData): FixData ={
    FixData(this.value + right.value, this.q + right.q, roundType)(button)
  }

  def -(right: FixData): FixData ={
    FixData(this.value - right.value, this.q - right.q, roundType)(button)
  }

  def unary_- : FixData = FixData(-this.value, -this.q, roundType)(button)

}

/**
  * IntToFixData
  * @example{{{ val x = toFixData(0xFFAE,SQ(8,4))}}}
  * @return {{{FixData: -5.125, QFormat: Q(8,4,signed) }}}
  * toFixData(322111, SQ(8,4)) => FixData: -8.0,   QFormat: Q(8,4,signed)
  * toFixData(322111, UQ(8,4)) => FixData: 7.9375, QFormat: Q(8,4,unsigned)
  * toFixData(-322111,SQ(8,4)) => FixData: -8.0,   QFormat: Q(8,4,signed)
  * toFixData(-322111,UQ(8,4)) => FixData: 0,      QFormat: Q(8,4,unsigned)
  * toFixData(-0x0f,  SQ(8,4)) => FixData: -0.9375,QFormat: Q(8,4,signed)
  */
object toFixData{
  def apply(value: Int, q: QFormat): FixData = {
    (value, q.signed) match {
      case (x, true) if x >= 0 => {
        val signedValue = if (value >= q.halfCapcity) value%q.capcity - q.capcity else value
        FixData(signedValue * q.resolution, q)
      }
      case (_, true)=> {
        val signedValue =  value
        FixData(signedValue * q.resolution, q)
      }
      case (x, false) if x >= 0 => {
        val signedValue =  value
        FixData(signedValue * q.resolution, q)
      }
      case (_, false)=> {
        FixData(0, q)
      }
    }
  }
}

