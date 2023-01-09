package spinal.tester.code

import spinal.core._

class Ranged{
  def of(size : Int) = {

  }
}

class RangedFlat extends Bundle{
  val regType = Bits()
}
class RangedComp extends Bundle{
  val value = Bits()
}

class MyRanged extends Ranged{
  val TMP = of(64)
}
class F32 extends Bundle{
  val toto = Bits(7 bit)
}
