package spinal.tester.scalatest

import spinal.core._
import spinal.lib.crypto.symmetric._



/******************************************************************************
  * DES
  */
class DESBlockTester extends Component {

    val g    = DESBlockGenerics()
    val gIO  = SymmetricCryptoBlockGeneric( keyWidth    = g.keyWidth + g.keyWidthParity,
                                            blockWidth  = g.blockWidth,
                                            useEncDec   = true)

    val io  = new SymmetricCryptoBlockIO(gIO)
    val des = new DESBlock(g)

    des.io <> io
}

class DESBlockCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "DESTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/crypto/symmetric/DES_Block"
  override def createToplevel: Component = new DESBlockTester
  override def backendConfig(config: SpinalConfig) : SpinalConfig = {
       config.copy(defaultClockDomainFrequency  = FixedFrequency(50 MHz),
      defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW))
  }
}



/******************************************************************************
  * Triple DES
  */
class TripleDESBlockTester extends Component {

  val gDES = DESBlockGenerics()
  val gIO  = SymmetricCryptoBlockGeneric(keyWidth    = ((gDES.keyWidth.value + gDES.keyWidthParity.value) * 3) bits,
                                         blockWidth  = gDES.blockWidth,
                                         useEncDec   = true)

  val io = new SymmetricCryptoBlockIO(gIO)

  val des3 = new TripleDESBlock()
  des3.io <> io
}


class TripleDESBlockCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "TripleDESTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/crypto/symmetric/TripleDES"
  override def createToplevel: Component = new TripleDESBlockTester
  override def backendConfig(config: SpinalConfig) : SpinalConfig = {
    config.copy(defaultClockDomainFrequency  = FixedFrequency(50 MHz),
      defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW))
  }
}


