package spinal.lib.formal

import spinal.lib.IMasterSlave
import scala.language.{higherKinds, implicitConversions}

/**
 * For bundles which implement IMasterSlave, this can be used to add logic which informs on if the master or slave of
 * that bundle are using it correctly.
 */
trait FormalMasterSlave extends FormalData { self: IMasterSlave =>
  /**
   * @return True if and only if the driving signals are valid
   */
  def formalIsProducerValid() : Seq[FormalProperty]
  /**
   * @return True if and only if the response signals are valid
   */
  def formalIsConsumerValid() : Seq[FormalProperty] = Seq()

  override def formalIsStateValid() : Seq[FormalProperty] = formalIsConsumerValid() ++ formalIsProducerValid()

  def asIMasterSlave = self
}
