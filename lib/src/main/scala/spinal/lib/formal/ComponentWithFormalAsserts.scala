package spinal.lib.formal

import spinal.core.formal.anyseq
import spinal.core.{Bool, Component, Data, MultiData, True, assert, Vec}
import spinal.lib.IMasterSlave

/**
 * This implements formal assertions based on the inputs and outputs of a component.
 */
class ComponentWithFormalAsserts extends Component with HasFormalAsserts {

  override def asFormalDut() = {
    formalConfigureForTest()
    super.asFormalDut()
  }

  private def flattenIO(): Seq[Data] = {
    def hasDirectionnSpecifier(io: Data) = io match {
      case io: IMasterSlave => (io.isSlaveInterface || io.isMasterInterface)
      case _ => !io.isDirectionLess
    }

    def recursiveWalk(io: Data): Seq[Data] = io match {
      case io: MultiData => if (hasDirectionnSpecifier(io)) Seq(io) else io.elements.map(_._2).toSeq.flatMap(recursiveWalk)
      case _ => Seq(io)
    }

    getGroupedIO(true).flatMap(recursiveWalk)
  }

  /**
   * Use anyseq on any input signals to this component which do not have existing assignments.
   */
  def anyseq_inputs(): Unit = {
    getAllIo.filter(_.isInput).filter(_.dlcIsEmpty).foreach(anyseq)
  }

  override lazy val formalValidInputs: Bool = {
    // Go through our IO and find annotated inputs. If all of those are valid, the inputs to the component are typically
    // valid.
    Vec(flattenIO().map({
      case io: FormalMasterSlave =>
        if (io.asIMasterSlave.isMasterInterface) io.formalIsConsumerValid()
        else if (io.asIMasterSlave.isSlaveInterface) io.formalIsProducerValid()
        else if (io.isInput) io.formalIsValid()
        else {
          assert(false)
          True
        }
      case io: FormalBundle => if (io.isInput) io.formalIsValid() else True
      case _ => True
    })).asBits.andR
  }

  /**
   * Iterate through all the output IOs and assert/assum each is valid if it is driven by this component.
   * @param useAssumes Whether or not to assume the outputs are valid
   */
  protected def formalCheckOutputs()(implicit useAssumes: Boolean = false): Unit = {
    flattenIO().foreach({
      case io: FormalMasterSlave =>
        if (io.asIMasterSlave.isMasterInterface) assertOrAssume(io.formalIsProducerValid())
        else assertOrAssume(io.formalIsConsumerValid())
      case io: FormalBundle => if (io.isOutput) assertOrAssume(io.formalIsValid())
      case _ => True
    })
  }

  override protected def formalChecks()(implicit useAssumes: Boolean = false): Unit = {
    // By default we assume all children of this component are valid so we are only testing the logic around the
    // given component itself. Override to change this behavior. In particular when the component uses library components
    // which are tested directly, this tends to save a lot of time
    HasFormalAsserts.formalAssertsChildren(this, assumesInputValid = useAssumes, useAssumes = true)
    formalCheckOutputs()
  }
}
