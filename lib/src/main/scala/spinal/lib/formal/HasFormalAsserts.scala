package spinal.lib.formal

import spinal.core.formal.anyseq
import spinal.core.{Bool, Component, Composite, Data, MultiData, SpinalTag, True, Vec, assert, assume, cover, when}
import spinal.idslplugin.Location
import spinal.lib.IMasterSlave

import scala.collection.mutable
import scala.ref.WeakReference
import scala.util.Try

object AssertionLevel extends Enumeration {
  type Level = Value
  val None, Assertion, Assumption = Value
}

class HasFormalAssertsTag(val formalAsserts : HasFormalAsserts) extends SpinalTag


trait HasFormalAsserts {
  if (!this.isInstanceOf[Component]) {
    Component.current.addTag(new HasFormalAssertsTag(this))
  }

  private val OwningComponent = WeakReference(Component.current)

  private var CurrentAssertionLevel = AssertionLevel.None
  private var CurrentInputsAssertionLevel = AssertionLevel.None

  def formalConfigureForTest(): this.type = {
    formalAssumeInputs()
    formalAsserts()
    this
  }

  lazy val formalValidInputs: Bool = True

  /**
   * Configure asserts around the input signals to the given object. This is kept seperate so that you can run
   * dut.formalAssertInputs()
   * dut.formalAssumes()
   *
   * which will test that the inputs to the already validated dut object adhere to whatever contract is in place for
   * it's inputs.
   */
  final def formalAssertInputs()(implicit useAssumes: Boolean = false): Unit = {
    val newLevel = if (useAssumes) AssertionLevel.Assumption else AssertionLevel.Assertion
    if (newLevel > CurrentInputsAssertionLevel) {
      val validInputs = new Composite(OwningComponent(), "formalValidInputs") {
        val validInputs = formalValidInputs
      }.validInputs
      assertOrAssume(validInputs)
      CurrentInputsAssertionLevel = newLevel
    }
  }

  final def formalAssumeInputs(): Unit = formalAssertInputs()(useAssumes = true)

  /**
   * Set o formal assertions required for testing and validating the component completely.
   *
   * @param useAssumes indicates that we want to assume all the predicates are always true; which informs inductive
   *                   provers which states are acceptable.
   * @return An area (typically a composite) which may contain signals useful for collectign during a simulation
   */
  protected def formalChecks()(implicit useAssumes: Boolean = false) : Unit = {}

  def FormalCompositeName(implicit useAssumes: Boolean = false) = if (useAssumes) "formalAssumes" else "formalAsserts"

  private def formalAssertsChildren(assumesInputValid: Boolean, useAssumes: Boolean = false): Unit = {
    HasFormalAsserts.formalAssertsChildren(Try {
      this.asInstanceOf[Component]
    }.getOrElse(null), assumesInputValid, useAssumes)
  }

  private def formalAssertsChildren()(implicit useAssumes: Boolean): Unit = {
    formalAssertsChildren(useAssumes, useAssumes)
  }

  def formalAssertOrAssume()(implicit useAssumes: Boolean = false): Unit = {
    if (useAssumes) {
      formalAssumes()
    } else {
      formalAsserts()
    }
  }

  def formalAsserts(): Unit = {
    if (CurrentAssertionLevel >= AssertionLevel.Assertion)
      return

    CurrentAssertionLevel = AssertionLevel.Assertion

    formalAssertsChildren()(useAssumes = false)
    formalChecks()(useAssumes = false)
  }

  def formalAssumes(): Unit = {
    if (CurrentAssertionLevel == AssertionLevel.Assumption)
      return

    formalAssumeInputs()
    if (HasFormalAsserts.alwaysAssert) {
      formalAsserts()
    } else {
      CurrentAssertionLevel = AssertionLevel.Assumption
      when(formalValidInputs) {
        formalAssertsChildren()(useAssumes = true)
        formalChecks()(useAssumes = true)
      }
    }
  }

  /**
   * Helper function; uses the implicit useAssumes variable to either emit an assert or assume
   *
   * @param cond       Condition to assert or assume
   * @param msg        Some backends with asserts will print out a message when an assert fails. Ignored for assumes
   * @param useAssumes True to emit an assume
   */
  def assertOrAssume(cond: Bool, msg: Any*)(implicit loc: Location, useAssumes: Boolean): Unit =
    HasFormalAsserts.assertOrAssume(cond, msg: _*)

}

object HasFormalAsserts {
  private def alwaysAssert: Boolean = sys.env.contains("SPINAL_FORMAL_NEVER_ASSUME")

  private def formalAssertTags(c: Component, assumesInputValid: Boolean, useAssumes: Boolean = false): Unit = {
    val direct_asserts = c.getTagsOf[HasFormalAssertsTag]().map(_.formalAsserts)

    direct_asserts.foreach(a => {
      a.formalAssertInputs()(useAssumes = assumesInputValid)
      a.formalAssertOrAssume()(useAssumes)
    })
  }

  def formalAssertsChildren(c: Component, assumesInputValid: Boolean, useAssumes: Boolean = false): Unit = {
    if (c == null) {
      return
    }

    def apply(c: Component, walkSet: mutable.HashSet[Component]): Unit = {
      if (!walkSet.contains(c)) {
        formalAssertTags(c, assumesInputValid, useAssumes)

        walkSet += c
        c match {
          case c: HasFormalAsserts => {
            c.formalAssertInputs()(useAssumes = assumesInputValid)
            c.formalAssertOrAssume()
          }
          case _ => c.walkComponents(apply(_, walkSet))
        }
      }
    }

    c.addPrePopTask(() => {
      formalAssertTags(c, assumesInputValid, useAssumes)

      val walkSet = new mutable.HashSet[Component]()
      walkSet += c
      c.walkComponents(apply(_, walkSet))
    })
  }

  /**
   * Helper function; uses the implicit useAssumes variable to either emit an assert or assume
   *
   * @param cond       Condition to assert or assume
   * @param msg        Some backends with asserts will print out a message when an assert fails. Ignored for assumes
   * @param useAssumes True to emit an assume
   */
  def assertOrAssume(cond: Bool, msg: Any*)(implicit loc: Location, useAssumes: Boolean): Unit = {
    if (useAssumes) {
      assume(cond)
    } else {
      assert(cond, Seq(msg: _*))
    }
  }
}

class ComponentWithFormalAsserts extends Component with HasFormalAsserts {
  override def asFormalDut() = {
    formalConfigureForTest()
    super.asFormalDut()
  }

  private def flattenIO() : Seq[Data] = {
    def hasDirectionnSpecifier(io : Data) = io match {
      case io: IMasterSlave => (io.isSlaveInterface || io.isMasterInterface)
      case _ => !io.isDirectionLess
    }
    def recursiveWalk(io : Data): Seq[Data] = io match {
      case io: MultiData => if (hasDirectionnSpecifier(io)) Seq(io) else io.elements.map(_._2).toSeq.flatMap(recursiveWalk)
      case _ => Seq(io)
    }
    getGroupedIO(true).flatMap(recursiveWalk)
  }

  def anyseq_inputs(): Unit = {
    getAllIo.filter(_.isInput).filter(_.dlcIsEmpty).foreach(anyseq)
  }

  override lazy val formalValidInputs: Bool =
    Vec(flattenIO().map({
      case io: FormalMasterSlave =>
        if(io.isMasterInterface) io.formalIsConsumerValid()
        else if(io.isSlaveInterface) io.formalIsProducerValid()
        else if(io.isInput) io.formalIsValid()
        else {
          assert(false)
          True
        }
      case io: FormalBundle => if(io.isInput) io.formalIsValid() else True
      case _ => True
    })).asBits.andR

  protected def formalCheckOutputs()(implicit useAssumes: Boolean = false): Unit = {
    flattenIO().foreach({
      case io: FormalMasterSlave =>
        if (io.isMasterInterface) assertOrAssume(io.formalIsProducerValid())
        else assertOrAssume(io.formalIsConsumerValid())
      case io: FormalBundle => if (io.isOutput) assertOrAssume(io.formalIsValid())
      case _ => True
    })
  }

  override protected def formalChecks()(implicit useAssumes: Boolean = false): Unit = {
    HasFormalAsserts.formalAssertsChildren(this, assumesInputValid = false, useAssumes = true)
    formalCheckOutputs()
  }

  override def formalAssumes(): Unit = {
    withAutoPull()
    super.formalAssumes()
  }

  override def formalAsserts(): Unit = {
    withAutoPull()
    super.formalAsserts()
  }
}