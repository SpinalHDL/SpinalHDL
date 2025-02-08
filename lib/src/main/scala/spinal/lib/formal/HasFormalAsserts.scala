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

/**
 * This trait ties formal assumptions and assertions to the implementing object. This is useful to seperate out assertion
 * and formal testing logic from the main body of areas and components.
 *
 * If you are adding this to a Component, you likely want to use `ComponentWithFormalAsserts` instead.
 */
trait HasFormalAsserts {
  if (!this.isInstanceOf[Component]) {
    Component.current.addTag(new HasFormalAssertsTag(this))
  }

  private val OwningComponent = WeakReference(Component.current)

  private var CurrentAssertionLevel = AssertionLevel.None
  private var CurrentInputsAssertionLevel = AssertionLevel.None

  /**
   * Helper function to configure this object for testing. This typically means we assume the inputs are valid but
   * want to assert the internal conditions are valid as a result.
   */
  def formalConfigureForTest(): this.type = {
    formalAssumeInputs()
    formalAsserts()
    this
  }

  /**
   * This override defines whether or not the input conditions are valid. Typically there is no expectations made of a
   * component if the inputs aren't valid.
   */
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
   * Set of formal assertions required for testing and validating the component completely.
   *
   * @param useAssumes indicates that we want to assume all the predicates are always true; which informs inductive
   *                   provers which states are acceptable.
   */
  protected def formalChecks()(implicit useAssumes: Boolean = false) : Unit = {}

  /**
   * If you wrap the formalChecks in a composite, this lets you name it appropraitely
    */
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

    formalAssertInputs()
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
   * Helper function; uses the implicit useAssumes variable to either emit an assert or assume.
   *
   * This is used to declare something that should be considered a formal invariant of the design. Depending on context,
   * we can assume this property rather than assert it. If the formal property given is true, using assert or assume
   * on it has the same final results on any given test however assumptions allow formal validation to run much faster
   * since it does not have to also reprove the valididty of trusted components.
   *
   * @param cond       Condition to assert or assume
   * @param msg        Some backends with asserts will print out a message when an assert fails. Ignored for assumes
   * @param useAssumes True to emit an assume
   */
  def assertOrAssume(cond: Bool, msg: Any*)(implicit loc: Location, useAssumes: Boolean): Unit =
    HasFormalAsserts.assertOrAssume(cond, msg: _*)

}

object HasFormalAsserts {
  /***
   * Occasionaly it can be useful to swap every assumption except the input assumptions for assertions. This is mainly
   * to verify that there are no faulty assumptions being made.
   */
  private def alwaysAssert: Boolean = sys.env.contains("SPINAL_FORMAL_NEVER_ASSUME")

  private def formalAssertTags(c: Component, assumesInputValid: Boolean, useAssumes: Boolean = false): Unit = {
    val direct_asserts = c.getTagsOf[HasFormalAssertsTag]().map(_.formalAsserts)

    direct_asserts.foreach(a => {
      a.formalAssertInputs()(useAssumes = assumesInputValid)
      a.formalAssertOrAssume()(useAssumes)
    })
  }

  /**
   * Helper function to find every tagged formal asserts object attached to a component and activate it's assertions or
   * assumptions
   *
   * @param component The component to walk through
   * @param assumesInputValid Whether or not to assume the inputs to this component are always valid
   * @param useAssumes Whether or not to call formalAssumes() instead of formalAsserts() on the child objects.
   */
  def formalAssertsChildren(component: Component, assumesInputValid: Boolean, useAssumes: Boolean = false): Unit = {
    if (component == null) {
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

    component.addPrePopTask(() => {
      formalAssertTags(component, assumesInputValid, useAssumes)

      val walkSet = new mutable.HashSet[Component]()
      walkSet += component
      component.walkComponents(apply(_, walkSet))
    })
  }

  /**
   * Helper function; uses the implicit useAssumes variable to either emit an assert or assume
   *
   * This is used to declare something that should be considered a formal invariant of the design. Depending on context,
   * we can assume this property rather than assert it. If the formal property given is true, using assert or assume
   * on it has the same final results on any given test however assumptions allow formal validation to run much faster
   * since it does not have to also reprove the valididty of trusted components.
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

