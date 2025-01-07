package spinal.core.formal

import spinal.core.formal.HasFormalAsserts._alwaysAssert
import spinal.core.{Area, Bool, Component, ImplicitArea, True, assert, assume, when}
import spinal.idslplugin.Location

import scala.collection.mutable

object FormalDut{
  def apply[T <: Component](dut : T) = {
    val c = Component.current
    if(c != null) {
      c.withAutoPull()
      c.setFormalTester()
    }

    dut match {
      case withAsserts: HasFormalAsserts => withAsserts.formalAsserts()
      case _ => {}
    }

    dut.asFormalDut()
  }
}

trait HasFormalAsserts {
  lazy val formalValidInputs : Bool = True
  /**
   * Configure asserts around the input signals to the given object. This is kept seperate so that you can run
   * dut.formalAssertInputs()
   * dut.formalAssumes()
   *
   * which will test that the inputs to the already validated dut object adhere to whatever contract is in place for
   * it's inputs.
   */
  final def formalAssertInputs()(implicit loc: Location, useAssumes : Boolean = false): Area = new Area { assertOrAssume( formalValidInputs )}
  final def formalAssumeInputs()(implicit loc: Location) = formalAssertInputs()(loc = loc, useAssumes = true)

  /**
   * Set o formal assertions required for testing and validating the component completely.
   * @param useAssumes indicates that we want to assume all the predicates are always true; which informs inductive
   *                   provers which states are acceptable.
   * @return An area (typically a composite) which may contain signals useful for collectign during a simulation
   */
  def formalAsserts()(implicit useAssumes : Boolean = false) : Area

  def formalAssumes() = {
    if(_alwaysAssert) {
      formalAsserts()
    } else {
      var area: Area = null
      when(formalValidInputs) {
        area = formalAsserts()(useAssumes = true)
      }
      area
    }
  }

  /**
   * Helper function; uses the implicit useAssumes variable to either emit an assert or assume
   * @param cond Condition to assert or assume
   * @param msg Some backends with asserts will print out a message when an assert fails. Ignored for assumes
   * @param useAssumes True to emit an assume
   */
  def assertOrAssume(cond : Bool, msg : Any*)(implicit loc: Location, useAssumes : Boolean): Unit =
    HasFormalAsserts.assertOrAssume(cond, msg:_*)

  def withFormalAsserts() : this.type = {
    formalAsserts()
    this
  }
  def withFormalAssumes() : this.type = {
    formalAssumes()
    this
  }
}

object HasFormalAsserts {
  def _alwaysAssert : Boolean = sys.env.contains("SPINAL_FORMAL_NEVER_ASSUME")

  def formalAssertsChildren(c: Component, assumesInputValid : Boolean , useAssumes : Boolean = false)(implicit loc: Location): Unit = {
    def apply(c : Component, walkSet : mutable.HashSet[Component]) : Unit = {
      if (!walkSet.contains(c)) {

        walkSet += c
        c match {
          case c: HasFormalAsserts => {
            c.formalAssertInputs()(loc = loc, useAssumes = assumesInputValid)
            if(useAssumes) {
              c.formalAssumes()
            } else {
              c.formalAsserts()
            }
          }
          case _ => c.walkComponents(apply(_, walkSet))
        }
      }
    }

    c.addPrePopTask(() => {
      val walkSet = new mutable.HashSet[Component]()
      walkSet += c
      c.walkComponents(apply(_, walkSet))
    })
  }

  /**
   * Helper function; uses the implicit useAssumes variable to either emit an assert or assume
   * @param cond Condition to assert or assume
   * @param msg Some backends with asserts will print out a message when an assert fails. Ignored for assumes
   * @param useAssumes True to emit an assume
   */
  def assertOrAssume(cond : Bool, msg : Any*)(implicit loc: Location, useAssumes : Boolean): Unit = {
    if(useAssumes) {
      assume(cond)
    } else {
      assert(cond, Seq(msg:_*))
    }
  }
}