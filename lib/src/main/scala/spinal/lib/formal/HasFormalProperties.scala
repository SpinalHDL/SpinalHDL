package spinal.lib.formal

import spinal.core.internals.{AssertStatement, AssertStatementKind}
import spinal.core.{Bool, Component, SpinalTag, True, when}

class HasFormalAssertsTag(val formalAsserts : HasFormalProperties) extends SpinalTag

/**
 * This trait is used to define some component-like thing (area, component, etc) as having formal properties that it
 * abides by. Use overrides on `formalInputProperties` and `formalProperties` when implementing this trait to fill in
 * this logic.
 *
 * formalInputProperties defines what constitutes valid input to the component-like thing. It can be empty for certain
 * cases.
 *
 * `formalProperties` are a set of properties that are expected to hold when every property in `formalInputProperties`
 * is true.
 *
 * When setting up for formal testing, you also need to invoke `formalSetMinimumAssertionKind`, `formalConfigureForTest`,
 * `formalAssertProperties` or `formalAssumeProperties` to determine how these properties are used. The two typical
 * cases are:
 *
 * - When being tested directly, `formalConfigureForTest` assumes all the input properties are true and asserts that the
 * body properties are true.
 * - When nested in a component under test, assert all the input properties are true and assume that the body properties
 * are true. This allows the solver to not waste time trying to verify the properties in the component not under direct
 * test.
 *
 * If you are adding this to a Component, you likely want to use `ComponentWithFormalAsserts`
 */
trait HasFormalProperties { self =>
  /**
   * The typical configuration when testing on a block directly. Assume it's inputs and assert it's body properties.
   * Typically, the inputs are also all driven by `any_seq` or `any_const`.
   */
  def formalConfigureForTest() = {
    formalSetMinimumAssertionKind(AssertStatementKind.ASSERT, AssertStatementKind.ASSUME)
  }

  /**
   * Set the assertion kinds used for both body properties and input properties. Notably, this function will latch any
   * assertion level that was set to ASSUME even if called again with ASSESRTION. This follows system verilog logic
   * which will also effectively ignore asserts if it was given an assume with the same predicate prior.
   *
   * @param assertionKind Main properties assertion kind
   * @param inputAssertionKind Input properties assertion kind
   */
  def formalSetMinimumAssertionKind(assertionKind : AssertStatementKind = AssertStatementKind.ASSERT,
                                     inputAssertionKind : AssertStatementKind = AssertStatementKind.ASSERT): this.type = {
    if (shouldApplyAssertionKind(CurrentAssertionKind, assertionKind)) {
      CurrentAssertionKind = assertionKind
    }
    if (shouldApplyAssertionKind(CurrentInputsAssertionKind, inputAssertionKind)) {
      CurrentInputsAssertionKind = inputAssertionKind
    }
    this
  }

  def formalAssertProperties(): this.type = formalSetMinimumAssertionKind(AssertStatementKind.ASSERT)

  def formalAssumeProperties(): this.type = formalSetMinimumAssertionKind(AssertStatementKind.ASSUME)

  /**
   * @return Returns the list of properties that must be true if the input to the given component-like class are to
   *         be considered valid, and thus signify that the `formalProperties` themselves are all true as well.
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  protected def formalInputProperties() : Seq[FormalProperty] = Seq()

  /**
   * @return The formal properties which should all be true if the formalInputProperties are true too. These are the main
   *         assertions we are concerned with defining and verifying in formal testing
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  protected def formalProperties() : Seq[FormalProperty]

  if (!this.isInstanceOf[Component]) {
    Component.current.addTag(new HasFormalAssertsTag(this))
  }

  private def shouldApplyAssertionKind(current: Option[AssertStatementKind], newKind: AssertStatementKind): Boolean = {
    (current, newKind) match {
      case (None, _) => true
      case (_, AssertStatementKind.ASSERT) => false
      case (Some(AssertStatementKind.ASSUME), _) => false
      case (Some(AssertStatementKind.ASSERT), AssertStatementKind.ASSUME) => true
    }
  }

  private var _CurrentAssertionKind : Option[AssertStatementKind] = None
  def CurrentAssertionKind = _CurrentAssertionKind
  def CurrentAssertionKind_=(kind: AssertStatementKind): Unit ={
    val actualKind = if(HasFormalProperties.alwaysAssert) AssertStatementKind.ASSERT else kind
    _CurrentAssertionKind = Some(actualKind)
    formalAssertStatements.foreach(_.kind = actualKind)
  }

  private var _CurrentInputsAssertionKind : Option[AssertStatementKind] = None
  def CurrentInputsAssertionKind = _CurrentInputsAssertionKind
  def CurrentInputsAssertionKind_=(kind: AssertStatementKind): Unit ={
    _CurrentInputsAssertionKind = Some(kind)
    formalAssertInputStatements.foreach(_.kind = kind)
  }

  final lazy val formalInputStateIsValid: Bool = {
    formalInputPropertiesEval.map(_.condition).fold(True)((x,y) => x && y)
  }

  final lazy val formalStateIsValid: Bool = {
    formalInputStateIsValid && formalProperties().map(_.condition).fold(True)((x, y) => x && y)
  }

  def formalChildren(): Seq[HasFormalProperties] = Seq()
  def formalDescendants(includeSelf : Boolean = false): Seq[HasFormalProperties] = {
    (if(includeSelf) Seq(self) else Seq()) ++
      formalChildren().flatMap(c => c.formalDescendants(true))
  }
  /**
   * Gather all the formal properties and turn them into assert statements. These statements can be upgraded to assumptions
   * later if required.
   */
  private lazy val formalAssertStatements = {
    var asserts : Seq[AssertStatement] = null

    // For assumptions, it is a hard requirement that we predicate the assumption on the inputs being valid. It is also
    // useful with assertions -- the fewer assertions that fire off at once when testing the better since that makes it
    // easier to figure out where the flaw in the logic is.
    when(formalInputStateIsValid) {
      asserts = formalPropertiesEval.map(_.assertStatement)
    }
    asserts
  }

  private lazy val formalAssertInputStatements = {
    formalInputPropertiesEval.map(_.assertStatement)
  }

  // We use private lazy vals here to only call formalProperties / formalInputProperties once.
  private lazy val formalPropertiesEval = formalProperties()
  private lazy val formalInputPropertiesEval = formalInputProperties()
}

object HasFormalProperties {
  /***
   * Occasionaly it can be useful to swap every assumption except the input assumptions for assertions. This is mainly
   * to verify that there are no faulty assumptions being made.
   */
  private def alwaysAssert: Boolean = sys.env.contains("SPINAL_FORMAL_NEVER_ASSUME")
}

