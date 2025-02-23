package spinal.lib.formal

/**
 * This trait can be use by custom bundle classes to add logic on what is a valid state for that bundle.
 */
trait FormalData { self =>
  type Self <: FormalData

  /**
   * @return Whether or not the current state of the bundle is valid. Typically either asserted or assumed by a
   *         component which has this bundle as an input or an output.
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  def formalIsStateValid(): Seq[FormalProperty]

  /**
   * When a bundle has a formal contract which can not be tied directly to it's current state, it is sometimes necessary
   * to assert that two objects have a functionally equivalent internal state.
   *
   * As a concrete example, a pipelined bus will typically have an assertion that it can't receive more data than was
   * requested and so there is an outstanding replies counter which must be part of the busses formal contract. This
   * function would be called when you wanted to ensure that another bus has the exact smae oustanding replies counter.
   *
   * This is typically required for inductive methods which often must be told this, even if the components share
   * all of the same signals.
   *
   * @param that The object to assert equivalence with.
   */
  def formalAssertEquivalence(that: Self): Unit = {}
}
