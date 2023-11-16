package spinal.idslplugin


/**
 * A trait representing a callback mechanism for post-initialization operations.
 * 
 * This trait is used to define an action that should be taken after an object has been initialized.
 * Classes or traits implementing `PostInitCallback` can define custom behavior that is executed
 * after the initial construction and setup of the instance.
 */
trait PostInitCallback {
  /**
   * The method to be called after the initialization of the object.
   *
   * Implementing classes should override this method to define custom post-initialization behavior.
   * This method is expected to return the current instance (`this`) of the class.
   *
   * @return the current instance of the class implementing this trait.
   */
  def postInitCallback(): this.type
}

/**
 * A trait for callback handling during value assignments.
 * 
 * This trait provides a mechanism to define custom behavior whenever a value is assigned to a variable.
 * It is particularly useful in scenarios where additional actions need to be taken or side-effects need
 * to be considered during value assignments.
 */
trait ValCallback {
  /**
   * A method to be called during the assignment of a value.
   * 
   * Implementors can override this method to define custom behavior that occurs when a value is assigned.
   * This method can be used to perform additional checks, logging, or other side-effects.
   *
   * @param ref The value being assigned.
   * @param name The name of the variable to which the value is being assigned.
   * @tparam T The type of the value being assigned.
   * @return The value after potential modifications or checks.
   */
  def valCallback[T](ref: T, name: String): T
}