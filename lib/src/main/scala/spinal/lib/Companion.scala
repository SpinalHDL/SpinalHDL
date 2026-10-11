package spinal.lib

import spinal.core.{Data, MultiData, dontName}

/** A trait that a `MultiData` can implement, to make it less lonely :) */
trait Companion[T <: Data] {

  /** Make sure we are implemented on a MultiData object only */
  assert(this.isInstanceOf[MultiData], s"Error: Companion ${this} must be of Type spinal.core.MultiData")

  /** The value to act as a companion
    * Initialized as lazy so as to not fail during object creation
    */
  lazy private val companion = getCompanion()

  /** Parse object to find the field with the companion attribute
    * Ensure that there is one (and only one) field with said attribute
    * @return The field in this with the companion attribute as an instance of [T <: Data]
    */
  private def getCompanion(): T = {
    var companion: T = null.asInstanceOf[T]
    for (field <- this.getClass.getDeclaredFields) {
      if (field.isAnnotationPresent(classOf[companion])) {
        field.setAccessible(true)
        if (companion == null) companion = field.get(this).asInstanceOf[T]
        else assert(false, s"Error: There may only be one companion annotation in ${this}")
      }
    }
    if (companion == null) assert(false, s"Error: ${this} has no Companions and is sad :(")
    companion
  }

  /** Access the companion attribute */
  def apply(): T = companion

  /** I found another way to initialize this, though I'm not sure which method is better
    * It works in the same way, but makes companion a var instead of a lazy val
    * (and hence is null only until apply() is called)
    * Feedback would be appreciated :)

        private var companion: T = _

        private def getCompanion(): Unit = {
          for (field <- this.getClass.getDeclaredFields) {
            if (field.isAnnotationPresent(classOf[companion])) {
              field.setAccessible(true)
              if (companion == null) companion = field.get(this).asInstanceOf[T]
              else assert(false, s"Error: There may only be one companion annotation in ${this}")
            }
          }
          if (companion == null) assert(false, s"Error: ${this} has no Companions and is sad :(")
        }

        def apply(): T = { if (companion == null) getCompanion(); companion }
    */
}
