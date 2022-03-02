package spinal

import scala.languageFeature.postfixOps

package object core {

  implicit val postfixOps          : postfixOps = scala.language.postfixOps

  class IntBuilder(val i: Int) extends AnyVal {

    def bits = println("Got bits " + i)
  }

  implicit def IntToBuilder(value: Int): IntBuilder = new IntBuilder(value)
}
