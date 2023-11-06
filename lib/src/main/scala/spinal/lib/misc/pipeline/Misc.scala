package spinal.lib.misc.pipeline

import scala.collection.mutable

class FromUp() {
  var withValid = false
  val payload = mutable.LinkedHashSet[StageableKey]()
}

class FromDown() {
  var withReady = false
  val payload = mutable.LinkedHashSet[StageableKey]()
}
