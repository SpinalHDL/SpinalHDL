package spinal.lib.misc.pipeline

import spinal.core.Nameable
import spinal.lib.StageableKey

import scala.collection.mutable

class FromUp() {
  var withValid = false
  val payload = mutable.LinkedHashSet[StageableKey]()
}

class FromDown() {
  var withReady = false
  val payload = mutable.LinkedHashSet[StageableKey]()
}

object Misc{
  def nameThat(self : Nameable, target: Nameable, key: StageableKey, postfix: String): Unit = {
    target.setLambdaName(self.isNamed && key.stageable.isNamed) {
      val stageName = self.getName
      val stageSlices = stageName.split('_')
      val postfixName = key.toString + postfix
      val postfixSlices = postfixName.split('_')
      var i = 0
      val iEnd = stageSlices.length min postfixSlices.length
      while (i != iEnd && stageSlices(i) == postfixSlices(i)) i += 1
      stageName + "_" + postfixSlices.drop(i).mkString("_")
    }
  }
}