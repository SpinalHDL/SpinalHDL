package spinal.lib.generator

import spinal.core.{Bool, Nameable}
import spinal.core.fiber._

trait InterruptCtrlGeneratorI{
  def addInterrupt(interrupt : Handle[Bool], id : Int)
  def getBus : Handle[Nameable]
}