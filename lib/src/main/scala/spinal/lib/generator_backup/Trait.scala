package spinal.lib.generator_backup

import spinal.core.{Bool, Nameable}

trait InterruptCtrlGeneratorI{
  def addInterrupt(interrupt : Handle[Bool], id : Int)
  def getBus : Handle[Nameable]
}