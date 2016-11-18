package spinal.lib.math

import spinal.core._
import spinal.lib.{Delay, traversableOncePimped}

import scala.collection.mutable.ArrayBuffer


object SIntMath {
  case class MultTask(aOffset: Int, bOffset: Int, aWidth: Int, bWidth: Int)
  def mul(a: SInt, b: SInt, multOpWidth: Int, keepFrom: Int,multRegs : Int = 0,aggregatorStages: (SInt, Int) => SInt = (s,l) => s): SInt = {
    val multTasks = ArrayBuffer[MultTask]()
    val aWidth = widthOf(a)
    val bWidth = widthOf(b)
    for (aOffset <- Range(0, aWidth, multOpWidth)) {
      for (bOffset <- Range(0, bWidth, multOpWidth)) {
        val aPartWidth = Math.min(multOpWidth, aWidth - aOffset)
        val bPartWidth = Math.min(multOpWidth, bWidth - bOffset)
        if (aOffset + aPartWidth + bOffset + bPartWidth > keepFrom)
          multTasks += MultTask(aOffset, bOffset, aPartWidth, bPartWidth)
      }
    }

    var ret = SInt(widthOf(a) + widthOf(b) bit)
    //ret := 0
    //for(task <- multTasks){
    val mults = multTasks.map(task => {
      val aPartSigned = task.aOffset + task.aWidth == aWidth
      val bPartSigned = task.bOffset + task.bWidth == bWidth
      val aPart = a(task.aOffset, task.aWidth bit)
      val bPart = b(task.bOffset, task.bWidth bit)
      val mult = (aPartSigned, bPartSigned) match {
        case (false, false) => S(False ## (U(aPart) * U(bPart)))
        case (false, true) => S(False ## aPart) * bPart
        case (true, false) => aPart * S(False ## bPart)
        case (true, true) => aPart * bPart
      }
      //ret = ret + (mult << (task.aOffset + task.bOffset))
      Delay((mult << (task.aOffset + task.bOffset)),multRegs).resize(aWidth+bWidth)
    })
    //ret
     mults.sortWith(widthOf(_) < widthOf(_)).reduceBalancedTree((l, r) => l + r,aggregatorStages)
  }
}
