package spinal.lib.sim

import spinal.core.Data
import spinal.core.sim._

import scala.collection.mutable

//
//object ScoreboardInOrder{
//  implicit class ScoreboardInOrderPimper(pimped : ScoreboardInOrder[SimData]){
//
//    def pushDut(that : Data) : Unit = {
//      pimped.pushDut(SimData.copy(that))
//    }
//
//    def pushRef(that : Data) : Unit = {
//      pimped.pushRef(SimData.copy(that))
//    }
//
//  }
//}

case class ScoreboardInOrder[T]() {
  val dut,ref = mutable.Queue[T]()
  var matches = 0

  if(Phase.isUsed){
    Phase.check{
      if(dut.nonEmpty || ref.nonEmpty){
        if(dut.nonEmpty){
          println("Unmatched DUT transaction : \n")
          dut.foreach(d => println(d))
        }

        if(ref.nonEmpty){
          println("Unmatched reference transaction :\n")
          ref.foreach(d => println(d))
        }
        Phase.check.onEnd(simFailure())
      }
    }
  }

  def pushDut(that : T) : Unit = {
    dut.enqueue(that)
    check()
  }

  def pushRef(that : T) : Unit = {
    ref.enqueue(that)
    check()
  }

  def check(): Unit ={
    if(ref.nonEmpty && dut.nonEmpty){
      val dutHead = dut.dequeue()
      val refHead = ref.dequeue()
      if(dutHead != refHead){
        println("Transaction missmatch :")
        println("REF :")
        println(refHead)
        println("DUT :")
        println(dutHead)
        simFailure()
      }
      matches += 1
    }
  }
}
