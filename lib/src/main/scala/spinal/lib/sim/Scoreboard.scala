package spinal.lib.sim

import spinal.core.Data

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
      assert(dutHead == refHead)
      matches += 1
    }
  }
}
