class Data{
  type Self <: Data

  def := (that : Self) : Unit = println("a")
}
class UInt extends Data{
  type Self = UInt
}

class Bundle extends Data{
  type Self = Bundle
}

class SFix extends Bundle{
  override type Self = super.Self
}

val u1 = new UInt()
val b1 = new Bundle()
val b2 = new Bundle()
val s1 = new SFix()
//b1 := u1
b1 := b2
s1 := b2
//
//
//import spinal.core._
//import spinal.lib._
//val seq = Seq(1,2,3,4,5,6,7)
//seq.reduceBalancedSpinal(_+_)
//
//
//val map = collection.mutable.Map[(Int,Int),Int]()
//map += ((1,2) -> 3)
//map += ((10,20) -> 30)
//
//map((10,20))
//
//
//class Entity
//
//trait Persister {
//  def doPersist(e: Entity) = {
//    e.persistForReal()
//  }
//}
//
//// our refined instance (and type):
//val refinedMockPersister = new Persister {
//  override def doPersist(e: Entity) = ()
//}