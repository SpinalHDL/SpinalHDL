
val x = 1.32e32
x.toString
BigDecimal.valueOf(x).toBigInt();

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