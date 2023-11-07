package spinal.lib.misc.pipeline

import scala.collection.mutable

object Builder {
  def apply(head : Connector, tail : Connector*) : Unit = apply(head +: tail)
  def apply(connectors: Seq[Connector]): Unit = {
    def propagateDown(): Unit = {
      val solved = mutable.ArrayBuffer[Node]()
      val seeds = mutable.ArrayBuffer[Connector]()
      seeds ++= connectors.filter(c => c.ups.isEmpty || c.ups.forall(_.up == null))
      while (seeds.nonEmpty) {
        val tmp = seeds.toArray
        seeds.clear()
        for (e <- tmp) {
//          println(s"PR DOWN $e")
          e.propagateDown()
          for (d <- e.downs) {
            solved += d
            if (d.down != null && d.down.ups.forall(u => solved.contains(u))) {
              seeds += d.down
            }
          }
        }
      }
    }

    def propagateUp(): Unit = {
      val solved = mutable.ArrayBuffer[Node]()
      val seeds = mutable.ArrayBuffer[Connector]()
      seeds ++= connectors.filter(c => c.downs.isEmpty || c.downs.forall(_.down == null))
      while (seeds.nonEmpty) {
        val tmp = seeds.toArray
        seeds.clear()
        for (e <- tmp) {
//          println(s"PR UP $e")
          e.propagateUp()
          for (d <- e.ups) {
            solved += d
            if (d.up != null && d.up.downs.forall(u => solved.contains(u))) {
              seeds += d.up
            }
          }
        }
      }
    }

    propagateUp()
    propagateDown()
    for (c <- connectors) c.build()
  }
}


