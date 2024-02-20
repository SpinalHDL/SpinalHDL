package spinal.lib.misc.pipeline

import spinal.core.Nameable
import scala.collection.Seq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Builder {
  def apply(head : Link, tail : Link*) : Unit = apply(head +: tail)
  def apply(connectors: Seq[Link]): Unit = {
    def propagateDown(): Unit = {
      val solved = mutable.ArrayBuffer[Node]()
      val seeds = mutable.ArrayBuffer[Link]()
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
      val seeds = mutable.ArrayBuffer[Link]()
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

    for (c <- connectors) c.nodeSetup()
    propagateUp()
    propagateDown()
    for (c <- connectors) c.build()

    val nodes = connectors.flatMap(c => c.ups ++ c.downs).distinctLinked
    for(n <- nodes) n.build()
  }
}

class NodesBuilder() extends Nameable {
  val nodes = ArrayBuffer[Node]()
  val connectors = ArrayBuffer[Link]()

  class Node extends spinal.lib.misc.pipeline.Node {
    nodes += this
  }

  def genStagedPipeline(): Unit = {
    for ((up, down) <- (nodes, nodes.tail).zipped) {
      connectors += StageLink(up, down).setCompositeName(this, "connector")
    }
    Builder(connectors)
  }
}


