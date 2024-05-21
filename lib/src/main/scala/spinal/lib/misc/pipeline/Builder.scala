package spinal.lib.misc.pipeline

import spinal.core.Nameable

import scala.collection.{Seq, mutable}
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

class StagePipeline() extends Nameable {
  val nodes = mutable.LinkedHashMap[Int, Node]()
  val links = mutable.ArrayBuffer[StageLink]()

  def apply(i : Int) = node(i)
  def node(i : Int) = nodes.getOrElseUpdate(i, new Node().setCompositeName(this, s"node_${i.toString}"))
  class Area(i : Int) extends NodeMirror(node(i)) with spinal.core.Area

  def build(withoutCollapse : Boolean = false): Unit = {
    for(i <- nodes.keys.min until nodes.keys.max){
      val stage = StageLink(node(i), node(i+1)).setCompositeName(this, s"stage_${i+1}")
      if(withoutCollapse) stage.withoutCollapse()
      links += stage
    }
    Builder(links)
  }
}

class StageCtrlPipeline() extends Nameable {
  val ctrls = mutable.LinkedHashMap[Int, CtrlLink]()
  val links = mutable.ArrayBuffer[StageLink]()

  def ctrl(i : Int) = ctrls.getOrElseUpdate(i, CtrlLink().setCompositeName(this, s"ctrl_${i.toString}"))
  class Ctrl(i : Int) extends CtrlLinkMirror(ctrl(i))
  class InsertArea extends NodeMirror(ctrl(0).up) with spinal.core.Area

  def build(): Unit = {
    for(i <- ctrls.keys.min until ctrls.keys.max){
      links += StageLink(ctrl(i).down, ctrl(i+1).up).setCompositeName(this, s"stage_${i+1}")
    }
    Builder(links ++ ctrls.values)
  }
}


