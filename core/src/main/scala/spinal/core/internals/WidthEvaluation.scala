package spinal.core.internals

import java.util.IdentityHashMap

import spinal.core._

import scala.collection.mutable

/** Construction-time width evaluation. A completed value is kept only for one top-level getWidth call. */
final class WidthEvaluation(globalData: GlobalData) {
  val completed = new IdentityHashMap[Widthable, java.lang.Integer]()
  var calculatingWidth = false

  class Frame(val node: Widthable) {
    val dependencies = node.calcNodes
  }

  def get(node: Widthable): Int = {
    if (globalData.nodeGetWidthWalkedSet.contains(node))
      SpinalError(s"Can't calculate width of ${node.getScalaLocationLong} when design is in construction phase")
    if (completed.containsKey(node)) return completed.get(node).intValue()
    if (calculatingWidth)
      throw new IllegalStateException(s"Missing width dependency on ${node.getScalaLocationLong}")
    evaluate(node)
  }

  def evaluate(root: Widthable): Int = {
    val frames = mutable.Stack[Frame]()

    def push(node: Widthable): Unit = {
      if (globalData.nodeGetWidthWalkedSet.contains(node))
        SpinalError(s"Can't calculate width of ${node.getScalaLocationLong} when design is in construction phase")
      val frame = new Frame(node)
      globalData.nodeGetWidthWalkedSet += node
      frames.push(frame)
    }

    try {
      push(root)

      while (frames.nonEmpty) {
        val frame = frames.top
        if (frame.dependencies.hasNext) {
          frame.dependencies.next() match {
            case child: Widthable if !completed.containsKey(child) => push(child)
            case _ =>
          }
        } else {
          val previous = calculatingWidth
          calculatingWidth = true
          val width = try frame.node.calcWidth finally calculatingWidth = previous
          if (width == -1) {
            SpinalError(s"Can't infer width because of unspecified width on ${frame.node.getScalaLocationLong}")
          }

          frames.pop()
          globalData.nodeGetWidthWalkedSet -= frame.node
          if (frame.node.widthWhenNotInferred != -1 && frame.node.widthWhenNotInferred != width) {
            SpinalError(s"getWidth result differ from last call ${frame.node.getScalaLocationLong}")
          }
          frame.node.widthWhenNotInferred = width
          completed.put(frame.node, width)
        }
      }
      completed.get(root).intValue()
    } finally {
      frames.foreach(frame => globalData.nodeGetWidthWalkedSet -= frame.node)
    }
  }
}
