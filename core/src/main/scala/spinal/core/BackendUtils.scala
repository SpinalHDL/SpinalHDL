package spinal.core

import scala.StringBuilder
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, StringBuilder}


class ComponentBuilder(val component: Component) {
  val parts = ArrayBuffer[(StringBuilder, Boolean)]()

  def newPart(mustMatch: Boolean): StringBuilder = {
    val builder = new mutable.StringBuilder
    parts += (builder -> mustMatch)
    builder
  }

  def result: String = {
    val ret = new mutable.StringBuilder
    parts.foreach(ret ++= _._1)
    ret.result()
  }

  var hash: Integer = null

  override def hashCode(): Int = {
    if (hash == null) {
      hash = parts.filter(_._2).foldLeft(0)(_ + _._1.result().hashCode())
    }
    hash
  }

  override def equals(obj: scala.Any): Boolean = {
    if (this.hashCode() != obj.hashCode()) return false //Colision into hashmap implementation don't check it XD
    obj match {
      case cb: ComponentBuilder => {
        for ((a, b) <- (parts, cb.parts).zipped) {
          if (a._2 || b._2) {
            if (a._1.result() != b._1.result()) {
              return false
            }
          }
        }
        return true;
      }
      case _ => return ???
    }
  }
}


class ConditionalTree(val instanceCounter: Int)

class WhenTree(val cond: Node, instanceCounter: Int) extends ConditionalTree(instanceCounter) {
  var whenTrue: AssignementLevel = new AssignementLevel
  var whenFalse: AssignementLevel = new AssignementLevel
}

class SwitchTree(instanceCounter: Int, context: SwitchContext) extends ConditionalTree(instanceCounter) {
  val cases = new Array[(Node, AssignementLevel)](context.caseCount)
}

class AssignementLevel {
  //map of precedent ConditionalTree , assignements      if no precedent ConditionalTree simple assignememnt => null
  val logicChunk = mutable.Map[ConditionalTree, ArrayBuffer[(Node, Node)]]()
  val conditionalTrees = mutable.Map[ConditionalContext, ConditionalTree]()

  def isEmpty = logicChunk.isEmpty && conditionalTrees.isEmpty

  def isNotEmpty = !isEmpty


  def walkWhenTree(root: Node, that: Node): Unit = {
    def getElements: Iterator[Node] = {
      if (that.isInstanceOf[MultipleAssignmentNode]) {
        return that.getInputs
      } else {
        return Iterator(that)
      }
    }

    var lastConditionalTree: ConditionalTree = null
    for (node <- getElements) {
      node match {
        case whenNode: WhenNode => {
          if (!whenNode.whenTrue.isInstanceOf[NoneNode]) {
            val whenTree = this.conditionalTrees.getOrElseUpdate(whenNode.w, new WhenTree(whenNode.cond, node.instanceCounter)).asInstanceOf[WhenTree]
            lastConditionalTree = whenTree
            whenTree.whenTrue.walkWhenTree(root, whenNode.whenTrue)
          }
          if (!whenNode.whenFalse.isInstanceOf[NoneNode]) {
            val whenTree = this.conditionalTrees.getOrElseUpdate(whenNode.w, new WhenTree(whenNode.cond, node.instanceCounter)).asInstanceOf[WhenTree]
            lastConditionalTree = whenTree
            whenTree.whenFalse.walkWhenTree(root, whenNode.whenFalse)
          }
        }
        case switchNode: SwitchNode => {
          val switchTree = this.conditionalTrees.getOrElseUpdate(switchNode.context, new SwitchTree(node.instanceCounter, switchNode.context)).asInstanceOf[SwitchTree]
          lastConditionalTree = switchTree
          switchNode.onEachInput(input => {
            val caseNode = input.asInstanceOf[CaseNode]
            val tmp = switchTree.cases(caseNode.context.id)
            var caseElement = if (tmp != null) tmp
            else {
              val tmp = (caseNode.cond -> new AssignementLevel)
              switchTree.cases(caseNode.context.id) = tmp
              tmp
            }
            caseElement._2.walkWhenTree(root, caseNode.assignement)
          })
        }
        case reg: Reg =>
        case _ => this.logicChunk.getOrElseUpdate(lastConditionalTree, new ArrayBuffer[(Node, Node)]) += new Tuple2(root, node)
      }
    }
  }
}




trait VhdlVerilogBase {
  def isReferenceable(node: Node) = node.isInstanceOf[Nameable]

  def getSensitivity(nodes: Iterable[Node], includeNodes: Boolean): mutable.Set[Node] = {
    val sensitivity = mutable.Set[Node]()

    if (includeNodes)
      nodes.foreach(walk(_))
    else
      nodes.foreach(_.onEachInput(walk(_)))

    def walk(node: Node): Unit = {
      if (isReferenceable(node))
        sensitivity += node
      else
        node.onEachInput(walk(_))
    }

    sensitivity
  }


  class Process(val order: Int) {
    var sensitivity: mutable.Set[Node] = null
    val nodes = ArrayBuffer[Node]()
    val whens = ArrayBuffer[ConditionalContext]()
    var hasMultipleAssignment = false

    def genSensitivity: Unit = sensitivity = getSensitivity(nodes, false)


    def needProcessDef: Boolean = {
      if (!whens.isEmpty || nodes.size > 1) return true
      if (hasMultipleAssignment) {
        val ma: MultipleAssignmentNode = nodes(0).getInput(0).asInstanceOf[MultipleAssignmentNode]
        val assignedBits = new AssignedBits(nodes(0).getWidth)
        ma.onEachInput(_ match {
          case assign: AssignementNode => {
            val scope = assign.getScopeBits
            if (!AssignedBits.intersect(scope, assignedBits).isEmpty) return true
            assignedBits.add(scope)
          }
          case _ => return true
        })
      }
      return false
    }
  }

  def getAsyncProcesses(component: Component) : Seq[Process] = {
    var processCounter = 0


    val processSet = mutable.Set[Process]()
    val whenToProcess = mutable.Map[ConditionalContext, Process]()

    def move(to: Process, from: Process): Unit = {
      to.nodes ++= from.nodes
      to.whens ++= from.whens
      to.hasMultipleAssignment |= from.hasMultipleAssignment
      from.whens.foreach(whenToProcess(_) = to)
      processSet.remove(from)
    }

    val asyncSignals = component.nodes.filter(_ match {
      case signal: BaseType => (!signal.isDelay) && (!((signal.isIo && signal.isInput) || component.kindsOutputsBindings.contains(signal)))
      case _ => false
    })

    for (signal <- asyncSignals) {
      var process: Process = null
      var hasMultipleAssignment = false
      walk(signal.getInput(0))
      def walk(that: Node): Unit = {
        that match {
          case wn: WhenNode => {
            if (whenToProcess.contains(wn.w)) {
              val otherProcess = whenToProcess.get(wn.w).get
              if (process == null) {
                process = otherProcess
                otherProcess.nodes += signal
              } else if (process != otherProcess) {
                move(otherProcess, process)
                process = otherProcess
              }
            } else {
              if (process == null) {
                process = new Process(processCounter);
                processCounter += 1
                process.nodes += signal
                processSet += process
              }
              process.whens += wn.w
              whenToProcess += (wn.w -> process)
            }

            walk(wn.whenTrue)
            walk(wn.whenFalse)
          }
          case switchNode: SwitchNode => {
            if (whenToProcess.contains(switchNode.context)) {
              val otherProcess = whenToProcess.get(switchNode.context).get
              if (process == null) {
                process = otherProcess
                otherProcess.nodes += signal
              } else if (process != otherProcess) {
                move(otherProcess, process)
                process = otherProcess
              }
            } else {
              if (process == null) {
                process = new Process(processCounter);
                processCounter += 1
                process.nodes += signal
                processSet += process
              }
              process.whens += switchNode.context
              whenToProcess += (switchNode.context -> process)
            }

            switchNode.cases.foreach(n => walk(n.asInstanceOf[CaseNode].assignement))
          }
          case man: MultipleAssignmentNode => {
            man.onEachInput(walk(_))
            hasMultipleAssignment = true
          }
          case that => {
            if (process == null) {
              process = new Process(processCounter);
              processCounter += 1
              process.nodes += signal
              processSet += process
            }
          }
        }
      }

      process.hasMultipleAssignment |= hasMultipleAssignment
    }

    val processList = processSet.toList.sortWith(_.order < _.order)
    processList
  }

}
