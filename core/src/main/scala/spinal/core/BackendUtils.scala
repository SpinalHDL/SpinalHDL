package spinal.core
//
import java.text.SimpleDateFormat
//import java.util
import java.util.{Calendar, Date}
//
//import scala.StringBuilder
//import scala.collection.mutable
//import scala.collection.mutable.{ArrayBuffer, StringBuilder}
//
//
//class ComponentBuilder(val component: Component) {
//  val parts = ArrayBuffer[(StringBuilder, Boolean)]()
//
//  def newPart(mustMatch: Boolean): StringBuilder = {
//    val builder = new mutable.StringBuilder
//    parts += (builder -> mustMatch)
//    builder
//  }
//
//  def result: String = {
//    val ret = new mutable.StringBuilder
//    parts.foreach(ret ++= _._1)
//    ret.result()
//  }
//
//  var hash: Integer = null
//
//  override def hashCode(): Int = {
//    if (hash == null) {
//      hash = parts.filter(_._2).foldLeft(0)(_ + _._1.result().hashCode())
//    }
//    hash
//  }
//
//  override def equals(obj: scala.Any): Boolean = {
//    if (this.hashCode() != obj.hashCode()) return false //Colision into hashmap implementation don't check it XD
//    obj match {
//      case cb: ComponentBuilder => {
//        for ((a, b) <- (parts, cb.parts).zipped) {
//          if (a._2 || b._2) {
//            if (a._1.result() != b._1.result()) {
//              return false
//            }
//          }
//        }
//        return true;
//      }
//      case _ => return ???
//    }
//  }
//}
//
//
//trait AssignmentLevelNode{
//  var dependencies = 0
//  val dependers = ArrayBuffer[AssignmentLevelNode]()
//
//  def addDepender(that : AssignmentLevelNode) : Unit = {
//    dependers += that
//    that.dependencies += 1
//  }
//}
//case class AssignmentLevelSimple(that : Node,by : Node) extends AssignmentLevelNode{
//
//}
//class AssignmentLevelWhen(val cond: Node,val context : WhenContext) extends AssignmentLevelNode{
//  var whenTrue: AssignmentLevel = null
//  var whenFalse: AssignmentLevel = null
//  val whenTrueCmds,whenFalseCmds = ArrayBuffer[AssignmentLevelCmd]()
//
//  def isSwitchable: (Node,Node,AssignmentLevelWhen) = {
//    (if(cond.isInstanceOf[Bool]) cond.asInstanceOf[Bool].input else cond) match {
//      case cond : Operator.Enum.Equal => {
//        (cond.left,if(cond.right.isInstanceOf[SpinalEnumCraft[_]]) cond.right.asInstanceOf[SpinalEnumCraft[_]].input else cond.right) match {
//          case (c : SpinalEnumCraft[_],l : EnumLiteral[_]) => return (c,l,this)
//          case _ =>
//        }
//      }
//      case cond : Operator.BitVector.Equal => {
//        (cond.left,if(cond.right.isInstanceOf[BitVector]) cond.right.asInstanceOf[BitVector].input else cond.right) match {
//          case (c : BitVector,l : BitVectorLiteral) => return (c,l,this)
//          case _ =>
//        }
//      }
//
//
//      case _ =>
//    }
//    return null
//  }
//
//  def getElseWhenChain() : List[AssignmentLevelWhen] = {
//    // && whenFalse.content.head.isInstanceOf[AssignmentLevelWhen]
//    if(whenFalse.content.size == 1 && whenFalse.content.head.isInstanceOf[AssignmentLevelWhen]){
//      this :: whenFalse.content.head.asInstanceOf[AssignmentLevelWhen].getElseWhenChain()
//    }else{
//      List(this)
//    }
//  }
//}
//
//class AssignmentLevelSwitch(val key: Node) extends AssignmentLevelNode {
//  val cases = ArrayBuffer[SwitchTreeCase]()
//  var default : SwitchTreeDefault = null
//}
//
//case class SwitchTreeCase(const : Node,doThat : AssignmentLevel)
//case class SwitchTreeDefault(doThat : AssignmentLevel)
//case class AssignmentLevelCmd(that : Node,by : Node)
//
//class AssignmentLevel(inTasks : Seq[AssignmentLevelCmd]) {
//  val content = new ArrayBuffer[AssignmentLevelNode]()
//
//  def isNotEmpty = !content.isEmpty
//  def isOnlyAWhen = content.size == 1 && content.head.isInstanceOf[AssignmentLevelWhen]
//
//  def build(): Unit = {
//    def getElements(that : Node): Iterator[Node] = if (that.isInstanceOf[MultipleAssignmentNode])
//      that.getInputs else Iterator(that)
//
//    val whenMap = mutable.HashMap[WhenContext,AssignmentLevelWhen]()
//
//    inTasks.foreach(inTask => {
//      var previous : AssignmentLevelNode = null
//      getElements(inTask.by).foreach(input => input match {
//        case input : WhenNode => {
//          val temp = whenMap.getOrElseUpdate(input.w,{
//            val newOne = new AssignmentLevelWhen(input.cond,input.w)
//            content += newOne
//            newOne
//          })
//
//          if (input.whenTrue != null) {
//            temp.whenTrueCmds += AssignmentLevelCmd(inTask.that,input.whenTrue)
//          }
//          if (input.whenFalse != null) {
//            temp.whenFalseCmds += AssignmentLevelCmd(inTask.that,input.whenFalse)
//          }
//
//          if(previous != null) previous.addDepender(temp)
//          previous = temp
//        }
//        case reg: Reg =>
//        case input  => {
//          val temp = AssignmentLevelSimple(inTask.that,input)
//          content += temp
//          if(previous != null) previous.addDepender(temp)
//          previous = temp
//        }
//      })
//    })
//
//    val readyTasks = content.filter(_.dependencies == 0)
//    content.clear()
//
//    def flushTask(task : AssignmentLevelNode) : Unit = {
//      content += task
//      task.dependers.foreach(depender => {
//        depender.dependencies -= 1
//        if(depender.dependencies == 0) flushTask(depender)
//      })
//    }
//    readyTasks.foreach(flushTask)
//
//    content.foreach(_ match {
//      case task : AssignmentLevelWhen => {
//        task.whenTrue = new AssignmentLevel(task.whenTrueCmds)
//        task.whenFalse = new AssignmentLevel(task.whenFalseCmds)
//      }
//      case task : AssignmentLevelNode =>
//    })
//  }
//
//
//  def caseify() : Unit = {
//    var idx = content.size
//    while(idx != 0){
//      idx-=1;
//      content(idx) match {
//        case whenTree : AssignmentLevelWhen if whenTree.context.parentElseWhen == null => {
//          val chain = whenTree.getElseWhenChain()
//          if(chain.size > 1){
//            val switchable = chain.map(_.isSwitchable)
//            if(switchable.foldLeft(true)((carry,e) => carry && (e != null))){
//              val key = switchable.head._1
//              if(switchable.foldLeft(true)((carry,k) => carry && (k._1 == key))) {
//                val switchNode = new AssignmentLevelSwitch(key)
//                switchable.foreach { case (_, lit, src) => {
//                  switchNode.cases += SwitchTreeCase(lit, src.whenTrue)
//                }}
//                switchNode.default = SwitchTreeDefault(switchable.last._3.whenFalse)
//
//                content(idx) = switchNode
//              }
//            }
//          }
//        }
//        case _ =>
//      }
//    }
//
//  }
//
//  build()
//  caseify()
//}
//
//
trait MemBitsMaskKind
object MULTIPLE_RAM extends MemBitsMaskKind
object SINGLE_RAM extends MemBitsMaskKind

object VhdlVerilogBase{
  def getHeader(commentSymbole : String,toplevel : Component): String =
    s"""$commentSymbole Generator : SpinalHDL v${Spinal.version}    git head : ${spinal.core.Info.gitHash}
       |$commentSymbole Date      : ${new SimpleDateFormat("dd/MM/yyyy, HH:mm:ss").format(Calendar.getInstance().getTime)}
       |$commentSymbole Component : ${toplevel.definitionName}
       |
       |""".stripMargin
}


trait VhdlVerilogBase {
//  def isReferenceable(node: Node) = node.isInstanceOf[Nameable]
//
//  def getSensitivity(nodes: Iterable[Node], includeNodes: Boolean): mutable.Set[Node] = {
//    val sensitivity = mutable.Set[Node]()
//
//    if (includeNodes)
//      nodes.foreach(walk(_))
//    else
//      nodes.foreach(_.onEachInput(walk(_)))
//
//    def walk(node: Node): Unit = {
//      if (isReferenceable(node))
//        sensitivity += node
//      else
//        node.onEachInput(walk(_))
//    }
//
//    sensitivity
//  }
//
//
//  class Process(val order: Int) {
//    var sensitivity: mutable.Set[Node] = null
//    val nodes = ArrayBuffer[Node]()
//    val whens = ArrayBuffer[ConditionalContext]()
//    var hasMultipleAssignment = false
//
//    def genSensitivity: Unit = sensitivity = getSensitivity(nodes, false)
//
//
//    def needProcessDef: Boolean = {
//      if (!whens.isEmpty || nodes.size > 1) return true
//      if (hasMultipleAssignment) {
//        //        val ma: MultipleAssignmentNode = nodes(0).getInput(0).asInstanceOf[MultipleAssignmentNode]
//        //        val assignedBits = new AssignedBits(nodes(0).getWidth)
//        //        ma.onEachInput(_ match {
//        //          case assign: AssignmentNode => {
//        //            val scope = assign.getScopeBits
//        //            if (!AssignedBits.intersect(scope, assignedBits).isEmpty) return true
//        //            assignedBits.add(scope)
//        //          }
//        //          case _ => return true
//        //        })
//        return true; //Symplified because of Verilog backend
//      }
//      return false
//    }
//  }
//
//  def getAsyncProcesses(component: Component,merge : Boolean = true) : Seq[Process] = {
//    var processCounter = 0
//
//
//    val processSet = mutable.Set[Process]()
//    val whenToProcess = mutable.Map[ConditionalContext, Process]()
//
//    def move(to: Process, from: Process): Unit = {
//      to.nodes ++= from.nodes
//      to.whens ++= from.whens
//      to.hasMultipleAssignment |= from.hasMultipleAssignment
//      from.whens.foreach(whenToProcess(_) = to)
//      processSet.remove(from)
//    }
//
//    val asyncSignals = component.nodes.filter(_ match {
//      case signal: BaseType => (!signal.isDelay) && (!((signal.isIo && signal.isInput) || component.kindsOutputsBindings.contains(signal)))
//      case _ => false
//    })
//
//    for (signal <- asyncSignals) {
//      var process: Process = null
//      var hasMultipleAssignment = false
//      walk(signal.getInput(0))
//      def walk(that: Node): Unit = {
//        that match {
//          case wn: WhenNode => {
//            if (merge && whenToProcess.contains(wn.w)) {
//              val otherProcess = whenToProcess.get(wn.w).get
//              if (process == null) {
//                process = otherProcess
//                otherProcess.nodes += signal
//              } else if (process != otherProcess) {
//                move(otherProcess, process)
//                process = otherProcess
//              }
//            } else {
//              if (process == null) {
//                process = new Process(processCounter);
//                processCounter += 1
//                process.nodes += signal
//                processSet += process
//              }
//              process.whens += wn.w
//              whenToProcess += (wn.w -> process)
//            }
//
//            walk(wn.whenTrue)
//            walk(wn.whenFalse)
//          }
//
//          case man: MultipleAssignmentNode => {
//            man.onEachInput(walk(_))
//            hasMultipleAssignment = true
//          }
//          case that => {
//            if (process == null) {
//              process = new Process(processCounter);
//              processCounter += 1
//              process.nodes += signal
//              processSet += process
//            }
//          }
//        }
//      }
//
//      process.hasMultipleAssignment |= hasMultipleAssignment
//    }
//
//    val processList = processSet.toList.sortWith(_.order < _.order)
//    processList
//  }
//
}
