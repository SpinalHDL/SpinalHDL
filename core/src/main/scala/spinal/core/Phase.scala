package spinal.core
//
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
//
///**
// * Created by PIC32F_USER on 05/06/2016.
// */
//
class PhaseContext(val config : SpinalConfig){
  var globalData = GlobalData.reset
  config.applyToGlobalData(globalData)
  var dirtyConsumers = true
  val globalScope = new NamingScope()
  var topLevel: Component = null
//  val enums = mutable.Map[SpinalEnum,mutable.Set[SpinalEnumEncoding]]()
  val reservedKeyWords = mutable.Set[String](
    //VHDL
    "abs", "access", "after", "alias", "all",
    "and", "architecture", "array", "assert",
    "attribute", "begin", "block", "body",
    "buffer", "bus", "case", "component",
    "configuration", "constant", "disconnect", "downto",
    "else", "elsif", "end", "entity", "exit", "file",
    "for", "function", "generate", "generic",
    "group", "guarded", "if", "impure", "in",
    "inertial", "inout", "is", "label", "library",
    "linkage", "literal", "loop", "map", "mod",
    "nand", "new", "next", "nor", "not", "null",
    "of", "on", "open", "or", "others", "out",
    "package", "port", "postponed", "procedure",
    "process", "pure", "range", "record", "register",
    "reject", "rem", "report", "return", "rol",
    "ror", "select", "severity", "signal", "shared",
    "sla", "sll", "sra", "srl", "subtype", "then",
    "to", "transport", "type", "unaffected", "units",
    "until", "use", "variable", "wait", "when",
    "while", "with", "xnor", "xor",

    //Verilog + SystemVerilog
    "alias", "always", "always_comb", "always_ff",
    "always_latch", "and", "assert", "assign",
    "assume", "automatic", "before", "begin", "bind",
    "bins", "binsof", "bit", "break",
    "buf", "bufif0", "bufif1", "byte", "case", "casex",
    "casez", "cell", "chandle", "class", "clocking", "cmos",
    "config", "const", "constraint", "context", "continue",
    "cover", "covergroup", "coverpoint", "cross", "deassign",
    "default", "defparam", "design", "disable", "dist", "do",
    "edge", "else", "end", "endcase", "endclass", "endclocking",
    "endconfig", "endfunction", "endgenerate", "endgroup",
    "endinterface", "endmodule", "endpackage","endprimitive",
    "endprogram","endproperty","endspecify","endsequence",
    "endtable","endtask","enum","event","expect","export",
    "extends","extern","final","first_match","for","force",
    "foreach","forever","fork","forkjoin","function",
    "generate","genvar","highz0","highz1","if","iff",
    "ifnone","ignore_bins","illegal_bins","import","incdir",
    "include","initial","inout","input","inside",
    "instance","int","integer","interface","intersect",
    "join","join_any","join_none","large","liblist",
    "library","local","localparam","logic","longint",
    "macromodule","matches","medium","modport","module",
    "nand","negedge","new","nmos","nor","noshowcancelled","not",
    "notif0","notif1","null","or","output","package",
    "packed","parameter","pmos","posedge","primitive",
    "priority","program","property","protected","pull0",
    "pull1","pulldown","pullup","pulsestyle_onevent",
    "pulsestyle_ondetect","pure","rand","randc",
    "randcase","randsequence","rcmos","real","realtime",
    "ref","reg","release","repeat","return","rnmos",
    "rpmos","rtran","rtranif0","rtranif1","scalared",
    "sequence","shortint","shortreal","showcancelled",
    "signed","small","solve","specify","specparam",
    "static","string","strong0","strong1","struct",
    "super","supply0","supply1","table","tagged","task",
    "this","throughout","time","timeprecision","timeunit","tran",
    "tranif0","tranif1","tri","tri0","tri1","triand",
    "trior","trireg","type","typedef","union","unique",
    "unsigned","use","uwire","var","vectored","virtual",
    "void","wait","wait_order","wand","weak0","weak1",
    "while","wildcard","wire","with","within","wor",
    "xnor","xor"
  )


  reservedKeyWords.foreach(globalScope.allocateName(_))

  def components(): ArrayBuffer[Component] ={
    val ret = ArrayBuffer[Component]()
    ret.clear()
    def walk(c: Component): Unit = {
      ret += c
      c.children.foreach(walk(_))
    }
    walk(topLevel)
    ret
  }
  def sortedComponents = components.sortWith(_.level > _.level)

//  def walkNodesDefautStack = {
//    val nodeStack = mutable.Stack[Node]()
//
//    topLevel.getOrdredNodeIo.foreach(nodeStack.push(_))
//    components.foreach(c => {
//      c match {
//        case blackBox: BlackBox => blackBox.getOrdredNodeIo.filter(_.isInput).foreach(nodeStack.push(_))
//        case _ =>
//      }
//      c.additionalNodesRoot.foreach(nodeStack.push(_))
//    })
//    nodeStack
//  }
//
//  def walkNodesBlackBoxGenerics() = {
//    val nodeStack = mutable.Stack[Node]()
//    components.foreach(_ match {
//      case blackBox: BlackBox => {
//        blackBox.getGeneric.flatten.foreach(_ match {
//          case bt: BaseType => nodeStack.push(bt)
//          case _ =>
//        })
//      }
//      case _ =>
//    })
//    nodeStack
//  }
//
//  def fillNodeConsumer(): Unit = {
//    Node.walk(walkNodesDefautStack,(node)=>{
//      node.onEachInput(input => {
//        if (input != null) input.consumers += node
//      })
//    })
//  }
//
//  def removeNodeConsumer() : Unit = {
//    Node.walk(walkNodesDefautStack,_.consumers.clear())
//  }



  def checkGlobalData() : Unit = {
    if (!GlobalData.get.context.isEmpty) SpinalError("context stack is not empty :(")
  }

  def checkPendingErrors() = if(!globalData.pendingErrors.isEmpty) SpinalError()



//  def checkNoZeroWidth(): Unit ={
//    def zeroCheck (that : WidthProvider): Unit ={
//      if (that.getWidth < 1) {
//        println(that)
//      }
//    }
//
//    Node.walk(walkNodesDefautStack,_ match {
//      case node : WidthProvider => {
//        zeroCheck(node)
//      }
//      case _ =>
//    })
//  }
//
  def doPhase(phase: Phase): Unit ={
    if(phase.useNodeConsumers && dirtyConsumers){
//      removeNodeConsumer()
//      fillNodeConsumer()
      dirtyConsumers = false
    }
    phase.impl(this)
    checkPendingErrors()
//    if(phase.hasNetlistImpact){
//      dirtyConsumers = true
//    }
  }

}

trait Phase{
  def impl(pc: PhaseContext): Unit

  def hasNetlistImpact : Boolean
  def useNodeConsumers : Boolean
}

trait PhaseNetlist extends Phase{
  override def hasNetlistImpact: Boolean = true
}

trait PhaseMisc extends Phase{
  override def hasNetlistImpact : Boolean = false
}

trait PhaseCheck extends Phase{
  override def hasNetlistImpact : Boolean = false
}

//class MultiPhase(pc: PhaseContext) extends Phase{
//  val phases = ArrayBuffer[Phase]()
//
//  override def impl(pc : PhaseContext): Unit = {
//    phases.foreach(_.impl(pc : PhaseContext))
//  }
//}
//
//
////class PhaseFillComponentList(pc: PhaseContext) extends Phase{
////  override def impl(pc : PhaseContext): Unit = {
////    pc.fillComponentList()
////  }
////}
//
//
//
//class PhaseNodesBlackBoxGenerics(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    val nodeStack = mutable.Stack[Node]()
//    pc.components.foreach(_ match {
//      case blackBox: BlackBox => {
//        blackBox.getGeneric.flatten.foreach(_ match {
//          case bt: BaseType => nodeStack.push(bt)
//          case _ =>
//        })
//      }
//      case _ =>
//    })
//  }
//}
//
//class PhaseMoveLogicTags(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk(pc.walkNodesDefautStack,_ match{
//        case node : BaseType => {
//          if(node.input.isInstanceOf[SyncNode]){
//            val moves = node.filterTag(_.moveToSyncNode)
//            node.removeTags(moves)
//            node.input.addTags(moves)
//          }
//        }
//        case _ =>
//    })
//  }
//}
//
//
//class PhaseApplyIoDefault(pc: PhaseContext) extends PhaseNetlist{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk(pc.walkNodesDefautStack,node => {
//      node match{
//        case node : BaseType => {
//          if(node.input == null && node.defaultValue != null){
//            val c = node.dir match {
//              case `in` => node.component
//              case `out` => if(node.component.parent != null)
//                node.component.parent
//              else
//                null
//              case _ => node.component
//            }
//            if(c != null) {
//              node.dir match{
//                case `in` =>  {
//                  Component.push(c.parent)
//                  node.assignFrom(node.defaultValue)
//                  Component.pop(c.parent)
//                }
//                case _ => {
//                  Component.push(c)
//                  node.assignFrom(node.defaultValue)
//                  Component.pop(c)
//                }
//              }
//            }
//          }
//        }
//        case _ =>
//      }
//
//    })
//  }
//}
//
//class MemTopology(val mem: Mem[_]) {
//  val writes = ArrayBuffer[MemWrite]()
//  val readsAsync = ArrayBuffer[MemReadAsync]()
//  val readsSync = ArrayBuffer[MemReadSync]()
//  val writeReadSameAddressSync = ArrayBuffer[(MemWrite, MemReadSync)]() //DISABLED
//  val readWriteSync = ArrayBuffer[(MemReadWrite_writePart, MemReadWrite_readPart)]()
//}
//
//
//trait PhaseMemBlackboxing extends PhaseNetlist{
//  override def impl(pc: PhaseContext): Unit = {
//    import pc._
//    val memsTopo = mutable.Map[Mem[_], MemTopology]()
//
//    def topoOf(mem: Mem[_]) = memsTopo.getOrElseUpdate(mem, new MemTopology(mem))
//
//    Node.walk(pc.walkNodesDefautStack,node => node match {
//      case write: MemWrite => {
//        val memTopo = topoOf(write.getMem)
////        val readSync = memTopo.readsSync.find(readSync => readSync.address.getInput(0) == write.address.getInput(0)).orNull
////        if (readSync == null) {
//          memTopo.writes += write
////        } else {
////          memTopo.readsSync -= readSync
////          memTopo.writeReadSameAddressSync += (write -> readSync)
////          readSync.sameAddressThan(write)
////        }
//      }
//      case readAsync: MemReadAsync => topoOf(readAsync.getMem).readsAsync += readAsync
//      case readSync: MemReadSync => {
//        val memTopo = topoOf(readSync.getMem)
////        val write = memTopo.writes.find(write => readSync.address.getInput(0) == write.address.getInput(0)).orNull
////        if (write == null) {
//          memTopo.readsSync += readSync
////        } else {
////          memTopo.writes -= write
////          memTopo.writeReadSameAddressSync += (write -> readSync)
////          readSync.sameAddressThan(write)
////        }
//      }
//      case writePart: MemReadWrite_writePart => {
//        val memTopo = topoOf(writePart.getMem)
//        if (memTopo.readWriteSync.count(_._1 == writePart) == 0) {
//          memTopo.readWriteSync += (writePart -> writePart.readPart)
//        }
//      }
//      case readPart: MemReadWrite_readPart => {
//        val memTopo = topoOf(readPart.getMem)
//        if (memTopo.readWriteSync.count(_._2 == readPart) == 0) {
//          memTopo.readWriteSync += (readPart.writePart -> readPart)
//        }
//      }
//      case _ =>
//    })
//    doBlackboxing(pc,memsTopo)
//  }
//
//  def doBlackboxing(pc: PhaseContext,memTopologies : mutable.Map[Mem[_], MemTopology]) : Unit
//}
//
//abstract class PhaseMemBlackBoxingWithPolicy(policy : MemBlackboxingPolicy) extends PhaseMemBlackboxing{
//  override def doBlackboxing(pc: PhaseContext, memTopologies: mutable.Map[Mem[_], MemTopology]): Unit = {
//    import pc._
//    for ((mem, topo) <- memTopologies if policy.translationInterest(topo)) {
//      val message = doBlackboxing(topo)
//      if(message != null) policy.onUnblackboxable(topo,this,message)
//    }
//  }
//
//  //Return null if success
//  def doBlackboxing(memTopology: MemTopology) : String
//}
//
//
//class PhaseMemBlackBoxingDefault(policy : MemBlackboxingPolicy) extends PhaseMemBlackBoxingWithPolicy(policy){
//  override def useNodeConsumers = false
//
//  def doBlackboxing(topo: MemTopology) : String = {
//    val mem = topo.mem
//    mem.component.rework {
//      if (mem.initialContent != null) {
//        return "Can't blackbox ROM"//TODO
//      } else if (topo.writes.size == 1 && (!topo.readsAsync.isEmpty || !topo.readsSync.isEmpty) && topo.writeReadSameAddressSync.isEmpty && topo.readWriteSync.isEmpty) {
//        val wr = topo.writes(0)
//        for (rd <- topo.readsAsync) {
//          rd.checkInferedWidth
//          wr.checkInferedWidth
//          val clockDomain = wr.getClockDomain
//          clockDomain.push()
//
//          val ram = new Ram_1w_1ra(
//            wordWidth = mem.getWidth,
//            wordCount = mem.wordCount,
//            wrAddressWidth = wr.address.getWidth,
//            wrDataWidth = wr.data.getWidth,
//            rdAddressWidth = rd.address.getWidth,
//            rdDataWidth = rd.getData.getWidth,
//            wrMaskWidth = if (wr.mask != null) wr.mask.getWidth else 1,
//            wrMaskEnable = wr.mask != null,
//            readUnderWrite = rd.readUnderWrite,
//            technology = mem.technology
//          )
//          val enable = clockDomain.isClockEnableActive
//
//          ram.io.wr.en := wr.getEnable.allowSimplifyIt() && enable
//          ram.io.wr.addr := wr.getAddress.allowSimplifyIt()
//          ram.io.wr.data := wr.getData.allowSimplifyIt()
//          if (wr.mask != null)
//            ram.io.wr.mask := wr.getMask.allowSimplifyIt()
//          else
//            ram.io.wr.mask := "1"
//
//
//          ram.io.rd.addr := rd.getAddress.allowSimplifyIt()
//          rd.getData.allowSimplifyIt() := ram.io.rd.data
//
//          ram.setName(mem.getName())
//          clockDomain.pop()
//        }
//
//        for (rd <- topo.readsSync) {
//          rd.checkInferedWidth
//          wr.checkInferedWidth
//          val ram = new Ram_1w_1rs(
//            wordWidth = mem.getWidth,
//            wordCount = mem.wordCount,
//            wrClock = wr.getClockDomain,
//            rdClock = rd.getClockDomain,
//            wrAddressWidth = wr.address.getWidth,
//            wrDataWidth = wr.data.getWidth,
//            rdAddressWidth = rd.address.getWidth,
//            rdDataWidth = rd.getData.getWidth,
//            wrMaskWidth = if (wr.mask != null) wr.mask.getWidth else 1,
//            wrMaskEnable = wr.mask != null,
//            readUnderWrite = rd.readUnderWrite,
//            technology = mem.technology
//          )
//
//          ram.io.wr.en := wr.getEnable.allowSimplifyIt() && wr.getClockDomain.isClockEnableActive
//          ram.io.wr.addr := wr.getAddress.allowSimplifyIt()
//          ram.io.wr.data := wr.getData.allowSimplifyIt()
//          if (wr.mask != null)
//            ram.io.wr.mask := wr.getMask.allowSimplifyIt()
//          else
//            ram.io.wr.mask := "1"
//
//          ram.io.rd.en := rd.getReadEnable.allowSimplifyIt() && rd.getClockDomain.isClockEnableActive
//          ram.io.rd.addr := rd.getAddress.allowSimplifyIt()
//          rd.getData.allowSimplifyIt() := ram.io.rd.data
//
//          ram.generic.rdEnEnable = {
//            val lit = ram.io.rd.en.getLiteral[BoolLiteral]
//            lit == null || lit.value == false
//          }
//
//          ram.setName(mem.getName())
//        }
//
////      } else if (topo.writes.isEmpty && topo.readsAsync.isEmpty && topo.readsSync.isEmpty && topo.writeReadSameAddressSync.size == 1 && topo.readWriteSync.isEmpty) {
////        val wr = topo.writeReadSameAddressSync(0)._1
////        val rd = topo.writeReadSameAddressSync(0)._2
////        if (rd.getClockDomain.clock == wr.getClockDomain.clock) {
////          val clockDomain = wr.getClockDomain
////
////          clockDomain.push()
////
////          val ram = new Ram_1wrs(mem.getWidth, mem.wordCount, rd.readUnderWrite)
////          val enable = clockDomain.isClockEnableActive
////
////          ram.io.addr := wr.getAddress.allowSimplifyIt()
////          ram.io.wr.en := wr.getEnable.allowSimplifyIt() && enable
////          ram.io.wr.data := wr.getData.allowSimplifyIt()
////
////          ram.io.rd.en := rd.getReadEnable.allowSimplifyIt() && enable
////          rd.getData.allowSimplifyIt() := ram.io.rd.data
////
////          ram.generic.useReadEnable = {
////            val lit = ram.io.rd.en.getLiteral[BoolLiteral]
////            lit == null || lit.value == false
////          }
////
////          ram.setName(mem.getName())
////          clockDomain.pop()
////        } else {
////          return "Can't blackbox"//TODO
////        }
//      } else if (topo.writes.isEmpty && topo.readsAsync.isEmpty && topo.readsSync.isEmpty && topo.writeReadSameAddressSync.isEmpty && topo.readWriteSync.size == 1) {
//        val wr = topo.readWriteSync(0)._1
//        val rd = topo.readWriteSync(0)._2
//
//        wr.checkInferedWidth
//        rd.checkInferedWidth
//
//        val ram = new Ram_1wrs(mem.getWidth, mem.wordCount,mem.technology, rd.readUnderWrite)
//
//        ram.io.addr := wr.getAddress.allowSimplifyIt()
//        ram.io.en := wr.getChipSelect.allowSimplifyIt() && wr.getClockDomain.isClockEnableActive
//        ram.io.wr := wr.getWriteEnable.allowSimplifyIt()
//        ram.io.wrData := wr.getData.allowSimplifyIt()
//
//        rd.getData.allowSimplifyIt() := ram.io.rdData
//
//        ram.setName(mem.getName())
//      } else if (topo.writes.isEmpty && topo.readsAsync.isEmpty && topo.readsSync.isEmpty && topo.writeReadSameAddressSync.isEmpty && topo.readWriteSync.size == 2) {
//        val portA_wr = topo.readWriteSync(0)._1
//        val portA_rd = topo.readWriteSync(0)._2
//
//        val portB_wr = topo.readWriteSync(1)._1
//        val portB_rd = topo.readWriteSync(1)._2
//
//        portA_wr.checkInferedWidth
//        portA_rd.checkInferedWidth
//
//        portB_wr.checkInferedWidth
//        portB_rd.checkInferedWidth
//
//
//        val ram = new Ram_2wrs(
//          wordWidth = mem.getWidth,
//          wordCount = mem.wordCount,
//          technology = mem.technology,
//
//          portA_readUnderWrite = portA_rd.readUnderWrite,
//          portA_clock = portA_wr.getClockDomain,
//          portA_addressWidth = portA_wr.address.getWidth,
//          portA_dataWidth  = portA_wr.getWidth,
//          portA_maskWidth  = if(portA_wr.getMask != null) portA_wr.getMask.getWidth else 1,
//          portA_maskEnable  = portA_wr.getMask != null,
//
//          portB_readUnderWrite = portA_rd.readUnderWrite,
//          portB_clock = portB_wr.getClockDomain,
//          portB_addressWidth = portB_wr.address.getWidth,
//          portB_dataWidth  = portB_wr.getWidth,
//          portB_maskWidth  = if(portB_wr.getMask != null) portB_wr.getMask.getWidth else 1,
//          portB_maskEnable  = portB_wr.getMask != null
//        )
//
//        ram.io.portA.addr := portA_wr.getAddress.allowSimplifyIt()
//        ram.io.portA.en := portA_wr.getChipSelect.allowSimplifyIt() && portA_wr.getClockDomain.isClockEnableActive
//        ram.io.portA.wr := portA_wr.getWriteEnable.allowSimplifyIt()
//        ram.io.portA.wrData := portA_wr.getData.allowSimplifyIt()
//        ram.io.portA.mask := (if(portA_wr.getMask != null) portA_wr.getMask.allowSimplifyIt() else B"1")
//        portA_rd.getData.allowSimplifyIt() := ram.io.portA.rdData
//
//        ram.io.portB.addr := portB_wr.getAddress.allowSimplifyIt()
//        ram.io.portB.en := portB_wr.getChipSelect.allowSimplifyIt() && portB_wr.getClockDomain.isClockEnableActive
//        ram.io.portB.wr := portB_wr.getWriteEnable.allowSimplifyIt()
//        ram.io.portB.wrData := portB_wr.getData.allowSimplifyIt()
//        ram.io.portB.mask := (if(portB_wr.getMask != null) portB_wr.getMask.allowSimplifyIt() else B"1")
//        portB_rd.getData.allowSimplifyIt() := ram.io.portB.rdData
//
//        ram.setName(mem.getName())
//      } else {
//        return "Unblackboxable memory topology"//TODO
//      }
//    }
//    return null
//  }
//}
//
class PhaseNameNodesByReflection(pc: PhaseContext) extends PhaseMisc{
  override def useNodeConsumers = false
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    globalData.nodeAreNamed = true
    if (topLevel.getName() == null) topLevel.setWeakName("toplevel")
    for (c <- sortedComponents) {
      c.nameElements()
      if(c.definitionName == null) {
        c.definitionName = pc.config.globalPrefix + c.getClass.getSimpleName
      }
      c match {
//        case bb: BlackBox => {
//          bb.getGeneric.genNames
//        }
        case _ =>
      }
    }
  }
}

//class PhaseCollectAndNameEnum(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk(walkNodesDefautStack,node => {
//      node match {
//        case enum: SpinalEnumCraft[_] => enums.getOrElseUpdate(enum.spinalEnum,null) //Encodings will be added later
//        case _ =>
//      }
//    })
//
//    val scope = pc.globalScope.newChild
//    enums.keys.foreach(e => {
//      val name = if(e.isNamed)
//        e.getName()
//      else
//        e.getClass.getSimpleName.replace("$","")
//
//      e.setName(scope.allocateName(name))
//    })
//
//    for (enumDef <- enums.keys) {
//      Misc.reflect(enumDef, (name, obj) => {
//        obj match {
//          case obj: Nameable => obj.setWeakName(scope.getUnusedName(name))
//          case _ =>
//        }
//      })
//      for (e <- enumDef.elements) {
//        if (e.isUnnamed) {
//          e.setWeakName(scope.getUnusedName("e" + e.position))
//        }
//      }
//    }
//  }
//}
//
class PhasePullClockDomains(pc: PhaseContext) extends PhaseNetlist{
  override def useNodeConsumers = false
  override def impl(pc : PhaseContext): Unit = {
    import pc._

    GraphUtils.walkAllComponents(pc.topLevel,c => {
      val cds = mutable.HashSet[ClockDomain]()
      c.ownNameableNodes.foreach(_ match {
        case bt : BaseType if bt.isReg => {
          val cd = bt.dslContext.clockDomain
          if(bt.isUsingResetSignal && (!cd.hasResetSignal && !cd.hasSoftResetSignal))
            SpinalError(s"Clock domain without reset contain a register which needs one\n ${bt.getScalaLocationLong}")

          cds += cd
        }
        case _ =>
      })

      c.rework{
        for(cd <- cds){
          cd.readClockWire
          if(cd.hasResetSignal) cd.readResetWire
          if(cd.hasSoftResetSignal) cd.readSoftResetWire
          if(cd.hasClockEnableSignal) cd.readClockEnableWire
        }
      }
    })
//    Node.walk(walkNodesDefautStack,(node, push) =>  {
//      node match {
//        case delay: SyncNode => {
//          val clockDomain = delay.getClockDomain
//          if(delay.isUsingResetSignal && (!clockDomain.hasResetSignal && !clockDomain.hasSoftResetSignal))
//            SpinalError(s"Clock domain without reset contain a register which needs one\n ${delay.getScalaLocationLong}")
//
//          Component.push(delay.component)
//          delay.setInput(SyncNode.getClockInputId,clockDomain.readClockWire)
//
//          if(delay.isUsingResetSignal)      delay.setInput(SyncNode.getClockResetId,clockDomain.readResetWire.dontSimplifyIt())
//          if(delay.isUsingSoftResetSignal)  delay.setInput(SyncNode.getClockSoftResetId,clockDomain.readSoftResetWire.dontSimplifyIt())
//          if(delay.isUsingEnableSignal)     delay.setInput(SyncNode.getClockEnableId,clockDomain.readClockEnableWire.dontSimplifyIt())
//          Component.pop(delay.component)
//        }
//        case _ =>
//      }
//      node.onEachInput(push(_))
//    })
  }
}
//
//class PhaseCheck_noNull_noCrossHierarchy_noInputRegister_noDirectionLessIo(pc: PhaseContext) extends PhaseCheck{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    val errors = mutable.ArrayBuffer[String]()
//
//    for(c <- components){
//      try{
//        val io = c.reflectIo
//        for(bt <- io.flatten){
//          if(bt.isDirectionLess){
//            errors += s"Direction less signal into io def ${bt.getScalaLocationLong}"
//          }
//        }
//      }catch{
//        case _ : Throwable =>
//      }
//
//    }
//    if(!errors.isEmpty)
//      SpinalError(errors)
//
//    Node.walk(walkNodesDefautStack,node => {
//      node match {
//        case node: BaseType => {
//          val nodeInput0 = node.input
//          if (nodeInput0 != null) {
//            if (node.isInput && nodeInput0.isInstanceOf[Reg] && nodeInput0.component == node.component) {
//              errors += s"Input register are not allowed ($node) \n${node.getScalaLocationLong}"
//            } else {
//              val nodeInput0IsIo = nodeInput0.isInstanceOf[BaseType] && nodeInput0.asInstanceOf[BaseType].isIo
//              if (node.isIo) {
//                if (node.isInput) {
//                  if (nodeInput0.component != node.component.parent && !(!nodeInput0.component.isTopLevel && nodeInput0IsIo && nodeInput0.component.parent == node.component.parent)) {
//                    if (nodeInput0.component == node.component)
//                      errors += s"Input $node can't be assigned from inside at\n${ScalaLocated.long(node.assignementThrowable)}"
//                    else
//                      errors += s"Input $node is not assigned by parent component but another at\n${ScalaLocated.long(node.assignementThrowable)}"
//                  }
//                } else if (node.isOutput) {
//                  if (nodeInput0.component != node.component && !(nodeInput0IsIo && node.component == nodeInput0.component.parent))
//                    errors += s"Output $node is not assigned by his component but an other at\n${ScalaLocated.long(node.assignementThrowable)}"
//                } else
//                  errors += s"No direction specified on IO \n${node.getScalaLocationLong}"
//              } else {
//                if (nodeInput0.component != node.component && !(nodeInput0IsIo && node.component == nodeInput0.component.parent))
//                  errors += s"Node $node is assigned outside his component at\n${ScalaLocated.long(node.assignementThrowable)}"
//              }
//            }
//          } else {
//            if (!(node.isInput && node.component.isTopLevel) && !(node.isOutput && node.component.isInstanceOf[BlackBox]))
//              errors += s"No driver on $node at \n${node.getScalaLocationLong}"
//          }
//        }
//        case _ => {
//          node.onEachInput((in,idx) => {
//            if (in == null) {
//              errors += s"No driver on $node at ${node.getScalaLocationLong}"
//            } else {
//              if (in.component != node.component && !(in.isInstanceOf[BaseType] && in.asInstanceOf[BaseType].isIo && node.component == in.component.parent)) {
//                val throwable = node match{
//                  case node : AssignementTreePart => node.getAssignementContext(idx)
//                  case _ => node.scalaTrace
//                }
//                errors += s"$node is driven by $in, which is a hierarchy violation\n${ScalaLocated.long(throwable)}"
//              }
//            }
//          })
//        }
//      }
//    })
//    if (!errors.isEmpty)
//      SpinalError(errors)
//  }
//
//}
//
//class PhaseAddInOutBinding(pc: PhaseContext) extends PhaseNetlist{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk(walkNodesDefautStack,(node,push) => {
//      //Create inputss bindings, usefull if the node is driven by when statments
//      if (node.isInstanceOf[BaseType] && node.component.parent != null) {
//        val baseType = node.asInstanceOf[BaseType]
//        if (baseType.isInput) {
//          val inBinding = cloneOf(baseType) //To be sure that there is no need of resize between it and node
//          inBinding.assignementThrowable = baseType.assignementThrowable
//          inBinding.scalaTrace = baseType.scalaTrace
//          inBinding.input = baseType.input
//          baseType.input = inBinding
//          inBinding.component = node.component.parent
//          inBinding.dontCareAboutNameForSymplify = true
//        }
//      }
//
//      node.onEachInput(push(_))
//
//      //Create outputs bindings
//      node.onEachInput((nodeInput,i) => {
//        val nodeInput = node.getInput(i)
//        nodeInput match {
//          case nodeInput: BaseType => {
//            if (nodeInput.isOutput && (nodeInput.component.parent == node.component || (nodeInput.component.parent == node.component.parent && nodeInput.component != node.component))) {
//              val into = nodeInput.component.parent
//              val bind = into.kindsOutputsToBindings.getOrElseUpdate(nodeInput, {
//                val bind = cloneOf(nodeInput)
//                bind.scalaTrace = nodeInput.scalaTrace
//                bind.assignementThrowable = nodeInput.assignementThrowable
//                into.kindsOutputsToBindings.put(nodeInput, bind)
//                into.kindsOutputsBindings += bind
//                bind.component = into
//                bind.input = nodeInput
//                bind.dontCareAboutNameForSymplify = true
//                bind
//              })
//
//              node.setInput(i,bind)
//
//              //Update pulledDataCache with the output binding to make it consistent
//              if(node.component.pulledDataCache.contains(nodeInput)){
//                node.component.pulledDataCache(nodeInput) = bind
//              }
//            }
//          }
//          case _ =>
//        }
//      })
//    })
//  }
//}
//
//class PhaseNameBinding(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//  import pc._
//    for (c <- components) {
//      for ((bindedOut, bind) <- c.kindsOutputsToBindings) {
//        if (bind.isUnnamed && bindedOut.component.isNamed && bindedOut.isNamed) {
//          bind.setWeakName(bindedOut.component.getName() + "_" + bindedOut.getName())
//        }
//      }
//    }
//
//    Node.walk(walkNodesDefautStack,node => node match {
//      case node: BaseType => {
//        if (node.isInput && node.input != null && node.input.isInstanceOf[Nameable]) {
//          val nameable = node.input.asInstanceOf[Nameable]
//          if (nameable.isUnnamed && node.component.isNamed && node.isNamed) {
//            nameable.setWeakName(node.component.getName() + "_" + node.getName())
//          }
//        }
//      }
//      case _ =>
//    })
//  }
//}
//
//class PhaseAllowNodesToReadOutputs(pc: PhaseContext) extends PhaseNetlist{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    val outputsBuffers = mutable.Map[BaseType, BaseType]()
//    Node.walk(walkNodesDefautStack,node => {
//      node.onEachInput((nodeInput,i) => {
//        nodeInput match {
//          case baseTypeInput: BaseType => {
//            if (baseTypeInput.isOutput && baseTypeInput.component.parent != node.component) {
//              val buffer = outputsBuffers.getOrElseUpdate(baseTypeInput, {
//                val buffer = cloneOf(baseTypeInput)
//                buffer.input = baseTypeInput.input
//                baseTypeInput.input = buffer
//                buffer.component = baseTypeInput.component
////                if(baseTypeInput.isNamed){
////                  buffer.setWeakName(baseTypeInput.getName() + "_readableBuffer")
////                }
//                SpinalTagReady.splitNewSink(source=baseTypeInput,sink=buffer)
//                buffer
//              })
//              node.setInput(i,buffer)
//            }
//          }
//          case _ =>
//        }
//      })
//    })
//  }
//}
//
//class PhaseAllowNodesToReadInputOfKindComponent(pc: PhaseContext) extends PhaseNetlist{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk(walkNodesDefautStack,node => {
//      node.onEachInput((input,i) => {
//        input match {
//          case baseTypeInput: BaseType => {
//            if (baseTypeInput.isInput && baseTypeInput.component.parent == node.component) {
//              node.setInput(i,baseTypeInput.input)
//            }
//          }
//          case _ =>
//        }
//      })
//    })
//  }
//}
//
//class PhasePreInferationChecks(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    val errors = mutable.ArrayBuffer[String]()
//    Node.walk(walkNodesDefautStack ++ walkNodesBlackBoxGenerics,_.preInferationCheck())
//    if(!errors.isEmpty)
//      SpinalError(errors)
//  }
//}
//
//class PhaseInferWidth(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    globalData.nodeAreInferringWidth = true
//    val nodes = ArrayBuffer[Node with Widthable]()
//    Node.walk(walkNodesDefautStack ++ walkNodesBlackBoxGenerics,node => {
//      if(node.isInstanceOf[Node with Widthable]) nodes += node.asInstanceOf[Node with Widthable]
//    })
//
//
//    def checkAll(): Unit = {
//      val errors = mutable.ArrayBuffer[String]()
//      for (node <- nodes) {
//        if (node.inferWidth && !node.isInstanceOf[Reg]) {
//          //Don't care about Reg width inference
//          errors += s"Can't infer width on ${node.getScalaLocationLong}"
//        }
//        if (node.widthWhenNotInferred != -1 && node.widthWhenNotInferred != node.getWidth) {
//          errors += s"getWidth call result during elaboration differ from inferred width on\n${node.getScalaLocationLong}"
//        }
//      }
//      if (errors.nonEmpty)
//        SpinalError(errors)
//    }
//
//    var iterationCounter = 0
//    while (true) {
//      iterationCounter = iterationCounter + 1
//      var somethingChange = false
//      for (node <- nodes) {
//        val hasChange = node.inferWidth
//        somethingChange = somethingChange || hasChange
//      }
//
//      if (!somethingChange || iterationCounter == nodes.size) {
//        checkAll()
//        return
//      }
//    }
//  }
//}
//
//
//class PhaseInferEnumEncodings(pc: PhaseContext,encodingSwap : (SpinalEnumEncoding) => SpinalEnumEncoding) extends PhaseMisc{
//  override def useNodeConsumers = true
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    globalData.nodeAreInferringEnumEncoding = true
//    val nodes = ArrayBuffer[Node with EnumEncoded]()
//    val nodesInferrable = ArrayBuffer[Node with InferableEnumEncoding]()
//    Node.walk(walkNodesDefautStack ++ walkNodesBlackBoxGenerics,node => {
//      if(node.isInstanceOf[EnumEncoded]) nodes += node.asInstanceOf[Node with EnumEncoded]
//      if(node.isInstanceOf[InferableEnumEncoding]) nodesInferrable += node.asInstanceOf[Node with InferableEnumEncoding]
//    })
//
//    nodesInferrable.foreach(node => {
//      node.bootInferration()
//    })
//
//    nodes.foreach(enum => {
//      enum.swapEncoding(encodingSwap(enum.getEncoding))
//    })
//
//    nodes.foreach(enum => {
//      if(enum.propagateEncoding){
//        val alreadyWalkeds = mutable.Set[Node]()
//        def propagateOn(that : Node): Unit = {
//          that match {
//            case that : InferableEnumEncoding => {
//              if(alreadyWalkeds.contains(that)) return
//              alreadyWalkeds += that
//              if(that.encodingProposal(enum.getEncoding)) {
//                that.onEachInput(propagateOn(_))
//                that.consumers.foreach(propagateOn(_))
//              }
//            }
//            case _ =>
//          }
//        }
//        enum.onEachInput(propagateOn(_))
//        enum.consumers.foreach(propagateOn(_))
//      }
//    })
//
//    //Feed enums with encodings
//    enums.keySet.foreach(enums(_) = mutable.Set[SpinalEnumEncoding]())
//    nodes.foreach(enum => {
//      enums(enum.getDefinition) += enum.getEncoding
//    })
//
//
//    //give a name to unamed encodings
//    val unamedEncodings = enums.valuesIterator.flatten.toSet.withFilter(_.isUnnamed).foreach(_.setWeakName("anonymousEnc"))
//
//    //Check that there is no encoding overlaping
//    for((enum,encodings) <- enums){
//      for(encoding <- encodings) {
//        val reserveds = mutable.Map[BigInt, ArrayBuffer[SpinalEnumElement[_]]]()
//        for(element <- enum.elements){
//          val key = encoding.getValue(element)
//          reserveds.getOrElseUpdate(key,ArrayBuffer[SpinalEnumElement[_]]()) += element
//        }
//        for((key,elements) <- reserveds){
//          if(elements.length != 1){
//            PendingError(s"Conflict in the $enum enumeration with the '$encoding' encoding with the key $key' and following elements:.\n${elements.mkString(", ")}\n\nEnumeration defined at :\n${enum.getScalaLocationLong}Encoding defined at :\n${encoding.getScalaLocationLong}")
//          }
//        }
//      }
//    }
//  }
//}
//
//
//class PhaseSimplifyNodes(pc: PhaseContext) extends PhaseNetlist{
//  override def useNodeConsumers = true
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk(walkNodesDefautStack,_.simplifyNode)
//  }
//}
//
//class PhaseResizeLiteralSimplify(pc: PhaseContext) extends PhaseNetlist{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk(walkNodesDefautStack,node => node.onEachInput((input,id) => input match{
//      case resize : Resize => {
//        if(resize.input.getWidth == 0){
//          val newNode = resize match{
//            case _ : ResizeBits => BitsLiteral(0,resize.getWidth)
//            case _ : ResizeUInt => UIntLiteral(0,resize.getWidth)
//            case _ : ResizeSInt => SIntLiteral(0,resize.getWidth)
//          }
//          newNode.inferredWidth = resize.getWidth
//          node.setInput(id,newNode)
//        }
//      }
//      case _ =>
//    }))
//
//  }
//}
//
//class PhasePropagateBaseTypeWidth(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk(walkNodesDefautStack,node => {
//      node match {
//        case node: BitVector => {
//          val width = node.getWidth
//
//          node.input match {
//            case that: RegWidthable => {
//              that.inferredWidth = width
//              if(that.initialValue != null) walk(that,RegS.getInitialValueId)
//              walk(that,RegS.getDataInputId)
//            }
//            case _ => walk(node,0)
//          }
//          walk(node,0)
//
//          def walk(parent: Node,inputId : Int): Unit = {
//            val that = parent.getInput(inputId)
//            def walkChildren() : Unit = that.onEachInput((input,id) => walk(that,id))
//
//            that match {
//              case that: WhenNodeWidthable => {
//                that.inferredWidth = width
//                walk(that,1)
//                walk(that,2)
//              }
//              case that: MultipleAssignmentNodeWidthable => {
//                that.inferredWidth = width
//                walkChildren()
//              }
//              case that : AssignementNodeWidthable => that.inferredWidth = width
//              case dontCare : DontCareNodeFixed =>{
//                dontCare.inferredWidth = width
//              }
//              case dontCare : DontCareNodeInfered =>{
//                dontCare.inferredWidth = width
//              }
//              case _ =>
//            }
//          }
//
//        }
//        case _ =>
//      }
//    })
//
//  }
//}
//
//class PhaseNormalizeNodeInputs(pc: PhaseContext) extends PhaseNetlist{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk(walkNodesDefautStack,(node,push) => {
//      node.onEachInput(push(_))
//      node.normalizeInputs
//    })
//  }
//}
//
//class PhaseCheckInferredWidth(pc: PhaseContext) extends PhaseCheck{
//  override def useNodeConsumers = true
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    val errors = mutable.ArrayBuffer[String]()
//
//    def checker(node : Node) : Unit = {
//      node match {
//        case node: Node with Widthable if (node.getWidth < 0) =>
//          PendingError(s"$node has a negative number of bits\n${node.getScalaLocationLong}")
//        case node: CheckWidth =>
//          node.checkInferedWidth
//        case _ =>
//      }
//    }
//    Node.walk(walkNodesDefautStack,checker(_))
//  }
//}
//
//class PhaseKeepAll(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    def walker(that : Any): Unit ={
//      Misc.reflect(that.asInstanceOf[Object], (name, obj) => {
//        obj match {
//          case area: Area => walker(area)
//          case data: Data => data.keep()
//          case _ =>
//        }
//      })
//    }
//
//    for(c <- components()){
//      walker(c)
//    }
//  }
//}
//
//
//
//
//class PhaseCheckCombinationalLoops(pc: PhaseContext) extends PhaseCheck{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    val targetAlgoId = GlobalData.get.allocateAlgoId()
//
//    val errors = mutable.ArrayBuffer[String]()
//    val pendingNodes = mutable.Stack[Node]()
//    pendingNodes.pushAll(walkNodesDefautStack)
//
//    def nodeIsCompleted(node: Node) = node.algoId = targetAlgoId
//    def isNodeCompleted(node : Node) = node.algoId == targetAlgoId
//
//    def getNodeWidth(that : Node): Int = that match {
//      case that : WidthProvider => that.getWidth
//      case _ => 1 //Pessimistic for enum
//    }
//
//    while (!pendingNodes.isEmpty) {
//      val pop = pendingNodes.pop()
//      walk(scala.collection.immutable.HashMap[Node, AssignedBits](),Nil,pop,getNodeWidth(pop)-1,0)
//    }
//
//    if (!errors.isEmpty)
//      SpinalError(errors)
//
//    def walk(consumers :  scala.collection.immutable.HashMap[Node, AssignedBits],stack : List[(Node,Int,Int)],
//             node: Node,
//             outHi : Int, outLo : Int): Unit = {
//      if (node == null || node.component == null) {
//
//      }else {
//        val newStack = Tuple3(node,outHi,outLo) :: stack
//        var bitsAlreadyUsed = consumers.getOrElse(node, new AssignedBits(getNodeWidth(node)))
//        if (bitsAlreadyUsed.isIntersecting(AssignedRange(outHi, outLo))) {
//          val ordred = newStack.reverseIterator
//          val filtred = ordred.dropWhile((e) => (e._1 != node || e._2 < outLo || e._3 > outHi)).drop(1).toArray
//          if(!filtred.exists(_._1.hasTag(noCombinatorialLoopCheck))) {
//            // val filtredNode = filtred.map(_._1)
//            val wellNameLoop = filtred.reverseIterator.filter { case (n, hi, lo) => n.isInstanceOf[Nameable] && n.asInstanceOf[Nameable].isNamed }.map { case (n, hi, lo) => n.asInstanceOf[Nameable].toString() + s"[$hi:$lo]" }.foldLeft("")(_ + _ + " ->\n      ")
//            val multiLineLoop = filtred.reverseIterator.map(n => "      " + n.toString).foldLeft("") (_ + "\n" + _)
//            errors += s"  Combinatorial loop !\n      Partial chain :\n      ${wellNameLoop}\n      Full chain :\n${multiLineLoop}"
//          }
//        }else if (!isNodeCompleted(node)) {
//          node match {
//            case syncNode: SyncNode => {
//              nodeIsCompleted(node)
//              val newConsumers = consumers + (node -> bitsAlreadyUsed.+(AssignedRange(outHi, outLo)))
//              val syncNode = node.asInstanceOf[SyncNode]
//              syncNode.getSynchronousInputs.foreach(addPendingNode(_))
//              syncNode.getAsynchronousInputs.foreach(i => walk(newConsumers,newStack, i, getNodeWidth(i) - 1, 0)) //TODO, pessimistic
//            }
//            case baseType: BaseType => {
//              val consumersPlusFull = consumers + (baseType -> bitsAlreadyUsed.+(AssignedRange(getNodeWidth(node) - 1, 0)))
//              def walkBaseType(node: Node): Unit = {
//                if (node != null) {
//                  node match {
//                    case node: MultipleAssignmentNode => node.onEachInput(input => walkBaseType(input))
//                    case node: WhenNode => {
//                      walk(consumersPlusFull,newStack, node.cond, 0, 0) //Todo, to pessimistic !
//                      walkBaseType(node.whenTrue)
//                      walkBaseType(node.whenFalse)
//                    }
//                    case node: AssignementNode => {
//                      val newConsumers = consumers + (baseType -> bitsAlreadyUsed.+(node.getScopeBits))
//                      node.onEachInput((input,idx) => {
//                        val (inHi, inLo) = node.getOutToInUsage(idx, outHi, outLo)
//                        if (inHi >= inLo) walk(newConsumers,newStack, input, inHi, inLo)
//                      })
//                    }
//                    case _ => {
//                      walk(consumersPlusFull,newStack, node, outHi, outLo)
//                    }
//                  }
//                }
//              }
//
//              walkBaseType(baseType.input)
//            }
//            case _ => {
//              val newConsumers = consumers + (node -> bitsAlreadyUsed.+(AssignedRange(outHi, outLo)))
//              node.onEachInput((input,idx) => {
//                if (input != null) {
//                  val (inHi, inLo) = node.getOutToInUsage(idx, outHi, outLo)
//                  if (inHi >= inLo) walk(newConsumers,newStack, input, inHi, inLo)
//                }
//              })
//            }
//          }
//          if (outHi == getNodeWidth(node) - 1 && outLo == 0) nodeIsCompleted(node)
//        }
//      }
//    }
//    def addPendingNode(node: Node) = {
//      if (node != null && ! isNodeCompleted(node)) pendingNodes.push(node)
//    }
//
//  }
//}
//
//class PhaseCheckCrossClockDomains(pc: PhaseContext) extends PhaseCheck{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    val errors = mutable.ArrayBuffer[String]()
//
//    Node.walk(walkNodesDefautStack,node => {
//      node match {
//        case syncNode: SyncNode => {
//          if (!syncNode.hasTag(crossClockDomain)) {
//            val consumerCockDomain = syncNode.getClockDomain
//            for (syncInput <- syncNode.getSynchronousInputs) {
//              val walked = mutable.Set[Object]() //TODO upgrade it to the check bit by bit
//              check(syncInput)
//              def check(that: Node): Unit = {
//                if(walked.contains(that)) return;
//                walked += that
//                if(that == null){
//                  println(":(")
//                }
//                if (!that.hasTag(crossClockDomain)) {
//                  that match {
//                    case syncDriver: SyncNode => {
//                      val driverClockDomain = syncDriver.getClockDomain
//                      if (//syncDriver.getClockDomain.clock != consumerCockDomain.clock &&
//                        ! driverClockDomain.isSyncronousWith(consumerCockDomain)) {
//                        errors += s"Synchronous element ${syncNode.getScalaLocationShort} is driven " +
//                          s"by ${syncDriver.getScalaLocationShort} but they don't have the same clock domain. " +
//                          s"Register declaration at \n${syncNode.getScalaLocationLong}"
//                      }
//                    }
//                    case _ => that.onEachInput(input => if (input != null) check(input))
//                  }
//                }
//              }
//            }
//          }
//        }
//        case _ =>
//      }
//    })
//
//    if (!errors.isEmpty)
//      SpinalError(errors)
//  }
//}
//
//
//class PhaseCheckMisc(pc: PhaseContext) extends PhaseCheck{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk(walkNodesDefautStack,node => {
//      node match {
//        case baseType: BaseType => {
//          if(baseType.hasTag(randomBoot)){
//            if(!baseType.isReg){
//              pc.globalData.pendingErrors += (() => s"$baseType has the randBoot tag set but is not a register\n ${baseType.getScalaLocationLong}")
//            }
//          }
//        }
//        case _ =>
//      }
//    })
//  }
//}
//
////class PhaseFillNodesConsumers(pc: PhaseContext) extends Phase{
////  override def impl(pc : PhaseContext): Unit = {
////    pc.fillNodeConsumer()
////  }
////}
//
//
//
//class PhaseDontSymplifyBasetypeWithComplexAssignement(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk(walkNodesDefautStack,node => {
//      node match {
//        case baseType: BaseType => {
//          baseType.input match {
//            case wn: WhenNode => baseType.dontSimplifyIt()
//            case an: AssignementNode => baseType.dontSimplifyIt()
//            case man: MultipleAssignmentNode => baseType.dontSimplifyIt()
//            case _ =>
//          }
//        }
//        case _ =>
//      }
//    })
//  }
//}
//
//
//
class PhaseDeleteUselessBaseTypes(pc: PhaseContext) extends PhaseNetlist{
  override def useNodeConsumers = true
  override def impl(pc : PhaseContext): Unit = {
    import pc._

    //Reset algoId of all referenced driving things
    GraphUtils.walkAllComponents(pc.topLevel, c => {
      c.dslBody.walkStatements(_.walkDrivingExpressions(_ match {
        case ref: RefExpression => {
          ref.source.algoId = 0
        }
        case _ =>
      }))
    })

    //Count the number of driving reference done on each ref.source
    GraphUtils.walkAllComponents(pc.topLevel, c => {
      c.dslBody.walkStatements(_.walkDrivingExpressions(_ match {
        case ref : RefExpression => {
          ref.source.algoId += 1
        }
        case _ =>
      }))
    })

    GraphUtils.walkAllComponents(pc.topLevel, c => {
      c.dslBody.walkStatements(s => {
        //Bypass useless basetypes (referenced only once)
        s.walkRemapDrivingExpressions(_ match {
          case ref : RefExpression => {
            val source = ref.source
            if(source.algoId == 1 && source.isComb && source.isDirectionLess && source.canSymplifyIt && source.hasOnlyOneStatement){ //TODO IR keep it
              source.algoId = 0
              source.headStatement.removeStatement()
              source.headStatement.source
            } else {
              source.algoId = 0
              ref
            }
          }
          case e => e
        })

        //Remove assignement which drive useless base types
        s match {
          case s : AssignementStatement => {
            val target = s.finalTarget
            if(target.algoId == 0 && target.isDirectionLess && target.canSymplifyIt){
              s.removeStatement()
            }
          }
          case _ =>
        }
      })
    })
  }
}
//
//class PhaseCheck_noAsyncNodeWithIncompleteAssignment(pc: PhaseContext) extends PhaseCheck{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    val errors = mutable.ArrayBuffer[String]()
//
//    Node.walk(walkNodesDefautStack,node => node match {
//      case signal: BaseType if !signal.isDelay && node.component != null && !(signal.component.isInBlackBoxTree && !signal.isInput) && !(signal.component.parent == null && signal.isInput) => {
//        val signalRange = new AssignedRange(signal.getBitsWidth - 1, 0)
//
//        def walk(nodes: Iterator[Node]): AssignedBits = {
//          val assignedBits = new AssignedBits(signal.getBitsWidth)
//
//          for (node <- nodes) node match {
//            case wn: WhenNode => {
//              assignedBits.add(AssignedBits.intersect(walk(Iterator(wn.whenTrue)), walk(Iterator(wn.whenFalse))))
//            }
//            case an: AssignementNode => {
//              assignedBits.add(an.getAssignedBits)
//            }
//            case man: MultipleAssignmentNode => return walk(man.getInputs)
//            case null =>
//            case _ => assignedBits.add(signalRange)
//          }
//          assignedBits
//        }
//
//        val assignedBits = walk(signal.getInputs)
//
//        val unassignedBits = new AssignedBits(signal.getBitsWidth)
//        unassignedBits.add(signalRange)
//        unassignedBits.remove(assignedBits)
//        if (!unassignedBits.isEmpty) {
//          if(unassignedBits.isFull)
//            errors += s"Combinatorial signal $signal has no default value => LATCH, defined at\n${signal.getScalaLocationLong}"
//          else
//            errors += s"Incomplete assignment is detected on the combinatorial signal $signal, unassigned bit mask " +
//              s"is ${unassignedBits.toBinaryString}, declared at\n${signal.getScalaLocationLong}"
//        }
//      }
//      case _ =>
//    })
//
//    if (errors.nonEmpty)
//      SpinalError(errors)
//  }
//}
//
//class PhaseSimplifyBlacBoxGenerics(pc: PhaseContext) extends PhaseNetlist{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    components.foreach(_ match {
//      case blackBox: BlackBox => {
//        blackBox.getGeneric.flatten.foreach(tuple => {
//          val signal = tuple
//          if (signal.isInstanceOf[BaseType]) {
//            val baseType = signal.asInstanceOf[BaseType]
//            walk(baseType, baseType)
//            def walk(node: Node, first: Node): Unit = node match {
//              case node: BaseType => {
//                first.setInput(0,node.input)
//                if(node.input.isInstanceOf[Widthable])
//                  first.getInput(0).asInstanceOf[Widthable].inferredWidth = first.asInstanceOf[Widthable].inferredWidth
//                walk(node.input, first)
//              }
//              case lit: Literal =>
//              case _ => throw new Exception("BlackBox generic must be literal")
//            }
//          }
//
//        })
//      }
//      case _ =>
//    })
//  }
//}
//
//class PhasePrintUnUsedSignals(prunedSignals : mutable.Set[BaseType],unusedSignals : mutable.Set[BaseType])(pc: PhaseContext) extends PhaseCheck{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//
//    val targetAlgoId = GlobalData.get.algoId
//    Node.walk(walkNodesDefautStack,node => {node.algoId = targetAlgoId})
//
//    for(c <- components){
//      def checkNameable(that : Any) : Unit = that match {
//        case area : Area => {
//          area.forEachNameables(obj => checkNameable(obj))
//        }
//        case data : Data =>  {
//          data.flatten.foreach(bt => {
//            if(bt.algoId != targetAlgoId && (!bt.isInstanceOf[BitVector] || bt.asInstanceOf[BitVector].inferredWidth != 0) && !bt.hasTag(unusedTag)){
//              prunedSignals += bt
//            }
//          })
//        }
//        case _ => {}
//      }
//
//      c.forEachNameables(obj => checkNameable(obj))
//    }
//    if(!prunedSignals.isEmpty){
//      SpinalWarning(s"${prunedSignals.size} signals were pruned. You can call printPruned on the backend report to get more informations.")
//    }
//
//
//    val targetAlgoId2 = GlobalData.get.allocateAlgoId()
//    def walkPruned(node : Node) : Unit = node.onEachInput(input => {
//      if(input != null && input.algoId != targetAlgoId2){
//        input.algoId = targetAlgoId2
//        walkPruned(input)
//      }
//    })
//
//    prunedSignals.foreach(source => walkPruned(source))
//    unusedSignals ++= (prunedSignals.filter(_.algoId != targetAlgoId2))
//  }
//}
//
////class PhaseCompletSwitchCases extends PhaseCheck{
////  override def useNodeConsumers = false
////  override def impl(pc : PhaseContext): Unit = {
////    import pc._
////
////    def walkAssignementTree(output : BaseType,node : Node): Unit = node match {
////      case node : MultipleAssignmentNode => node.onEachInput(input => walkAssignementTree(output,input))
////      case node : Reg => {
////        walkAssignementTree(output,node.dataInput)
////        walkAssignementTree(output,node.initialValue)
////      }
////      case node : WhenNode => {
////
////      }
////      case _ =>
////    }
////
////    Node.walk(walkNodesDefautStack,_ match{
////      case baseType : BaseType => {
////        walkAssignementTree(baseType,baseType.input)
////      }
////    })
////
////  }
////}
//
//class PhaseAddNodesIntoComponent(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    Node.walk({
//      val stack = walkNodesDefautStack
//      for (c <- components) {
//        c.nodes = ArrayBuffer[Node]()
//      }
//      stack
//    },node => {
//      node match{
//        case node : Node with Widthable =>{
//          if(node.getWidth > 0){
//            node.component.nodes += node
//          }
//        }
//        case _ => node.component.nodes += node
//      }
//
//    })
//  }
//}
//
//class PhaseOrderComponentsNodes(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    for (c <- components) {
//      c.nodes = c.nodes.sortWith(_.instanceCounter < _.instanceCounter)
//    }
//  }
//}
//
class PhaseAllocateNames(pc: PhaseContext) extends PhaseMisc{
  override def useNodeConsumers = false
  override def impl(pc : PhaseContext): Unit = {
    import pc._
//    for (enumDef <- enums.keys) {
//      if (enumDef.isWeak)
//        enumDef.setName(globalScope.allocateName(enumDef.getName()));
//      else
//        globalScope.iWantIt(enumDef.getName(),s"Reserved name ${enumDef.getName()} is not free for ${enumDef.toString()}")
//    }
//
//
//    for((enum,encodings) <- enums;
//         encodingsScope = new Scope();
//         encoding <- encodings){
//      if (encoding.isWeak)
//        encoding.setName(encodingsScope.allocateName(encoding.getName()));
//      else
//        encodingsScope.iWantIt(encoding.getName(),s"Reserved name ${encoding.getName()} is not free for ${encoding.toString()}")
//    }

    for (c <- sortedComponents) {
//      if (c.isInstanceOf[BlackBox])
//        globalScope.lockName(c.definitionName)
//      else
        c.definitionName = globalScope.allocateName(c.definitionName)
    }

    globalScope.lockScope()

    for (c <- sortedComponents) {
      c.allocateNames(pc.globalScope)
    }
  }
}
//
//class PhaseRemoveComponentThatNeedNoHdlEmit(pc: PhaseContext) extends PhaseNetlist{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//   components.foreach(c => {
//      if (c.nameables.size == 0) { //TODO IR speed
//        if (c.parent != null) c.parent.children -= c
//      }
//    })
//  }
//}
//
//class PhasePrintStates(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    var counter = 0
//    Node.walk(walkNodesDefautStack,_ => counter = counter + 1)
//    SpinalInfo(s"Graph has $counter nodes")
//  }
//}
//
class PhaseCreateComponent(gen : => Component)(pc: PhaseContext) extends PhaseNetlist{
  override def useNodeConsumers = false
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    val defaultClockDomain = ClockDomain.external("",frequency = config.defaultClockDomainFrequency)
    globalData.context.push(globalData.contextHead.copy(clockDomain = defaultClockDomain))
    pc.topLevel = gen
    globalData.context.pop()
    pc.checkGlobalData()
  }
}

class PhaseDummy(doThat : => Unit) extends PhaseMisc{
  override def useNodeConsumers = false
  override def impl(pc : PhaseContext): Unit = {
    doThat
  }
}

//class PhaseCompletSwitchCases extends PhaseNetlist{
//  override def useNodeConsumers = false
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//
//    object Exclusion{
//      def apply(size : BigInt) : Exclusion = {
//        if(size < 4096) new ExclusionByArray((size))
//        else new ExclusionByNothing(size)
//      }
//    }
//
//    abstract class Exclusion(val size : BigInt){
//      var remaining = size
//      def allocate(id : Int): Boolean
//    }
//
//    class ExclusionByNothing(size : BigInt) extends Exclusion(size){ //TODO better than nothing
//    override def allocate(id: Int): Boolean = true
//    }
//
//    class ExclusionByArray(size : BigInt) extends Exclusion(size){
//      val occupancy = new Array[Boolean](size.toInt)
//
//      def allocate(id : Int): Boolean ={
//        if(occupancy(id)) return false
//        occupancy(id) = true
//        remaining -= 1
//        return true
//      }
//    }
//
//    def walkAssignementTree(output : BaseType,node : Node,excludedIn : collection.immutable.Map[BaseType,Exclusion],previous : Node,previousId : Int): Unit = node match {
//      case node : MultipleAssignmentNode => node.onEachInput((input,id) => walkAssignementTree(output,input,excludedIn,node,id))
//      case node : WhenNode => {
//        def bypassBastType(that : Node) : Node = if(that.isInstanceOf[BaseType]) that.asInstanceOf[BaseType].input else that
//        walkAssignementTree(output,node.whenTrue,excludedIn,node,1)
//        var excludedOut = excludedIn
//
//        def checkAndApplyCond(bt : BaseType,value : Int): Unit = {
//          val hit = excludedOut(bt)
//          if (!hit.allocate(value)) {
//            PendingError(s"Condition duplication to drive the $output signal\n" + node.w.getScalaLocationLong)
//          }
//          if (hit.remaining == 0 && node.whenFalse == null) {
//            previous.setInput(previousId, node.whenTrue)
//          }
//        }
//        bypassBastType(node.cond) match {
//          case op : Operator.BitVector.Equal => {
//            val opRightByp = bypassBastType(op.right)
//            if (opRightByp.isInstanceOf[Literal]) {
//              op.left match {
//                case bt: BitVector => {
//                  val value = opRightByp match {
//                    case lit: SIntLiteral => lit.value + (BigInt(1) << (op.right.getWidth - 1))
//                    case lit: BitVectorLiteral => {
//                      lit.value
//                    }
//                    case lit: BitsAllToLiteral => {
//                      if (lit.value) (BigInt(1) << lit.getWidth) - 1 else BigInt(0)
//                    }
//                  }
//                  if (!excludedOut.contains(bt)) {
//                    excludedOut += (bt -> Exclusion(BigInt(1) << bt.getWidth))
//                  }
//
//                  checkAndApplyCond(bt,value.toInt)
//                }
//                case _ =>
//              }
//            }
//          }
//          case op : Operator.Enum.Equal => {
//            (op.left, bypassBastType(op.right)) match {
//              case (craft: SpinalEnumCraft[_], lit: EnumLiteral[_]) => {
//                val value = lit.enum.position
//                if (!excludedOut.contains(craft)) {
//                  excludedOut += (craft -> Exclusion(lit.enum.spinalEnum.elements.length))
//                }
//                checkAndApplyCond(craft,value.toInt)
//              }
//              case _ =>
//            }
//          }
//          case op : Operator.Bool.Equal => {
//            (op.left, bypassBastType(op.right)) match {
//              case (craft: Bool, lit: BoolLiteral) => {
//                val value = if(lit.value) 1 else 0
//                if (!excludedOut.contains(craft)) {
//                  excludedOut += (craft -> Exclusion(2))
//                }
//                checkAndApplyCond(craft,value)
//              }
//              case _ =>
//            }
//          }
//          case _ =>
//        }
//        walkAssignementTree(output,node.whenFalse,excludedOut,node,2)
//      }
//      case _ =>
//    }
//
//    Node.walk(walkNodesDefautStack,_ match{
//      case baseType : BaseType => {
//        baseType.input match{
//          case reg : Reg => {
//            walkAssignementTree(baseType,reg.dataInput   ,collection.immutable.Map[BaseType,Exclusion](),reg,RegS.getDataInputId)
//            walkAssignementTree(baseType,reg.initialValue,collection.immutable.Map[BaseType,Exclusion](),reg,RegS.getInitialValueId)
//          }
//          case _ => walkAssignementTree(baseType,baseType.input,collection.immutable.Map[BaseType,Exclusion](),baseType,0)
//        }
//      }
//      case _ =>
//    })
//  }
//}
//

object SpinalVhdlBoot{
  def apply[T <: Component](config : SpinalConfig)(gen : => T) : SpinalReport[T] ={
    try {
      singleShot(config)(gen)
    } catch {
      case e : NullPointerException => {
        println(
          """
            |ERROR !
            |A null pointer access has been detected in the JVM.
            |This could happen when in your SpinalHDL description, you access an signal which is only defined further.
            |For instance :
            |  val result = Bool
            |  result := a ^ b  //a and b can't be accessed there because they are only defined one line below (Software rule of execution order)
            |  val a,b = Bool
          """.stripMargin)
        System.out.flush()
        throw e
      }
      case e: Throwable => {
        if(!config.debug){
          println("\n**********************************************************************************************")
          val errCnt = SpinalError.getErrorCount()
          SpinalWarning(s"Elaboration failed (${errCnt} error" + (if(errCnt > 1){s"s"} else {s""}) + s").\n" +
            s"          Spinal will restart with scala trace to help you to find the problem.")
          println("**********************************************************************************************\n")
          System.out.flush()
          return singleShot(config.copy(debug = true))(gen)
        }else{
          println("\n**********************************************************************************************")
          val errCnt = SpinalError.getErrorCount()
          SpinalWarning(s"Elaboration failed (${errCnt} error" + (if(errCnt > 1){s"s"} else {s""}) + ").")
          println("**********************************************************************************************")
          System.out.flush()
          throw e
        }
      }
    }
  }

  def singleShot[T <: Component](config : SpinalConfig)(gen : => T): SpinalReport[T] ={
    val pc = new PhaseContext(config)
    pc.globalData.anonymSignalPrefix = if(config.anonymSignalPrefix == null) "zz" else config.anonymSignalPrefix
    val prunedSignals = mutable.Set[BaseType]()
    val unusedSignals = mutable.Set[BaseType]()


    SpinalProgress("Start elaboration")


    val phases = ArrayBuffer[Phase]()

    phases +=new PhaseDummy({
      var stack = 0
      GlobalData.get.algoId = 1
      for(i <- 0 until 1000000){
        stack += GlobalData.get.algoId
      }
      SpinalProgress("BENCH" + stack)
    })
    phases += new PhaseCreateComponent(gen)(pc)


    phases += new PhaseDummy(SpinalProgress("Get names from reflection"))
    phases += new PhaseNameNodesByReflection(pc)
//    phases += new PhaseCollectAndNameEnum(pc)

    phases += new PhaseDummy(SpinalProgress("Transform connections"))
    phases += new PhasePullClockDomains(pc)

//    phases += new PhaseDummy(SpinalProgress("Simplify graph's nodes"))
    phases += new PhaseDeleteUselessBaseTypes(pc)

    phases += new PhaseAllocateNames(pc)
//    phases += new PhaseRemoveComponentThatNeedNoHdlEmit(pc)

//    def initVhdlBase[T <: VhdlBase](base : T) = {
//      base.packageName     = pc.config.globalPrefix + base.packageName
//      base.enumPackageName = pc.config.globalPrefix + base.enumPackageName
//      base
//    }

   // phases += initVhdlBase(new PhaseVhdl2(pc))
    phases += new PhaseVhdl(pc)


 /*   if(config.keepAll) phases  += new PhaseKeepAll(pc)
    phases ++= config.transformationPhases
    phases ++= config.memBlackBoxers
    phases += new PhaseApplyIoDefault(pc)
    phases += new PhaseMoveLogicTags(pc)
    phases += new PhaseNodesBlackBoxGenerics(pc)

    phases += new PhaseDummy(SpinalProgress("Get names from reflection"))
    phases += new PhaseNameNodesByReflection(pc)
    phases += new PhaseCollectAndNameEnum(pc)

    phases += new PhaseDummy(SpinalProgress("Transform connections"))
    phases += new PhasePullClockDomains(pc)
    phases += new PhaseCheck_noNull_noCrossHierarchy_noInputRegister_noDirectionLessIo(pc)
    phases += new PhaseAddInOutBinding(pc)
    phases += new PhaseNameBinding(pc)
    phases += new PhaseAllowNodesToReadOutputs(pc)
    phases += new PhaseAllowNodesToReadInputOfKindComponent(pc)

    phases += new PhaseDummy(SpinalProgress("Infer nodes's bit width"))
    phases += new PhasePreInferationChecks(pc)
    phases += new PhaseInferEnumEncodings(pc,e => e)
    phases += new PhaseInferWidth(pc)
    phases += new PhaseSimplifyNodes(pc)
    phases += new PhaseInferWidth(pc)
    phases += new PhasePropagateBaseTypeWidth(pc)
    phases += new PhaseNormalizeNodeInputs(pc)
    phases += new PhaseResizeLiteralSimplify(pc)
    phases += new PhaseCheckInferredWidth(pc)

    phases += new PhaseDummy(SpinalProgress("Check combinatorial loops"))
    phases += new PhaseCheckCombinationalLoops(pc)
    phases += new PhaseDummy(SpinalProgress("Check cross clock domains"))
    phases += new PhaseCheckCrossClockDomains(pc)
    phases += new PhaseCheckMisc(pc)

    phases += new PhaseDummy(SpinalProgress("Simplify graph's nodes"))
    phases += new PhaseDontSymplifyBasetypeWithComplexAssignement(pc)
    phases += new PhaseDeleteUselessBaseTypes(pc)

    phases += new PhaseCompletSwitchCases

    phases += new PhaseDummy(SpinalProgress("Check that there is no incomplete assignment"))
    phases += new PhaseCheck_noAsyncNodeWithIncompleteAssignment(pc)
    phases += new PhaseSimplifyBlacBoxGenerics(pc)

    phases += new PhaseDummy(SpinalProgress("Collect signals not used in the graph"))
    phases += new PhasePrintUnUsedSignals(prunedSignals,unusedSignals)(pc)

    phases += new PhaseDummy(SpinalProgress("Finalise"))
    phases += new PhaseAddNodesIntoComponent(pc)
    phases += new PhaseOrderComponentsNodes(pc)
    phases += new PhaseAllocateNames(pc)
    phases += new PhaseRemoveComponentThatNeedNoHdlEmit(pc)

    phases += new PhasePrintStates(pc)


    def initVhdlBase[T <: VhdlBase](base : T) = {
      base.packageName     = pc.config.globalPrefix + base.packageName
      base.enumPackageName = pc.config.globalPrefix + base.enumPackageName
      base
    }

    phases += initVhdlBase(new PhaseVhdl(pc))
    phases += initVhdlBase(new VhdlTestBenchBackend(pc))

*/
//    for(inserter <-config.phasesInserters){
//      inserter(phases)
//    }


    for(phase <- phases){
      pc.doPhase(phase)
    }


    pc.checkGlobalData()


    //pc.checkNoZeroWidth() for debug


    val report = new SpinalReport[T](pc.topLevel.asInstanceOf[T])
    report.prunedSignals ++= prunedSignals
    report.unusedSignals ++= unusedSignals

    report
  }
}
//
//
//
//class PhaseDontSymplifySomeNodesVerilog(pc: PhaseContext) extends PhaseMisc{
//  override def useNodeConsumers = true
//  override def impl(pc : PhaseContext): Unit = {
//    def applyTo(that : Node): Unit ={
//      assert(that.consumers.size == 1)
//      that.consumers(0) match {
//        case consumer: BaseType => consumer.dontSimplifyIt()
//        case _ =>
//      }
//    }
//    import pc._
//    Node.walk(walkNodesDefautStack,node => {
//      node match {
//        case node: Resize  => applyTo(node)
//        case node: Extract => node.getBitVector.asInstanceOf[BitVector].dontSimplifyIt()
//        case node: Literal => applyTo(node)
////        case node: Cast    => applyTo(node)
//        case node: SInt    => if(!node.input.isInstanceOf[SInt]) node.dontSimplifyIt()
//        case node: UInt    => if(!node.input.isInstanceOf[UInt]) node.dontSimplifyIt()
////        case node: Bool    => node.dontSimplifyIt()
////        case node: Modifier if node.consumers.size == 1 && node.consumers(0).isInstanceOf[SInt] => applyTo(node) // .....
////        case node: Operator.BitVector.Add => applyTo(node)
////        case node: Operator.BitVector.Sub => applyTo(node)
//        case node: Operator.BitVector.ShiftOperator => applyTo(node)
////        case node: Operator.Bits.Cat => applyTo(node)
////        case node : Extract => applyTo(node)
//        case _ =>
//      }
//    })
////    import pc._
////    Node.walk(walkNodesDefautStack,node => node match{
////      case bt : BaseType => bt.dontSimplifyIt()
////      case _ =>
////    })
//  }
//}
//
//
//
//
//object SpinalVerilogBoot{
//  def apply[T <: Component](config : SpinalConfig)(gen : => T) : SpinalReport[T] ={
//    try {
//      singleShot(config)(gen)
//    } catch {
//      case e : NullPointerException => {
//        println(
//          """
//            |ERROR !
//            |A null pointer access has been detected in the JVM.
//            |This could happen when in your SpinalHDL description, you access an signal which is only defined further.
//            |For instance :
//            |  val result = Bool
//            |  result := a ^ b  //a and b can't be accessed there because they are only defined one line below (Software rule of execution order)
//            |  val a,b = Bool
//          """.stripMargin)
//        System.out.flush()
//        throw e
//      }
//      case e: Throwable => {
//        if(!config.debug){
//          println("\n**********************************************************************************************")
//          val errCnt = SpinalError.getErrorCount()
//          SpinalWarning(s"Elaboration failed (${errCnt} error" + (if(errCnt > 1){s"s"} else {s""}) + s").\n" +
//            s"          Spinal will restart with scala trace to help you to find the problem.")
//          println("**********************************************************************************************\n")
//          System.out.flush()
//          return singleShot(config.copy(debug = true))(gen)
//        }else{
//
//          println("\n**********************************************************************************************")
//          val errCnt = SpinalError.getErrorCount()
//          SpinalWarning(s"Elaboration failed (${errCnt} error" + (if(errCnt > 1){s"s"} else {s""}) + ").")
//          println("**********************************************************************************************")
//          System.out.flush()
//          throw e
//        }
//      }
//    }
//  }
//
//  def singleShot[T <: Component](config : SpinalConfig)(gen : => T): SpinalReport[T] ={
//    val pc = new PhaseContext(config)
//    pc.globalData.anonymSignalPrefix = if(config.anonymSignalPrefix == null) "" else config.anonymSignalPrefix
//
//    val prunedSignals = mutable.Set[BaseType]()
//    val unusedSignals = mutable.Set[BaseType]()
//
//    SpinalProgress("Start elaboration")
//
//
//    val phases = ArrayBuffer[Phase]()
//
//    phases += new PhaseCreateComponent(gen)(pc)
//    phases += new PhaseDummy(SpinalProgress("Start analysis and transform"))
//    if(config.keepAll) phases  += new PhaseKeepAll(pc)
//    phases ++= config.transformationPhases
//    phases ++= config.memBlackBoxers
//    phases += new PhaseApplyIoDefault(pc)
//    phases += new PhaseMoveLogicTags(pc)
//    phases += new PhaseNodesBlackBoxGenerics(pc)
//
//    phases += new PhaseDummy(SpinalProgress("Get names from reflection"))
//    phases += new PhaseNameNodesByReflection(pc)
//    phases += new PhaseCollectAndNameEnum(pc)
//
//    phases += new PhaseDummy(SpinalProgress("Transform connections"))
//    phases += new PhasePullClockDomains(pc)
//    phases += new PhaseCheck_noNull_noCrossHierarchy_noInputRegister_noDirectionLessIo(pc)
//    phases += new PhaseAddInOutBinding(pc)
//    phases += new PhaseNameBinding(pc)
//    phases += new PhaseAllowNodesToReadOutputs(pc)
//    phases += new PhaseAllowNodesToReadInputOfKindComponent(pc)
//
//    phases += new PhaseDummy(SpinalProgress("Infer nodes's bit width"))
//    phases += new PhasePreInferationChecks(pc)
//    phases += new PhaseInferEnumEncodings(pc,e => if(e == `native`) binarySequential else e)
//    phases += new PhaseInferWidth(pc)
//    phases += new PhaseSimplifyNodes(pc)
//    phases += new PhaseInferWidth(pc)
//    phases += new PhasePropagateBaseTypeWidth(pc)
//    phases += new PhaseNormalizeNodeInputs(pc)
//    phases += new PhaseResizeLiteralSimplify(pc)
//    phases += new PhaseCheckInferredWidth(pc)
//
//    phases += new PhaseDummy(SpinalProgress("Check combinatorial loops"))
//    phases += new PhaseCheckCombinationalLoops(pc)
//    phases += new PhaseDummy(SpinalProgress("Check cross clock domains"))
//    phases += new PhaseCheckCrossClockDomains(pc)
//    phases += new PhaseCheckMisc(pc)
//
//    phases += new PhaseDummy(SpinalProgress("Simplify graph's nodes"))
//    phases += new PhaseDontSymplifyBasetypeWithComplexAssignement(pc)
//    phases += new PhaseDontSymplifySomeNodesVerilog(pc)    //VERILOG
//    phases += new PhaseDeleteUselessBaseTypes(pc)
//
//    phases += new PhaseCompletSwitchCases
//
//    phases += new PhaseDummy(SpinalProgress("Check that there is no incomplete assignment"))
//    phases += new PhaseCheck_noAsyncNodeWithIncompleteAssignment(pc)
//    phases += new PhaseSimplifyBlacBoxGenerics(pc)
//
//    phases += new PhaseDummy(SpinalProgress("Collect signals not used in the graph"))
//    phases += new PhasePrintUnUsedSignals(prunedSignals,unusedSignals)(pc)
//
//    phases += new PhaseDummy(SpinalProgress("Finalise"))
//    phases += new PhaseAddNodesIntoComponent(pc)
//    phases += new PhaseOrderComponentsNodes(pc)
//    phases += new PhaseAllocateNames(pc)
//    phases += new PhaseRemoveComponentThatNeedNoHdlEmit(pc)
//
//    phases += new PhasePrintStates(pc)
//    phases += new PhaseVerilog(pc)
//
//
//    for(inserter <-config.phasesInserters){
//      inserter(phases)
//    }
//
//    for(phase <- phases){
//      pc.doPhase(phase)
//    }
//
//
//    pc.checkGlobalData()
//
//    val report = new SpinalReport[T](pc.topLevel.asInstanceOf[T])
//    report.prunedSignals ++= prunedSignals
//    report.unusedSignals ++= unusedSignals
//
//    report
//  }
//}
