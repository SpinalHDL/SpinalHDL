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
  val globalScope = new NamingScope()
  var topLevel: Component = null
  val enums = mutable.Map[SpinalEnum,mutable.Set[SpinalEnumEncoding]]()
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

  def walkStatements(func : Statement => Unit): Unit ={
    GraphUtils.walkAllComponents(topLevel, c => c.dslBody.walkStatements(func))
  }

  def walkExpression(func : Expression => Unit): Unit ={
    GraphUtils.walkAllComponents(topLevel, c => c.dslBody.walkStatements(s => s.walkExpression(func)))
  }


  def walkDeclarations(func : DeclarationStatement => Unit) : Unit = {
    walkComponents(c => c.dslBody.walkDeclarations(e => func(e)))
  }


  def walkRemapExpressions(func : Expression => Expression): Unit ={
    GraphUtils.walkAllComponents(topLevel, c => c.dslBody.walkStatements(s => s.walkRemapExpressions(func)))
  }

  def walkDrivingExpression(func : Expression => Unit): Unit ={
    GraphUtils.walkAllComponents(topLevel, c => c.dslBody.walkStatements(s => s.walkDrivingExpressions(func)))
  }

  def walkComponents(func : Component => Unit): Unit ={
    GraphUtils.walkAllComponents(topLevel, c => func(c))
  }




  def checkGlobalData() : Unit = {
    if (!GlobalData.get.context.isEmpty) SpinalError("context stack is not empty :(")
  }

  def checkPendingErrors() = if(!globalData.pendingErrors.isEmpty) SpinalError()



  def doPhase(phase: Phase): Unit ={
    phase.impl(this)
    checkPendingErrors()
  }

}

trait Phase{
  def impl(pc: PhaseContext): Unit
  def hasNetlistImpact : Boolean
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




class PhaseApplyIoDefault(pc: PhaseContext) extends PhaseNetlist{
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    walkDeclarations(e => {
      e match{
        case node : BaseType if node.dlcIsEmpty => node.getTag(classOf[DefaultTag]) match {
          case Some(defaultValue) => {
            val c = node.dir match {
              case `in` => node.component
              case `out` => if(node.component.parent != null)
                node.component.parent
              else
                null
              case _ => node.component
            }
            if(c != null) {
              node.dir match{
                case `in` =>  {
                  Component.push(c.parent)
                  node.assignFrom(defaultValue.that)
                  Component.pop(c.parent)
                }
                case _ => {
                  Component.push(c)
                  node.assignFrom(defaultValue.that)
                  Component.pop(c)
                }
              }
            }
          }
          case _ =>
        }
        case _ =>
      }
    })
  }
}

class MemTopology(val mem: Mem[_]) {
//  val writes = ArrayBuffer[MemWrite]()
//  val readsAsync = ArrayBuffer[MemReadAsync]()
//  val readsSync = ArrayBuffer[MemReadSync]()
//  val writeReadSameAddressSync = ArrayBuffer[(MemWrite, MemReadSync)]() //DISABLED
//  val readWriteSync = ArrayBuffer[(MemReadWrite_writePart, MemReadWrite_readPart)]()
}
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
        case bb: BlackBox => {
          bb.getGeneric.genNames
        }
        case _ =>
      }
    }
  }
}

class PhaseCollectAndNameEnum(pc: PhaseContext) extends PhaseMisc{
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    walkDeclarations(e => e match{
      case enum: SpinalEnumCraft[_] => enums.getOrElseUpdate(enum.spinalEnum,null) //Encodings will be added later
      case _ =>
    })

    val scope = pc.globalScope.newChild
    enums.keys.foreach(e => {
      val name = if(e.isNamed)
        e.getName()
      else
        e.getClass.getSimpleName.replace("$","")

      e.setName(scope.allocateName(name))
    })

    for (enumDef <- enums.keys) {
      Misc.reflect(enumDef, (name, obj) => {
        obj match {
          case obj: Nameable => obj.setWeakName(scope.getUnusedName(name))
          case _ =>
        }
      })
      for (e <- enumDef.elements) {
        if (e.isUnnamed) {
          e.setWeakName(scope.getUnusedName("e" + e.position))
        }
      }
    }
  }
}

//TODO IR pull ram clocks
class PhasePullClockDomains(pc: PhaseContext) extends PhaseNetlist{
  override def impl(pc : PhaseContext): Unit = {
    import pc._

    walkComponents(c => {
      val cds = mutable.HashSet[ClockDomain]()
      c.dslBody.walkLeafStatements{
        case bt : BaseType if bt.isReg => {
          val cd = bt.clockDomain
          if(bt.isUsingResetSignal && (!cd.hasResetSignal && !cd.hasSoftResetSignal))
            SpinalError(s"Clock domain without reset contain a register which needs one\n ${bt.getScalaLocationLong}")

          cds += cd
        }
        case ls => ls.foreachClockDomain(cd => cds += cd)
      }

      c.rework{
        for(cd <- cds){
          cd.readClockWire
          if(cd.hasResetSignal) cd.readResetWire
          if(cd.hasSoftResetSignal) cd.readSoftResetWire
          if(cd.hasClockEnableSignal) cd.readClockEnableWire
        }
      }
    })
  }
}



//TODO IR infer with on IO (even if they aren't assigned on top level)
class PhaseInferWidth(pc: PhaseContext) extends PhaseMisc{
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    globalData.nodeAreInferringWidth = true




    var iterationCounter = 0
    while (true) {
      var somethingChange = false

      //Infer width on all expressions
      walkExpression(e => e match {
        case e : DeclarationStatement =>
        case e : Widthable =>
          val hasChange = e.inferWidth
          somethingChange = somethingChange || hasChange
        case _ =>
      })

      //Infer width on all nameable expression (BitVector)
      walkDeclarations(e => e match{
        case e : Widthable =>
          val hasChange = e.inferWidth
          somethingChange = somethingChange || hasChange
        case _ =>
      })

      //Check in the width inferation is done, then check it and generate errors
      if (!somethingChange || iterationCounter == 10000) {
        val errors = mutable.ArrayBuffer[String]()

        def widthableCheck(e : Widthable) : Unit = {
          if (e.inferWidth) {
            //Don't care about Reg width inference
            errors += s"Can't infer width on ${e.getScalaLocationLong}"
          }
          if (e.widthWhenNotInferred != -1 &&
            e.widthWhenNotInferred != e.getWidth) {
            errors += s"getWidth call result during elaboration differ from inferred width on\n${e.getScalaLocationLong}"
          }

          if(e.inferredWidth < -1){
            errors += s"Negative width on $e at ${e.getScalaLocationLong}"
          }
        }

        walkExpression(e => e match {
          case e : DeclarationStatement =>
          case e : Widthable => widthableCheck(e)
          case e : WidthProvider => if(e.getWidth < 0){
            errors += s"Negative width on $e at ${e.getScalaLocationLong}"
          }
          case _ =>
        })

        walkDeclarations(e => e match{
          case e : Widthable => widthableCheck(e)
          case _ =>
        })

        if (errors.nonEmpty)
          SpinalError(errors)
        return
      }
      iterationCounter += 1
    }
  }
}


class PhaseInferEnumEncodings(pc: PhaseContext,encodingSwap : (SpinalEnumEncoding) => SpinalEnumEncoding) extends PhaseMisc{
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    globalData.nodeAreInferringEnumEncoding = true
    val nodes = ArrayBuffer[Expression with EnumEncoded]()
    val nodesInferrable = ArrayBuffer[Expression with InferableEnumEncoding]()
    val consumers = mutable.HashMap[Expression , ArrayBuffer[Expression]]()
    var algo = globalData.allocateAlgoIncrementale()

    def walkExpression(node : Expression): Unit ={
      if(node.algoIncrementale != algo) {
        node.algoIncrementale = algo
        if (node.isInstanceOf[EnumEncoded]) nodes += node.asInstanceOf[Expression with EnumEncoded]
        if (node.isInstanceOf[InferableEnumEncoding]) nodesInferrable += node.asInstanceOf[Expression with InferableEnumEncoding]
        node.foreachDrivingExpression(input => input match {
          case input: Expression with EnumEncoded => consumers.getOrElseUpdate(input, ArrayBuffer[Expression]()) += node
          case _ =>
        })
        node.foreachDrivingExpression(input => walkExpression(input))
      }
    }



    walkStatements(s => s match {
      case s : AssignmentStatement =>
        val finalTarget = s.finalTarget
        s.source match {
          case source : Expression with EnumEncoded => consumers.getOrElseUpdate(source,ArrayBuffer[Expression]()) += finalTarget
          case _ =>
        }
        s.foreachDrivingExpression(e => walkExpression(e))
      case _ =>
        s.foreachDrivingExpression(e => walkExpression(e))
    })

    //???
    walkDeclarations{case e : Expression => walkExpression(e) case _ => }

    nodesInferrable.foreach(node => {
      node.bootInferration()
    })

    nodes.foreach(enum => {
      enum.swapEncoding(encodingSwap(enum.getEncoding))
    })

    algo = globalData.allocateAlgoIncrementale()
    nodes.foreach(enum => {
      if(enum.propagateEncoding){
        def propagateOn(that : Expression): Unit = {
          that match {
            case that : InferableEnumEncoding => {
              if(that.algoIncrementale == algo) return
              that.algoIncrementale = algo
              if(that.encodingProposal(enum.getEncoding)) {
                that match {
                  case that : SpinalEnumCraft[_] =>
                    that.dlcForeach(s => propagateOn(s.source))
                    consumers.getOrElse(that, Nil).foreach(propagateOn(_))
                  case _ =>
                    that.foreachExpression(propagateOn(_))
                    consumers.getOrElse(that, Nil).foreach(propagateOn(_))
                }
              }
            }
            case _ =>
          }
        }
        enum match {
          case enum : SpinalEnumCraft[_] =>
            enum.dlcForeach(s => propagateOn(s.source))
            consumers.getOrElse(enum, Nil).foreach(propagateOn(_))
          case _ =>
            enum.foreachExpression(propagateOn(_))
            consumers.getOrElse(enum, Nil).foreach(propagateOn(_))
        }
      }
    })

    //Feed enums with encodings
    enums.keySet.foreach(enums(_) = mutable.Set[SpinalEnumEncoding]())
    nodes.foreach(enum => {
      enums(enum.getDefinition) += enum.getEncoding
    })


    //give a name to unamed encodings
    val unamedEncodings = enums.valuesIterator.flatten.toSet.withFilter(_.isUnnamed).foreach(_.setWeakName("anonymousEnc"))

    //Check that there is no encoding overlaping
    for((enum,encodings) <- enums){
      for(encoding <- encodings) {
        val reserveds = mutable.Map[BigInt, ArrayBuffer[SpinalEnumElement[_]]]()
        for(element <- enum.elements){
          val key = encoding.getValue(element)
          reserveds.getOrElseUpdate(key,ArrayBuffer[SpinalEnumElement[_]]()) += element
        }
        for((key,elements) <- reserveds){
          if(elements.length != 1){
            PendingError(s"Conflict in the $enum enumeration with the '$encoding' encoding with the key $key' and following elements:.\n${elements.mkString(", ")}\n\nEnumeration defined at :\n${enum.getScalaLocationLong}Encoding defined at :\n${encoding.getScalaLocationLong}")
          }
        }
      }
    }
  }
}


class PhaseSimplifyNodes(pc: PhaseContext) extends PhaseNetlist{
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    val toRemove = mutable.ArrayBuffer[Statement]()
    walkStatements{
      case s : BitVector if s.getWidth == 0 => {
        s.foreachStatements(toRemove += _)
        s.removeStatement()
      }
      case s => s.walkRemapExpressions(_.simplifyNode)
    }

    toRemove.foreach(_.removeStatement())
  }
}

class PhaseNormalizeNodeInputs(pc: PhaseContext) extends PhaseNetlist{
  override def impl(pc : PhaseContext): Unit = {
    import pc._

    walkStatements(s => {
      s.walkExpression(e => {
        e.normalizeInputs
      })
      s.normalizeInputs
    })

    walkComponents(c => {
      c.dslBody.walkDeclarations(n => n match {
        case n : BitVector => n.removeTag(tagAutoResize)
        case _ =>
      })
    })
  }
}

class PhaseCheckCombinationalLoops() extends PhaseCheck{
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    val walkingId = GlobalData.get.allocateAlgoIncrementale()
    val okId = GlobalData.get.allocateAlgoIncrementale()

    def walk(path : List[(BaseNode)],
             node: BaseNode): Unit = {
      val newPath = node :: path
      if (node.algoIncrementale == walkingId) {
        val ordred = newPath.reverseIterator
        val filtred = ordred.dropWhile((e) => (e != node)).drop(1).toArray
        if (!filtred.exists(e => e.isInstanceOf[SpinalTagReady] && e.asInstanceOf[SpinalTagReady].hasTag(noCombinatorialLoopCheck))) {
          val wellNameLoop = new StringBuilder()
          for(n <- filtred.reverseIterator) n match{
            case n : DeclarationStatement => wellNameLoop ++= s"      >>> ${n.toString()} at ${n.getScalaLocationShort} >>>\n"
            case _ =>
          }
          val multiLineLoop = filtred.reverseIterator.map(n => "      " + n.toString).foldLeft("")(_ + "\n" + _)
          PendingError(s"  Combinatorial loop !\n      Partial chain :\n${wellNameLoop}\n      Full chain :\n${multiLineLoop}")
        }
      }else if (node.algoIncrementale != okId) {
        node.algoIncrementale = walkingId
        node match {
          case node: BaseType => {
            if(node.isComb) {
              node.algoIncrementale = walkingId
              node.foreachStatements(s => walk(newPath, s))
              node.algoIncrementale = okId
            }
          }
          case node: AssignmentStatement => {
            node.foreachDrivingExpression(e => walk(newPath, e))
            node.walkParentTreeStatementsUntilRootScope(s => walk(newPath, s))
          }
          case node: TreeStatement => {
            if (node.algoIncrementale != okId) {
              node.foreachDrivingExpression(e => walk(newPath, e))
              node.algoIncrementale = okId
            }
          }
          case node: Mem[_] =>
          case node: MemReadSync =>
          case node: MemReadWrite =>
          case node: MemWrite =>
          case node: Expression => {
            node.foreachDrivingExpression(e => walk(newPath, e))
          }
          case node : AssertStatement => {
            node.foreachDrivingExpression(e => walk(newPath, e))
          }
        }
      }
      node.algoIncrementale = okId
    }

    walkStatements(s => {
      if (s.algoIncrementale != okId) {
        walk(s :: Nil, s)
      }
    })
  }
}


class PhaseCheckCrossClock() extends PhaseCheck{
  override def impl(pc : PhaseContext): Unit = {
    import pc._


    walkStatements(s => {
      var walked = 0

      def walk(node: BaseNode, path : List[(BaseNode)], clockDomain: ClockDomain): Unit = {
        if(node.algoIncrementale == walked) return
        node.algoIncrementale = walked
        val newPath = node :: path

        def issue(syncDriver: BaseNode with ScalaLocated, otherClockDomain : ClockDomain): Unit ={
            val wellNameLoop = new StringBuilder()
            for(n <- newPath) n match{
              case n : DeclarationStatement => wellNameLoop ++= s"      >>> ${n.toString()} at ${n.getScalaLocationShort} >>>\n"
              case _ =>
            }
            val multiLineLoop = newPath.map(n => "      " + n.toString).foldLeft("")(_ + "\n" + _)
            PendingError(
              s"Synchronous element ${s} is driven " +
              s"by ${syncDriver} but they don't have the same clock domain. " +
              s"Register declaration at \n${s.getScalaLocationLong} through\n${wellNameLoop}"
            )
        }

        node match {
          case node : SpinalTagReady if node.hasTag(crossClockDomain) =>
          case node: BaseType => {
            if (node.isReg) {
              if(!node.clockDomain.isSyncronousWith(clockDomain)) {
                issue(node, node.clockDomain)
              }
            } else {
              node.foreachStatements(s => walk(s, newPath, clockDomain))
            }
          }
          case node : AssignmentStatement => {
            node.foreachDrivingExpression(e => walk(e, newPath, clockDomain))
            node.walkParentTreeStatementsUntilRootScope(s => walk(s, newPath, clockDomain))
          }
          case node : TreeStatement => {
            node.foreachDrivingExpression(e => walk(e, newPath, clockDomain))
          }
          case node : Mem[_] =>
          case node : MemReadSync => {
            if(!node.clockDomain.isSyncronousWith(clockDomain)) {
              issue(node, node.clockDomain)
            }
          }
          case node : MemReadWrite =>
            if(!node.clockDomain.isSyncronousWith(clockDomain)) {
              issue(node, node.clockDomain)
            }
          case node: Expression => {
            node.foreachDrivingExpression(e => walk(e, newPath, clockDomain))
          }
        }
      }
      s match {
        case s: BaseType if (s.isReg && !s.hasTag(crossClockDomain)) => {
          walked = GlobalData.get.allocateAlgoIncrementale()
          s.foreachStatements(as => walk(as, as :: s :: Nil, s.clockDomain))
        }
        case s: MemReadWrite if (!s.hasTag(crossClockDomain)) => {
          walked = GlobalData.get.allocateAlgoIncrementale()
          s.foreachDrivingExpression(as => walk(as, as :: s :: Nil, s.clockDomain))
        }
        case s: MemWrite if (!s.hasTag(crossClockDomain)) => {
          walked = GlobalData.get.allocateAlgoIncrementale()
          s.foreachDrivingExpression(as => walk(as, as :: s :: Nil, s.clockDomain))
        }
        case _ =>
      }
    })
  }
}

class PhaseRemoveUselessStuff(postClockPulling : Boolean, tagVitals : Boolean) extends PhaseNetlist{
  override def impl(pc: PhaseContext): Unit = {
    import pc._

    val okId = globalData.allocateAlgoIncrementale()

    def propagate(root: Statement, vital : Boolean): Unit = {
      if(root.algoIncrementale == okId) return
      root.algoIncrementale = okId
      val pending = mutable.ArrayStack[Statement](root)
      def propagate(s : Statement) = {
        if(s.algoIncrementale != okId) {
          s.algoIncrementale = okId
          pending.push(s)
        }
      }

      while(pending.nonEmpty){
        val s = pending.pop()
        if(postClockPulling) {
          s.foreachClockDomain(cd => {
            propagate(s.component.pulledDataCache(cd.clock).asInstanceOf[Bool])
            if(cd.hasResetSignal) propagate(s.component.pulledDataCache(cd.reset).asInstanceOf[Bool])
            if(cd.hasSoftResetSignal) propagate(s.component.pulledDataCache(cd.softReset).asInstanceOf[Bool])
            if(cd.hasClockEnableSignal) propagate(s.component.pulledDataCache(cd.clockEnable).asInstanceOf[Bool])
          })
        } else {
          s.foreachClockDomain(cd => {
            propagate(cd.clock)
            if(cd.hasResetSignal) propagate(cd.reset)
            if(cd.hasSoftResetSignal) propagate(cd.softReset)
            if(cd.hasClockEnableSignal) propagate(cd.clockEnable)
          })
        }

        s match {
          case s: BaseType => {
            if(vital)
              s.setAsVital()
            s.foreachStatements(propagate)
          }
          case s: AssignmentStatement => {
            s.walkExpression{ case e : Statement => propagate(e) case _ => }
            s.walkParentTreeStatements(propagate) //Could be walkParentTreeStatementsUntilRootScope but then should symplify removed TreeStatements
          }
          case s : WhenStatement => {
            s.walkExpression{ case e : Statement => propagate(e) case _ => }
          }
          case s : SwitchStatement => {
            s.walkExpression{ case e : Statement => propagate(e) case _ => }
          }
          case s : AssertStatement => {
            s.walkExpression{ case e : Statement => propagate(e) case _ => }
          }
          case s : Mem[_] => s.foreachStatements{
            case p : MemWrite => propagate(p)
            case p : MemReadWrite => propagate(p)
            case p : MemReadSync =>
            case p : MemReadAsync =>
          }
          case s : MemWrite => {
            s.walkExpression{ case e : Statement => propagate(e) case _ => }
          }
          case s : MemReadWrite => {
            s.walkExpression{ case e : Statement => propagate(e) case _ => }
          }
          case s : MemReadSync => {
            s.walkExpression{ case e : Statement => propagate(e) case _ => }
          }
          case s : MemReadAsync => {
            s.walkExpression{ case e : Statement => propagate(e) case _ => }
          }
        }
      }
    }

    //Propagate all vital signals (drive toplevel output and blackboxes inputs)
    topLevel.getAllIo.withFilter(_.isOutput).foreach(propagate(_, tagVitals))
    walkComponents{
      case c : BlackBox => c.getAllIo.withFilter(_.isInput).foreach(propagate(_, tagVitals))
      case c =>
    }

    walkStatements{
      case s : DeclarationStatement => if(s.isNamed) propagate(s, false)
      case s : AssertStatement => propagate(s, false)
      case s : TreeStatement =>
      case s : AssignmentStatement =>
      case s : MemWrite =>
      case s : MemReadWrite =>
      case s : MemReadSync =>
      case s : MemReadAsync =>
    }

    walkStatements(s => {
      if(s.algoIncrementale != okId){
        s.removeStatement()
      }
    })
  }
}


class PhaseRemoveIntermediateUnameds(onlyTypeNode : Boolean) extends PhaseNetlist{
  override def impl(pc: PhaseContext): Unit = {
    import pc._
    val koId = globalData.allocateAlgoIncrementale()

    walkDeclarations(e => e.algoInt = 0)

    //Count the number of driving reference done on each ref.source
    walkDrivingExpression(e => e match {
      case ref : DeclarationStatement => {
        ref.algoInt += 1
      }
      case _ =>
    })


    walkStatements(s => if(s.algoIncrementale != koId){
      s.walkRemapDrivingExpressions(e => e match {
        case ref : BaseType => {
          if(ref.algoInt == 1 && ref.isComb && ref.isDirectionLess && (!onlyTypeNode || ref.isTypeNode) && ref.canSymplifyIt && ref.hasOnlyOneStatement && Statement.isSomethingToFullStatement(ref.head) /*&& ref != excepted*/){ //TODO IR keep it
            ref.algoInt = 0
            val head = ref.head
            ref.algoIncrementale = koId
            head.algoIncrementale = koId
            head.source
          } else {
            ref
          }
        }
        case e => e
      })
    })


    walkStatements{
      case s if s.algoIncrementale == koId =>
        s.removeStatement()
      case s =>
    }
  }
}

class PhaseCheckIoBundle extends PhaseCheck{
  override def impl(pc: PhaseContext): Unit = {
    import pc._
    walkComponents(c => {
      try{
        val io = c.reflectIo
        for(bt <- io.flatten){
          if(bt.isDirectionLess){
           PendingError(s"IO BUNDLE ERROR : A direction less $bt signal was defined into $c component's io bundle\n${bt.getScalaLocationLong}")
          }
        }
      }catch{
        case _ : Throwable =>
      }
    })
  }

}
class PhaseCheckHiearchy extends PhaseCheck{
  override def impl(pc: PhaseContext): Unit = {
    import pc._

    //Check hierarchy read/write violation
    walkComponents(c => {
      c.dslBody.walkStatements(s => {
        var error = false
        s match {
          case s : AssignmentStatement => {
            val bt = s.finalTarget
            if (!(bt.isDirectionLess && bt.component == c) && !(bt.isOutput && bt.component == c) && !(bt.isInput && bt.component.parent == c)) {
              PendingError(s"HIERARCHY VIOLATION : $bt is drived by the $s statement, but isn't accessible in the $c component.\n${s.getScalaLocationLong}")
              error = true
            }

            if(!error){
              val rootScope = s.rootScopeStatement
              var ptr = s.parentScope

              while(ptr.parentStatement != null && ptr != rootScope){
                ptr = ptr.parentStatement.parentScope
              }

              if(ptr != rootScope){
                PendingError(s"SCOPE VIOLATION : $bt is assigned outside its declaration scope at \n${s.getScalaLocationLong}")
              }
            }
          }
          case _ =>
        }
        if(!error) s.walkExpression(e => e match{
          case bt : BaseType => {
            if(!(bt.component == c) && !(bt.isInput && bt.component.parent == c) && !(bt.isOutput && bt.component.parent == c)){
              PendingError(s"HIERARCHY VIOLATION : $bt is used to drive the $s statement, but isn't readable in the $c component\n${s.getScalaLocationLong}")
            }
          }
          case _ =>
        })
      })

      //Check register defined as component inputs
      c.getAllIo.foreach(bt => if(bt.isInput && bt.isReg){
        PendingError(s"REGISTER DEFINED AS COMPONENT INPUT : $bt is defined as a registered input of the $c component, but isn't allowed.\n${bt.getScalaLocationLong}")
      })
    })
  }
}

class PhaseCheck_noLatchNoOverride(pc: PhaseContext) extends PhaseCheck{
  override def impl(pc : PhaseContext): Unit = {
    import pc._

    walkComponents(c => {
      def walkBody(body : ScopeStatement) : mutable.HashMap[BaseType, AssignedBits] = {
        val assigneds = mutable.HashMap[BaseType, AssignedBits]()
        def getOrEmpty(bt : BaseType) = assigneds.getOrElseUpdate(bt, new AssignedBits(bt.getBitsWidth))
        def getOrEmptyAdd(bt : BaseType, src : AssignedBits): Boolean = {
          val dst = getOrEmpty(bt)
          val ret = src.isFull && !dst.isEmpty && !bt.hasTag(allowAssignmentOverride)
          dst.add(src)
          ret
        }
        def getOrEmptyAdd2(bt : BaseType, src : AssignedRange): Boolean = {
          val dst = getOrEmpty(bt)
          val ret = src.hi == dst.width-1 && src.lo == 0 && !dst.isEmpty  && !bt.hasTag(allowAssignmentOverride)
          dst.add(src)
          ret
        }
        def getOrEmptyAdd3(bt : BaseType, hi : Int, lo : Int): Boolean = {
          val dst = getOrEmpty(bt)
          val ret = hi == dst.width-1 && lo == 0 && !dst.isEmpty  && !bt.hasTag(allowAssignmentOverride)
          dst.add(hi, lo)
          ret
        }
        body.foreachStatements {
          case s: DataAssignmentStatement => { //Omit InitAssignmentStatement
            s.target match {
              case bt : BaseType => if(getOrEmptyAdd3(bt, bt.getBitsWidth - 1, 0)){
                PendingError(s"The previous assignments of $bt are fully overridden at \n${s.getScalaLocationLong}")
              }
              case e : BitVectorAssignmentExpression => {
                val bt = e.finalTarget
                if(getOrEmptyAdd2(bt,e.getAssignedBits)){
                  PendingError(s"The previous assignments of $bt are fully overridden at \n${s.getScalaLocationLong}")
                }
              }
            }
          }
          case s: WhenStatement => {
            val whenTrue = walkBody(s.whenTrue)
            val whenFalse = walkBody(s.whenFalse)
            for ((bt, assigned) <- whenTrue) {
              whenFalse.get(bt) match {
                case Some(otherBt) => if(getOrEmptyAdd(bt,assigned.intersect(otherBt))){
                  PendingError(s"The previous assignments of $bt are fully overridden inside the when statement at \n ${s.getScalaLocationLong}")
                }
                case None =>
              }
            }
          }
          case s: SwitchStatement => {
            val stuffs = if(s.isFullyCoveredWithoutDefault){
              s.elements.map(e => walkBody(e.scopeStatement))
            } else if(s.defaultScope != null){
              s.elements.map(e => walkBody(e.scopeStatement)) += walkBody(s.defaultScope)
            } else {
              null
            }
            if(stuffs != null) {
              val head = stuffs.head
              for (tailStuff <- stuffs.tail) {
                for ((bt, assigned) <- head) {
                  tailStuff.get(bt) match {
                    case Some(otherBt) => assigned.intersect(otherBt)
                    case None => assigned.clear()
                  }
                }
              }

              for ((bt, assigned) <- head) {
                if(getOrEmptyAdd(bt,assigned)){
                  PendingError(s"The previous assignments of $bt are fully overridden inside the switch statement at \n ${s.getScalaLocationLong}")
                }
              }
            }
          }
          case signal : BaseType if !(signal.component.isInBlackBoxTree && !signal.isInput) && !(signal.component.parent == null && signal.isInput)  => {
            getOrEmpty(signal)
          }
          case s =>
         }

        for((bt, assignedBits) <- assigneds if bt.isComb && (bt.isVital || !bt.dlcIsEmpty) && bt.rootScopeStatement == body && !assignedBits.isFull){
          val unassignedBits = new AssignedBits(bt.getBitsWidth)
          unassignedBits.add(bt.getBitsWidth-1, 0)
          unassignedBits.remove(assignedBits)
          if (!unassignedBits.isEmpty) {
            if(unassignedBits.isFull)
              PendingError(s"Combinatorial signal $bt has no default value => LATCH, defined at\n${bt.getScalaLocationLong}")
            else
              PendingError(s"Incomplete assignment is detected on the combinatorial signal $bt, unassigned bit mask " +
                s"is ${unassignedBits.toBinaryString}, declared at\n${bt.getScalaLocationLong}")
          }
        }

        assigneds
      }
      walkBody(c.dslBody)
    })
  }
}

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
class PhaseAllocateNames(pc: PhaseContext) extends PhaseMisc{
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    for (enumDef <- enums.keys) {
      if (enumDef.isWeak)
        enumDef.setName(globalScope.allocateName(enumDef.getName()));
      else
        globalScope.iWantIt(enumDef.getName(),s"Reserved name ${enumDef.getName()} is not free for ${enumDef.toString()}")
    }


    for((enum,encodings) <- enums;
       encodingsScope = new NamingScope();
       encoding <- encodings){
      if (encoding.isWeak)
        encoding.setName(encodingsScope.allocateName(encoding.getName()));
      else
        encodingsScope.iWantIt(encoding.getName(),s"Reserved name ${encoding.getName()} is not free for ${encoding.toString()}")
    }

    for (c <- sortedComponents) {
      if (c.isInstanceOf[BlackBox])
        globalScope.lockName(c.definitionName)
      else
        c.definitionName = globalScope.allocateName(c.definitionName)
    }

    globalScope.lockScope()

    for (c <- sortedComponents) {
      c.allocateNames(pc.globalScope)
    }
  }
}

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
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    val defaultClockDomain = ClockDomain.external("",frequency = config.defaultClockDomainFrequency)
    globalData.context.push(globalData.contextHead.copy(clockDomain = defaultClockDomain))
    binaryOneHot //Avoid unconstructable during phase
    pc.topLevel = gen
    globalData.context.pop()
    pc.checkGlobalData()
  }
}

class PhaseDummy(doThat : => Unit) extends PhaseMisc{
  override def impl(pc : PhaseContext): Unit = {
    doThat
  }
}


object SpinalVhdlBoot{
  def apply[T <: Component](config : SpinalConfig)(gen : => T) : SpinalReport[T] ={
    try {
      singleShot(config)(gen)
    } catch {
      case e : NullPointerException if config.debug=> {
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

    phases += new PhaseCreateComponent(gen)(pc)
    phases += new PhaseApplyIoDefault(pc)


    phases += new PhaseDummy(SpinalProgress("Get names from reflection"))
    phases += new PhaseNameNodesByReflection(pc)
    phases += new PhaseCollectAndNameEnum(pc)

    phases += new PhaseDummy(SpinalProgress("Transform connections"))

    phases += new PhaseCheckIoBundle()
    phases += new PhaseCheckHiearchy()
    phases += new PhaseRemoveUselessStuff(false, false)
    phases += new PhaseRemoveIntermediateUnameds(true)


    phases += new PhasePullClockDomains(pc)



    phases += new PhaseDummy(SpinalProgress("Infer nodes's bit width"))
    phases += new PhaseInferEnumEncodings(pc,e => e)
    phases += new PhaseInferWidth(pc)
    phases += new PhaseNormalizeNodeInputs(pc)
    phases += new PhaseSimplifyNodes(pc)


    phases += new PhaseRemoveUselessStuff(true, true)
    phases += new PhaseRemoveIntermediateUnameds(false)

    phases += new PhaseCheck_noLatchNoOverride(pc)
    phases += new PhaseCheckCombinationalLoops()
    phases += new PhaseCheckCrossClock()

    phases += new PhaseAllocateNames(pc)

    def initVhdlBase[T <: VhdlBase](base : T) = {
      base.packageName     = pc.config.globalPrefix + base.packageName
      base.enumPackageName = pc.config.globalPrefix + base.enumPackageName
      base
    }


    phases += initVhdlBase(new PhaseVhdl(pc))





    for(phase <- phases){
      SpinalProgress(phase.getClass.getName)
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



object SpinalVerilogBoot{
  def apply[T <: Component](config : SpinalConfig)(gen : => T) : SpinalReport[T] ={
    try {
      singleShot(config)(gen)
    } catch {
      case e : NullPointerException if config.debug=> {
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

    phases += new PhaseCreateComponent(gen)(pc)
    phases += new PhaseApplyIoDefault(pc)


    phases += new PhaseDummy(SpinalProgress("Get names from reflection"))
    phases += new PhaseNameNodesByReflection(pc)
    phases += new PhaseCollectAndNameEnum(pc)

    phases += new PhaseDummy(SpinalProgress("Transform connections"))

    phases += new PhaseCheckIoBundle()
    phases += new PhaseCheckHiearchy()
    phases += new PhaseRemoveUselessStuff(false, false)
    phases += new PhaseRemoveIntermediateUnameds(true)


    phases += new PhasePullClockDomains(pc)



    phases += new PhaseDummy(SpinalProgress("Infer nodes's bit width"))
    phases += new PhaseInferEnumEncodings(pc,e => if(e == `native`) binarySequential else e)
    phases += new PhaseInferWidth(pc)
    phases += new PhaseNormalizeNodeInputs(pc)
    phases += new PhaseSimplifyNodes(pc)


    phases += new PhaseRemoveUselessStuff(true, true)
    phases += new PhaseRemoveIntermediateUnameds(false)

    phases += new PhaseCheck_noLatchNoOverride(pc)
    phases += new PhaseCheckCombinationalLoops()
    phases += new PhaseCheckCrossClock()

    phases += new PhaseAllocateNames(pc)



    phases += new PhaseVerilog(pc)





    for(phase <- phases){
      SpinalProgress(phase.getClass.getName)
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
