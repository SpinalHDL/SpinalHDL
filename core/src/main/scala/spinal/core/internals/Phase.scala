package spinal.core.internals

import spinal.core._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import spinal.core.internals._
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

  def walkBaseNodes(func : BaseNode => Unit): Unit ={
    walkStatements(s => {
      func(s)
      s.walkExpression(e => {
        func(e)
      })
    })
  }


  def checkGlobalData() : Unit = {
    if (!GlobalData.get.dslScope.isEmpty) SpinalError("dslScope stack is not empty :(")
    if (!GlobalData.get.dslClockDomain.isEmpty) SpinalError("dslClockDomain stack is not empty :(")
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


class PhaseAnalog extends PhaseNetlist{
  override def impl(pc: PhaseContext): Unit = {
    import pc._

    //Be sure that sub io assign parent component stuff
    walkComponents(c => c.ioSet.withFilter(_.isInOut).foreach(io => {
      io.foreachStatements(s => s match {
        case AssignmentStatement(_ : BaseType, x: BaseType) if (x.isAnalog && x.component == c.parent) => {
          s.dlcRemove()
          x.dlcAppend(s)
          s.target = x
          s.source = io
        }
        case _ =>
      })
    }))

    val analogs = ArrayBuffer[BaseType]()
    val islands = mutable.LinkedHashSet[mutable.LinkedHashSet[BaseType]]()
    val analogToIsland = mutable.HashMap[BaseType,mutable.LinkedHashSet[BaseType]]()

    def addToIsland(that : BaseType, island : mutable.LinkedHashSet[BaseType]) : Unit = {
      island += that
      analogToIsland(that) = island
    }



    val wrapped = mutable.HashMap[BaseType,BaseType]()
    walkStatements{
      case bt : BaseType if bt.isAnalog => {
        analogs += bt

        //Manage islands
        bt.foreachStatements(s =>  s match{
          case AssignmentStatement(x, y: BaseType) if (y.isAnalog) =>
            if(s.finalTarget.component == y.component) {
              (analogToIsland.get(bt), analogToIsland.get(y)) match {
                case (None, None) =>
                  val island = mutable.LinkedHashSet[BaseType]()
                  addToIsland(bt,island)
                  addToIsland(y,island)
                  islands += island
                case (None, Some(island)) =>
                  addToIsland(bt,island)
                case (Some(island), None) =>
                  addToIsland(y,island)
                case (Some(islandBt), Some(islandY)) =>
                  islandY.foreach(addToIsland(_, islandBt))
                  islands.remove(islandY)
              }
            }
          case AssignmentStatement(x, y: BaseType) if (!y.isAnalog) =>
        })

        if(!analogToIsland.contains(bt)){
          val island = mutable.LinkedHashSet[BaseType]()
          addToIsland(bt,island)
          islands += island
        }
      }
      case _ =>
    }


    islands.foreach(island => {
//      if(island.size > 1){ //Need to reduce island because of VHDL/Verilog capabilities
        val target = island.count(_.isInOut) match {
          case 0 => island.head
          case 1 => island.find(_.isInOut).get
          case _ => PendingError("MULTIPLE INOUT interconnected in the same component"); null
        }

        //Remove target analog assignements
        target.foreachStatements(s =>  s match {
          case AssignmentStatement(x, y: BaseType) if (y.isAnalog && y.component == target.component) => s.removeStatement()
          case _ =>
        })

        //redirect island assignements to target
        //drive isllands analogs from target as comb signal
        for(bt <- island if bt != target){
          val btStatements = ArrayBuffer[AssignmentStatement]()
          bt.foreachStatements(btStatements += _)
          btStatements.foreach(s => s match {
              case AssignmentStatement(_, x: BaseType) if (!x.isAnalog) => //analog driver
                s.dlcRemove()
                target.dlcAppend(s)
                s.walkRemapExpressions(e => if(e == bt) target else e)
              case AssignmentStatement(_, x: BaseType) if (x.isAnalog && x.component.parent == bt.component) => //analog connection
                s.dlcRemove()
                target.dlcAppend(s)
                s.walkRemapExpressions(e => if(e == bt) target else e)
              case _ =>
            }
          )
          bt.removeAssignments()
          bt.setAsComb()
          bt.rootScopeStatement.push()
          bt := target
          bt.rootScopeStatement.pop()
        }

        //Convert target comb assignement into AnalogDriver nods
        target.foreachStatements(s => {
          s.source match {
            case btSource: BaseType if btSource.isAnalog =>
            case btSource => {
              s.parentScope.push()
              val enable = ConditionalContext.isTrue(target.rootScopeStatement)
              s.parentScope.pop()
              s.removeStatementFromScope()
              target.rootScopeStatement.append(s)
              val driver = btSource.getTypeObject match {
                case `TypeBool` => new AnalogDriverBool
                case `TypeBits` => new AnalogDriverBits
                case `TypeUInt` => new AnalogDriverUInt
                case `TypeSInt` => new AnalogDriverSInt
                case `TypeEnum` => new AnalogDriverEnum(btSource.asInstanceOf[EnumEncoded].getDefinition)
              }
              driver.data = s.source.asInstanceOf[driver.T]
              driver.enable = enable
              s.source = driver
            }
          }
        })
//      }
    })
  }
}

class MemTopology(val mem: Mem[_], val consumers : mutable.HashMap[Expression, ArrayBuffer[ExpressionContainer]]) {
  val writes = ArrayBuffer[MemWrite]()
  val readsAsync = ArrayBuffer[MemReadAsync]()
  val readsSync = ArrayBuffer[MemReadSync]()
  val readWriteSync = ArrayBuffer[MemReadWrite]()
  val writeReadSameAddressSync = ArrayBuffer[(MemWrite, MemReadSync)]() //DISABLED

  var portCount = 0
  mem.foreachStatements(s => {
    portCount += 1
    s match {
      case p: MemWrite => writes += p
      case p: MemReadAsync => readsAsync += p
      case p: MemReadSync => readsSync += p
      case p: MemReadWrite => readWriteSync += p
    }
  })
}


trait PhaseMemBlackboxing extends PhaseNetlist{
  override def impl(pc: PhaseContext): Unit = {
    import pc._
    val consumers = mutable.HashMap[Expression, ArrayBuffer[ExpressionContainer]]()
    val mems = mutable.LinkedHashSet[Mem[_]]()
    walkBaseNodes{
      case mem : Mem[_] => mems += mem
      case ec : ExpressionContainer => {
        ec.foreachExpression{
          case port : MemPortStatement => consumers.getOrElseUpdate(port, ArrayBuffer[ExpressionContainer]()) += ec
          case _ =>
        }
      }
      case _ =>
    }
    mems.foreach(mem => doBlackboxing(pc, new MemTopology(mem, consumers)))
  }

  def doBlackboxing(pc: PhaseContext, typo : MemTopology) : Unit
}

abstract class PhaseMemBlackBoxingWithPolicy(policy : MemBlackboxingPolicy) extends PhaseMemBlackboxing{
  override def doBlackboxing(pc: PhaseContext, typo : MemTopology): Unit = {
    import pc._
    if(policy.translationInterest(typo)) {
      val message = doBlackboxing(typo)
      if(message != null) policy.onUnblackboxable(typo,this,message)
    }
  }

  //Return null if success
  def doBlackboxing(memTopology: MemTopology) : String
}


class PhaseMemBlackBoxingDefault(policy : MemBlackboxingPolicy) extends PhaseMemBlackBoxingWithPolicy(policy){
  def doBlackboxing(topo: MemTopology) : String = {
    val mem = topo.mem
    def wrapBool(that : Expression): Bool = that match {
      case that : Bool => that
      case that => {
        val ret = Bool()
        ret.assignFrom(that)
        ret
      }
    }

    def wrapConsumers(oldSource : Expression, newSource : Expression): Unit ={
      topo.consumers.get(oldSource) match {
        case None =>
        case Some(array) => array.foreach(ec => {
          ec.remapExpressions{
            case e if e == oldSource => newSource
            case e => e
          }
        })
      }
    }

    def removeMem(): Unit ={
      mem.removeStatement()
      mem.foreachStatements(s => s.removeStatement())
    }
    
    mem.component.rework {
      if (mem.initialContent != null) {
        return "Can't blackbox ROM"//TODO
//      } else if (topo.writes.size == 1 && topo.readsAsync.size == 1 && topo.portCount == 2) {
        } else if (topo.writes.size == 1 && (!topo.readsAsync.isEmpty || !topo.readsSync.isEmpty) && topo.writeReadSameAddressSync.isEmpty && topo.readWriteSync.isEmpty) {
        val wr = topo.writes(0)
        for (rd <- topo.readsAsync) {
          val clockDomain = wr.clockDomain
          clockDomain.push()

          val ram = new Ram_1w_1ra(
            wordWidth = mem.getWidth,
            wordCount = mem.wordCount,
            wrAddressWidth = wr.address.getWidth,
            wrDataWidth = wr.data.getWidth,
            rdAddressWidth = rd.address.getWidth,
            rdDataWidth = rd.getWidth,
            wrMaskWidth = if (wr.mask != null) wr.mask.getWidth else 1,
            wrMaskEnable = wr.mask != null,
            readUnderWrite = rd.readUnderWrite,
            technology = mem.technology
          )

          ram.io.wr.en := wrapBool(wr.writeEnable) && clockDomain.isClockEnableActive
          ram.io.wr.addr.assignFrom(wr.address)
          ram.io.wr.data.assignFrom(wr.data)
          if (wr.mask != null)
            ram.io.wr.mask.assignFrom(wr.mask)
          else
            ram.io.wr.mask := "1"


          ram.io.rd.addr.assignFrom(rd.address)
          wrapConsumers(rd, ram.io.rd.data)


          ram.setName(mem.getName())
          clockDomain.pop()
        }

        for (rd <- topo.readsSync) {
          val ram = new Ram_1w_1rs(
            wordWidth = mem.getWidth,
            wordCount = mem.wordCount,
            wrClock = wr.clockDomain,
            rdClock = rd.clockDomain,
            wrAddressWidth = wr.address.getWidth,
            wrDataWidth = wr.data.getWidth,
            rdAddressWidth = rd.address.getWidth,
            rdDataWidth = rd.getWidth,
            wrMaskWidth = if (wr.mask != null) wr.mask.getWidth else 1,
            wrMaskEnable = wr.mask != null,
            readUnderWrite = rd.readUnderWrite,
            technology = mem.technology
          )

          ram.io.wr.en := wrapBool(wr.writeEnable) && wr.clockDomain.isClockEnableActive
          ram.io.wr.addr.assignFrom(wr.address)
          ram.io.wr.data.assignFrom(wr.data)
          if (wr.mask != null)
            ram.io.wr.mask.assignFrom(wr.mask)
          else
            ram.io.wr.mask := "1"


          ram.io.rd.en := wrapBool(rd.readEnable) && rd.clockDomain.isClockEnableActive
          ram.io.rd.addr.assignFrom(rd.address)
          wrapConsumers(rd, ram.io.rd.data)

          ram.setName(mem.getName())
        }

        removeMem()
      } else if (topo.portCount == 1 && topo.readWriteSync.size == 1) {
        val port = topo.readWriteSync.head

        val ram = new Ram_1wrs(mem.getWidth, mem.wordCount,mem.technology, dontCare)

        ram.io.addr.assignFrom(port.address)
        ram.io.en.assignFrom(wrapBool(port.chipSelect) && port.clockDomain.isClockEnableActive)
        ram.io.wr.assignFrom(port.writeEnable)
        ram.io.wrData.assignFrom(port.data)

        wrapConsumers(port, ram.io.rdData)

        ram.setName(mem.getName())
        removeMem()
      } else if (topo.portCount == 2 && topo.readWriteSync.size == 2) {
        val portA = topo.readWriteSync(0)
        val portB = topo.readWriteSync(1)

        val ram = new Ram_2wrs(
          wordWidth = mem.getWidth,
          wordCount = mem.wordCount,
          technology = mem.technology,

          portA_readUnderWrite = dontCare,
          portA_clock = portA.clockDomain,
          portA_addressWidth = portA.address.getWidth,
          portA_dataWidth  = portA.getWidth,
          portA_maskWidth  = if(portA.mask != null) portA.mask.getWidth else 1,
          portA_maskEnable  = portA.mask != null,

          portB_readUnderWrite = dontCare,
          portB_clock = portB.clockDomain,
          portB_addressWidth = portB.address.getWidth,
          portB_dataWidth  = portB.getWidth,
          portB_maskWidth  = if(portB.mask != null) portB.mask.getWidth else 1,
          portB_maskEnable  = portB.mask != null
        )

        ram.io.portA.addr.assignFrom(portA.address)
        ram.io.portA.en.assignFrom(wrapBool(portA.chipSelect) && portA.clockDomain.isClockEnableActive)
        ram.io.portA.wr.assignFrom(portA.writeEnable)
        ram.io.portA.wrData.assignFrom(portA.data)
        ram.io.portA.mask.assignFrom((if(portA.mask != null) portA.mask else B"1"))
        wrapConsumers(portA, ram.io.portA.rdData)

        ram.io.portB.addr.assignFrom(portB.address)
        ram.io.portB.en.assignFrom(wrapBool(portB.chipSelect) && portB.clockDomain.isClockEnableActive)
        ram.io.portB.wr.assignFrom(portB.writeEnable)
        ram.io.portB.wrData.assignFrom(portB.data)
        ram.io.portB.mask.assignFrom((if(portB.mask != null) portB.mask else B"1"))
        wrapConsumers(portB, ram.io.portB.rdData)

        ram.setName(mem.getName())
        removeMem()
      } else {
        return "Unblackboxable memory topology"//TODO
      }
    }
    return null
  }
}

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

class PhasePullClockDomains(pc: PhaseContext) extends PhaseNetlist{
  override def impl(pc : PhaseContext): Unit = {
    import pc._

    walkComponents(c => {
      val cds = mutable.LinkedHashSet[ClockDomain]()
      c.dslBody.walkLeafStatements{
        case bt : BaseType if bt.isReg => {
          val cd = bt.clockDomain
          if(bt.isUsingResetSignal && (!cd.hasResetSignal && !cd.hasSoftResetSignal))
            SpinalError(s"MISSING RESET SIGNAL in the ClockDomain used by $bt\n${bt.getScalaLocationLong}")

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


    //Fill consumers
    walkStatements(s => s match {
      case s : AssignmentStatement =>
        val finalTarget = s.finalTarget
        s.source match {
          case source : Expression with EnumEncoded => consumers.getOrElseUpdate(source,ArrayBuffer[Expression]()) += finalTarget
          case _ =>
        }
        s.foreachDrivingExpression(e => walkExpression(e))
      case s : SwitchStatement if s.value.getTypeObject == TypeEnum =>
        s.elements.foreach(_.keys.foreach{
          case key if key.getTypeObject == TypeEnum =>  consumers.getOrElseUpdate(s.value,ArrayBuffer[Expression]()) += key
        })
        s.foreachDrivingExpression(e => walkExpression(e))
      case _ =>
        s.foreachDrivingExpression(e => walkExpression(e))
    })


    walkDeclarations{case e : Expression => walkExpression(e) case _ => }

    //Prepear the feild
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


class PhaseSimplifyNodes(pc: PhaseContext) extends PhaseNetlist{
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    val toRemove = mutable.ArrayBuffer[Statement]()
    walkStatements{
      case s : BitVector if s.getWidth == 0 => {
        s.foreachStatements(toRemove += _)
        s.removeStatement()
      }
      case s : Mem[_] if s.getWidth == 0 => {
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
            case n : DeclarationStatement => wellNameLoop ++= s"    >>> ${n.toString()} at ${n.getScalaLocationShort} >>>\n"
            case _ =>
          }
          val multiLineLoop = filtred.reverseIterator.filter(!_.isInstanceOf[AssignmentStatement]).map(n => "    " + n.toString).foldLeft("")(_ + "\n" + _)
          PendingError(s"COMBINATORIAL LOOP :\n  Partial chain :\n${wellNameLoop}\n  Full chain :${multiLineLoop}")
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
              s"""CLOCK CROSSING VIOLATION from ${syncDriver} to ${s}.
                 |- Register declaration at
                 |${s.getScalaLocationLong}- through
                 |${wellNameLoop}"
               """.stripMargin
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
            propagate(s.mem)
            s.walkExpression{ case e : Statement => propagate(e) case _ => }
          }
          case s : MemReadSync => {
            propagate(s.mem)
            s.walkExpression{ case e : Statement => propagate(e) case _ => }
          }
          case s : MemReadAsync => {
            propagate(s.mem)
            s.walkExpression{ case e : Statement => propagate(e) case _ => }
          }
        }
      }
    }

    //Propagate all vital signals (drive toplevel output and blackboxes inputs)
    topLevel.getAllIo.withFilter(bt => bt.isOutputOrInOut).foreach(propagate(_, tagVitals))
    walkComponents{
      case c : BlackBox => c.getAllIo.withFilter(_.isInputOrInOut).foreach(propagate(_, tagVitals))
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
          if(ref.algoInt == 1 && ref.isComb && ref.isDirectionLess && (!onlyTypeNode || ref.isTypeNode) && ref.canSymplifyIt  && Statement.isSomethingToFullStatement(ref) /*&& ref != excepted*/){ //TODO IR keep it
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

class PhaseCompletSwitchCases extends PhaseNetlist{
  override def impl(pc: PhaseContext): Unit = {
    import pc._
    walkStatements{
      case s : SwitchStatement if s.isFullyCoveredWithoutDefault => {
        if(s.defaultScope != null && !s.defaultScope.isEmpty){
          PendingError(s"UNREACHABLE DEFAULT STATEMENT on \n" + s.getScalaLocationLong)
        }
        s.defaultScope = s.elements.last.scopeStatement
        s.elements.remove(s.elements.length-1)
      }
      case _ =>
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
          if(bt.isDirectionLess && !bt.hasTag(allowDirectionLessIoTag)){
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
            if (!(bt.isDirectionLess && bt.component == c) && !(bt.isOutputOrInOut && bt.component == c) && !(bt.isInputOrInOut && bt.component.parent == c)) {
              PendingError(s"HIERARCHY VIOLATION : $bt is drived by the $s statement, but isn't accessible in the $c component.\n${s.getScalaLocationLong}")
              error = true
            }

            if(!error && !(bt.isInOut)){
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
            if(!(bt.component == c) && !(bt.isInputOrInOut && bt.component.parent == c) && !(bt.isOutputOrInOut && bt.component.parent == c)){
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
class PhaseCheck_noRegisterAsLatch() extends PhaseCheck{
  override def impl(pc: PhaseContext): Unit = {
    import pc._
    walkStatements{
      case bt : BaseType if bt.isReg && (bt.isVital) => {
        var assignedBits = new AssignedBits(bt.getBitsWidth)
        bt.foreachStatements{
          case s : DataAssignmentStatement =>
            s.target match {
              case bt: BaseType => assignedBits.add(bt.getBitsWidth - 1, 0)
              case e: BitVectorAssignmentExpression =>  assignedBits.add(e.getMaxAssignedBits)
            }
          case _ =>
        }
        if(!assignedBits.isFull){
          if(assignedBits.isEmpty) {
            var withInit = false
            bt.foreachStatements{
              case s : InitAssignmentStatement => withInit = true
              case _ =>
            }
            if(bt.hasTag(unsetRegIfNoAssignementTag) && withInit){
              bt.setAsComb()
              val statements = ArrayBuffer[AssignmentStatement]()

              bt.foreachStatements(statements += _)

              statements.foreach{
                case s : InitAssignmentStatement => {
                  s.insertNext(DataAssignmentStatement(s.target, s.source).setScalaLocated(s))
                  s.removeStatement()
                }
              }
            }else {
              PendingError(s"UNASSIGNED REGISTER $bt, defined at\n${bt.getScalaLocationLong}")
            }
          }else {
            val unassignedBits = new AssignedBits(bt.getBitsWidth)
            unassignedBits.add(bt.getBitsWidth - 1, 0)
            unassignedBits.remove(assignedBits)
            PendingError(s"PARTIALLY ASSIGNED REGISTER $bt, unassigned bit mask is ${unassignedBits.toBinaryString}, defined at\n${bt.getScalaLocationLong}")
          }
        }
      }
      case _ =>
    }
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
        def getOrEmptyAdd3(bt : BaseType, hi : Int, lo : Int): Boolean = {
          val dst = getOrEmpty(bt)
          val ret = hi == dst.width-1 && lo == 0 && !dst.isEmpty  && !bt.hasTag(allowAssignmentOverride)
          dst.add(hi, lo)
          ret
        }
        def getOrEmptyAdd2(bt : BaseType, src : AssignedRange): Boolean = getOrEmptyAdd3(bt,src.hi, src.lo)

        body.foreachStatements {
          case s: DataAssignmentStatement => { //Omit InitAssignmentStatement
            if(!s.finalTarget.isAnalog) {
              s.target match {
                case bt: BaseType => if (getOrEmptyAdd3(bt, bt.getBitsWidth - 1, 0)) {
                  PendingError(s"ASSIGNMENT OVERLAP completely the previous one of $bt\n${s.getScalaLocationLong}")
                }
                case e: BitVectorAssignmentExpression => {
                  val bt = e.finalTarget
                  if (getOrEmptyAdd2(bt, e.getMinAssignedBits)) {
                    PendingError(s"ASSIGNMENT OVERLAP completely the previous one of $bt\n${s.getScalaLocationLong}")
                  }
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
                  PendingError(s"ASSIGNMENT OVERLAP completely the previous one of $bt\n ${s.getScalaLocationLong}")
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
                  PendingError(s"ASSIGNMENT OVERLAP completely the previous one of $bt\n ${s.getScalaLocationLong}")
                }
              }
            }
          }
          case signal : BaseType if !(signal.component.isInBlackBoxTree && !signal.isInput) && !(signal.component.parent == null && signal.isInput)  => {
            getOrEmpty(signal)
          }
          case s =>
         }

        for((bt, assignedBits) <- assigneds if (bt.isVital || !bt.dlcIsEmpty) && bt.rootScopeStatement == body && !assignedBits.isFull){
          if(bt.isComb) {
            val unassignedBits = new AssignedBits(bt.getBitsWidth)
            unassignedBits.add(bt.getBitsWidth - 1, 0)
            unassignedBits.remove(assignedBits)
            if (!unassignedBits.isEmpty) {
              if (bt.dlcIsEmpty)
                PendingError(s" signal $bt, defined at\n${bt.getScalaLocationLong}")
              else if (unassignedBits.isFull)
                PendingError(s"LATCH DETECTED from the combinatorial signal $bt, defined at\n${bt.getScalaLocationLong}")
              else
                PendingError(s"LATCH DETECTED from the combinatorial signal $bt, unassigned bit mask " +
                  s"is ${unassignedBits.toBinaryString}, defined at\n${bt.getScalaLocationLong}")
            }
          }
        }

        assigneds
      }
      walkBody(c.dslBody)
    })
  }
}



class PhasePrintUnUsedSignals(prunedSignals : mutable.Set[BaseType],unusedSignals : mutable.Set[BaseType])(pc: PhaseContext) extends PhaseCheck{
  override def impl(pc : PhaseContext): Unit = {
    import pc._

//    val targetAlgoId = GlobalData.get.algoId
//    Node.walk(walkNodesDefautStack,node => {node.algoId = targetAlgoId})
    walkStatements{
      case bt : BaseType if !bt.isVital && (!bt.isInstanceOf[BitVector] || bt.asInstanceOf[BitVector].inferredWidth != 0) && !bt.hasTag(unusedTag) && bt.isNamed && !bt.getName().startsWith(globalData.anonymSignalPrefix) => {
        prunedSignals += bt
      }
      case _ =>
    }
//    for(c <- components){
//      def checkNameable(that : Any) : Unit = that match {
//        case area : Area => {
//          area.foreachReflectableNameables(obj => checkNameable(obj))
//        }
//        case data : Data =>  {
//          data.flatten.foreach(bt => {
//            if(!bt.isVital && (!bt.isInstanceOf[BitVector] || bt.asInstanceOf[BitVector].inferredWidth != 0) && !bt.hasTag(unusedTag) && bt.isNamed){
//              prunedSignals += bt
//            }
//          })
//        }
//        case _ =>
//      }
//
//      c.foreachReflectableNameables(obj => checkNameable(obj))
//    }
    if(!prunedSignals.isEmpty){
      SpinalWarning(s"${prunedSignals.size} signals were pruned. You can call printPruned on the backend report to get more informations.")
    }


    val usedId = GlobalData.get.allocateAlgoIncrementale()
    walkStatements(s => {
      s.walkDrivingExpressions(e => e.algoIncrementale = usedId)
      s match {
        case s: MemReadSync => s.algoIncrementale = usedId
        case s: MemReadAsync => s.algoIncrementale = usedId
        case s: MemReadWrite => s.algoIncrementale = usedId
        case s =>
      }
    })

    prunedSignals.foreach(s => {
      if(s.algoIncrementale != usedId) {
        unusedSignals += s
      }
    })
  }
}

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
    defaultClockDomain.push()
    binaryOneHot //Avoid unconstructable during phase
    pc.topLevel = gen
    defaultClockDomain.pop()
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
    phases ++= config.transformationPhases
    phases ++= config.memBlackBoxers
    phases += new PhaseApplyIoDefault(pc)


    phases += new PhaseDummy(SpinalProgress("Get names from reflection"))
    phases += new PhaseNameNodesByReflection(pc)
    phases += new PhaseCollectAndNameEnum(pc)

    phases += new PhaseDummy(SpinalProgress("Transform connections"))

    phases += new PhaseCheckIoBundle()
    phases += new PhaseCheckHiearchy()
    phases += new PhaseAnalog()
    phases += new PhaseRemoveUselessStuff(false, false)
    phases += new PhaseRemoveIntermediateUnameds(true)


    phases += new PhasePullClockDomains(pc)



    phases += new PhaseDummy(SpinalProgress("Infer nodes's bit width"))
    phases += new PhaseInferEnumEncodings(pc,e => e)
    phases += new PhaseInferWidth(pc)
    phases += new PhaseNormalizeNodeInputs(pc)
    phases += new PhaseSimplifyNodes(pc)


    phases += new PhaseCompletSwitchCases()
    phases += new PhaseRemoveUselessStuff(true, true)
    phases += new PhaseRemoveIntermediateUnameds(false)

    phases += new PhaseCheck_noLatchNoOverride(pc)
    phases += new PhaseCheck_noRegisterAsLatch()
    phases += new PhaseCheckCombinationalLoops()
    phases += new PhaseCheckCrossClock()

    phases += new PhaseAllocateNames(pc)

    def initVhdlBase[T <: VhdlBase](base : T) = {
      base.packageName     = pc.config.globalPrefix + base.packageName
      base.enumPackageName = pc.config.globalPrefix + base.enumPackageName
      base
    }

    phases += new PhasePrintUnUsedSignals(prunedSignals,unusedSignals)(pc)
    phases += initVhdlBase(new PhaseVhdl(pc))

    for(inserter <-config.phasesInserters){
      inserter(phases)
    }



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
    phases ++= config.transformationPhases
    phases ++= config.memBlackBoxers
    phases += new PhaseApplyIoDefault(pc)


    phases += new PhaseDummy(SpinalProgress("Get names from reflection"))
    phases += new PhaseNameNodesByReflection(pc)
    phases += new PhaseCollectAndNameEnum(pc)

    phases += new PhaseDummy(SpinalProgress("Transform connections"))

    phases += new PhaseCheckIoBundle()
    phases += new PhaseCheckHiearchy()
    phases += new PhaseAnalog()
    phases += new PhaseRemoveUselessStuff(false, false)
    phases += new PhaseRemoveIntermediateUnameds(true)


    phases += new PhasePullClockDomains(pc)



    phases += new PhaseDummy(SpinalProgress("Infer nodes's bit width"))
    phases += new PhaseInferEnumEncodings(pc,e => if(e == `native`) binarySequential else e)
    phases += new PhaseInferWidth(pc)
    phases += new PhaseNormalizeNodeInputs(pc)
    phases += new PhaseSimplifyNodes(pc)


    phases += new PhaseCompletSwitchCases()
    phases += new PhaseRemoveUselessStuff(true, true)
    phases += new PhaseRemoveIntermediateUnameds(false)

    phases += new PhaseCheck_noLatchNoOverride(pc)
    phases += new PhaseCheck_noRegisterAsLatch()
    phases += new PhaseCheckCombinationalLoops()
    phases += new PhaseCheckCrossClock()

    phases += new PhaseAllocateNames(pc)


    phases += new PhasePrintUnUsedSignals(prunedSignals,unusedSignals)(pc)
    phases += new PhaseVerilog(pc)



    for(inserter <-config.phasesInserters){
      inserter(phases)
    }

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
