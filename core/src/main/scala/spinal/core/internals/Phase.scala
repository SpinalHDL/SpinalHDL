/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core.internals

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable.ListBuffer
import spinal.core._
import spinal.core.fiber.Engine
import spinal.core.internals.Operator.BitVector

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import spinal.core.internals._
import java.util

import scala.io.Source
import scala.util.Random


class PhaseContext(val config: SpinalConfig) {
  var globalData = GlobalData.reset(config)
  config.applyToGlobalData(globalData)

  def privateNamespaceName = config.globalPrefix + (if(config.privateNamespace) topLevel.definitionName + "_" else "")

  val duplicationPostfix = ""
  val globalScope         = new NamingScope(duplicationPostfix)
  var topLevel: Component = null
  val enums               = mutable.LinkedHashMap[SpinalEnum,mutable.LinkedHashSet[SpinalEnumEncoding]]()

  val vhdlKeywords = Array(
    "abs", "access", "after", "alias", "all", "and", "architecture", "array", "assert",
    "attribute", "begin", "block", "body", "buffer", "bus", "case", "component",
    "configuration", "constant", "disconnect", "downto", "else", "elsif", "end", "entity", "exit", "file",
    "for", "function", "generate", "generic", "group", "guarded", "if", "impure", "in",
    "inertial", "inout", "is", "label", "library", "linkage", "literal", "loop", "map", "mod",
    "nand", "new", "next", "nor", "not", "null", "of", "on", "open", "or", "others", "out",
    "package", "port", "postponed", "procedure", "process", "pure", "range", "record", "register",
    "reject", "rem", "report", "return", "rol", "ror", "select", "severity", "signal", "shared",
    "sla", "sll", "sra", "srl", "subtype", "then", "to", "transport", "type", "unaffected", "units",
    "until", "use", "variable", "wait", "when", "while", "with", "xnor", "xor"
  )

  val verilogKeywords = Array(
    "always", "end", "ifnone", "or", "rpmos", "tranif1", "and", "endcase", "initial", "output",
    "rtran", "tri", "assign", "endmodule", "inout", "parameter", "rtranif0", "tri0", "begin", "endfunction", "input", "pmos",
    "rtranif1", "tri1", "buf", "endprimitive", "integer", "posedge", "scalared", "triand", "bufif0", "endspecify", "join", "primitive",
    "small", "trior", "bufif1", "endtable", "large", "pull0", "specify", "trireg", "case", "endtask", "macromodule",
    "pull1", "specparam", "vectored", "casex", "event", "medium", "pullup", "strong0", "wait", "casez", "for", "module", "pulldown",
    "strong1", "wand", "cmos", "force", "nand", "rcmos", "supply0", "weak0", "deassign", "forever", "negedge", "real",
    "supply1", "weak1", "default", "for", "nmos", "realtime", "table", "while", "defparam", "function", "nor", "reg", "task", "wire", "disable", "highz0",
    "not", "release", "time", "wor", "edge", "highz1", "notif0", "repeat", "tran", "xnor", "else", "if", "notif1", "rnmos", "tranif0", "xor"
  )

  val systemVerilogKeywords = Array(
    "alias", "always", "always_comb", "always_ff", "always_latch", "and", "assert", "assign", "assume", "automatic", "before", "begin", "bind",
    "bins", "binsof", "bit", "break", "buf", "bufif0", "bufif1", "byte", "case", "casex", "casez", "cell", "chandle", "class",
    "clocking", "cmos", "config", "const", "constraint", "context", "continue", "cover", "covergroup", "coverpoint", "cross",
    "deassign", "default", "defparam", "design", "disable", "dist", "do", "edge", "else", "end", "endcase", "endclass",
    "endclocking", "endconfig", "endfunction", "endgenerate", "endgroup", "endinterface", "endmodule", "endpackage", "endprimitive", "endprogram",
    "endproperty", "endspecify", "endsequence", "endtable", "endtask", "enum", "event", "expect", "export", "extends", "extern", "final",
    "first_match", "for", "force", "foreach", "forever", "fork", "forkjoin", "function", "generate", "genvar", "highz0", "highz1", "if",
    "iff", "ifnone", "ignore_bins", "illegal_bins", "import", "incdir", "include", "initial", "inout", "input", "inside", "instance", "int", "integer", "interface",
    "intersect", "join", "join_any", "join_none", "large", "liblist", "library", "local", "localparam", "logic", "longint",
    "macromodule", "matches", "medium", "modport", "module", "nand", "negedge", "new", "nmos", "nor", "noshowcancelled",
    "not", "notif0", "notif1", "null", "or", "output", "package", "packed", "parameter", "pmos", "posedge", "primitive",
    "priority", "program", "property", "protected", "pull0", "pull1", "pulldown", "pullup", "pulsestyle_onevent", "pulsestyle_ondetect", "pure", "rand", "randc",
    "randcase", "randsequence", "rcmos", "real", "realtime", "ref", "reg", "release", "repeat", "return", "rnmos", "rpmos", "rtran",
    "rtranif0", "rtranif1", "scalared", "sequence", "shortint", "shortreal", "showcancelled", "signed", "small", "solve", "specify", "specparam",
    "static", "string", "strong0", "strong1", "struct", "super", "supply0", "supply1", "table", "tagged", "task", "this", "throughout",
    "time", "timeprecision", "timeunit", "tran", "tranif0", "tranif1", "tri", "tri0", "tri1", "triand", "trior", "trireg", "type", "typedef", "union",
    "unique", "unsigned", "use", "uwire", "var", "vectored", "virtual", "void", "wait", "wait_order", "wand", "weak0", "weak1",
    "while", "wildcard", "wire", "with", "within", "wor", "xnor", "xor"
  )

  val reservedKeyWords    = mutable.Set[String]()
  reservedKeyWords ++= vhdlKeywords
  reservedKeyWords ++= verilogKeywords
  reservedKeyWords ++= systemVerilogKeywords

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

  def sortedComponents = components().sortWith(_.level > _.level)

  def walkAll(func: Any => Unit): Unit = {
    GraphUtils.walkAllComponents(topLevel, c => {
      func(c)
      c.dslBody.walkStatements(s => {
        func(s)
        s.walkExpression(e => {
          func(e)
        })
      })
    })
  }

  def walkStatements(func: Statement => Unit): Unit = {
    GraphUtils.walkAllComponents(topLevel, c => c.dslBody.walkStatements(func))
  }

  def walkExpression(func: Expression => Unit): Unit = {
    GraphUtils.walkAllComponents(topLevel, c => c.dslBody.walkStatements(s => s.walkExpression(func)))
  }

  def walkExpressionPostorder(func: Expression => Unit): Unit = {
    GraphUtils.walkAllComponents(topLevel, c => c.dslBody.walkStatements(s => s.walkExpressionPostorder(func)))
  }

  def walkDeclarations(func: DeclarationStatement => Unit): Unit = {
    walkComponents(c => c.dslBody.walkDeclarations(e => func(e)))
  }

  def walkRemapExpressions(func: Expression => Expression): Unit ={
    GraphUtils.walkAllComponents(topLevel, c => c.dslBody.walkStatements(s => s.walkRemapExpressions(func)))
  }

  def walkDrivingExpression(func: Expression => Unit): Unit ={
    GraphUtils.walkAllComponents(topLevel, c => c.dslBody.walkStatements(s => s.walkDrivingExpressions(func)))
  }

  def walkComponents(func: Component => Unit): Unit ={
    GraphUtils.walkAllComponents(topLevel, c => func(c))
  }

  def walkComponentsExceptBlackbox(func: Component => Unit): Unit ={
    GraphUtils.walkAllComponents(topLevel, c => if(!c.isInstanceOf[BlackBox] || !c.asInstanceOf[BlackBox].isBlackBox) func(c))
  }

  def walkBaseNodes(func: BaseNode => Unit): Unit ={
    walkStatements(s => {
      func(s)
      s.walkExpression(e => {
        func(e)
      })
    })
  }

  def checkGlobalData(): Unit = {
    if (DslScopeStack.get != null) SpinalError("dslScope stack is not empty :(")
    if (ClockDomainStack.get != null) SpinalError("dslClockDomain stack is not empty :(")
  }

  def checkPendingErrors(msg : String) = if(globalData.pendingErrors.nonEmpty)
    SpinalError(msg)

  val verboseLog = if(config.verbose) new java.io.FileWriter("verbose.log") else null

  def doPhase(phase: Phase): Unit ={
    if(config.verbose) verboseLog.write(s"phase: $phase\n")
    phase.impl(this)
    if(config.verbose){
      var checksum = 0
      def seed(that : Int) = checksum = ((checksum << 1) | ((checksum & 0x80000000) >> 31)) ^ that
      walkComponents{c =>seed(c.getInstanceCounter)}
      walkStatements{s =>seed(s.getInstanceCounter)}
      verboseLog.write(s"checksum: $checksum\n")
      verboseLog.flush()

    }
    checkPendingErrors("Error detected in phase " + classNameOf(phase))
  }
}


trait Phase {
  def impl(pc: PhaseContext): Unit
  def hasNetlistImpact: Boolean
}


trait PhaseNetlist extends Phase {
  override def hasNetlistImpact: Boolean = true
}


trait PhaseMisc extends Phase {
  override def hasNetlistImpact: Boolean = false
}


trait PhaseCheck extends Phase {
  override def hasNetlistImpact: Boolean = false
}


//class PhaseZeroMemToReg extends PhaseNetlist{
//  override def impl(pc: PhaseContext): Unit = {
//    import pc._
//    walkDeclarations{
//      case mem : Mem[_] if mem.addressWidth == 0 => {
//        println("SIMPLIFY ME")
//        mem.component.rework{
//          val storage = Reg(Bits(mem.width bits))
//          mem.foreachStatements{
//            case port : MemWrite => {
//              when(port.writeEnable.asInstanceOf[Bool]){
//                storage := port.data.asInstanceOf[Bits]
//              }
//            }
//            case port : MemReadAsync => {
//              po
//            }
//            case _ =>
//          }
//        }
//        mem.removeStatement()
//        mem.foreachStatements(s => s.removeStatement())
//      }
//      case _ =>
//    }
//  }
//}

class PhaseDeviceSpecifics(pc : PhaseContext) extends PhaseNetlist{
  override def impl(pc: PhaseContext): Unit = {
    import pc._

//    walkDeclarations{
//      case mem : Mem[_] =>
//        var hit = false
//        mem.foreachStatements{
//          case port : MemReadAsync => hit = true
//          case _ =>
//        }
//        if(hit) mem.addAttribute("ram_style", "distributed") //Vivado stupid ganbling workaround Synth 8-6430
//      case _ =>
//    }
  }
}

class PhaseApplyIoDefault(pc: PhaseContext) extends PhaseNetlist{

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    walkDeclarations {
      case node: BaseType if node.dlcIsEmpty => node.getTag(classOf[DefaultTag]) match {
        case Some(defaultValue) =>
          val c = node.dir match {
            case `in` => node.component.parent
            case `out` => node.component
            case `inout` => PendingError(s"DEFAULT INOUT isn't allowed on $node at\n${node.getScalaLocationLong}")
            case _ => node.component
          }

          if (c != null) {
            val scope = node.rootScopeStatement
            val ctx = scope.push()
            node.assignFrom(defaultValue.that)
            ctx.restore()
          }
        case _ =>
      }
      case _ =>
    }
  }
}

class PhaseAnalog extends PhaseNetlist{

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    case class Bit(bt : BaseType, bitId : Int, scope : ScopeStatement)
    // A island represent the connection of multiple analog single bits
    class Island {
      def absorb(other: Island): Unit = {
        this.elements ++= other.elements
      }
      val elements = mutable.LinkedHashSet[Bit]()
      def add(that : Bit): Unit ={
        elements += that
      }
    }


    pc.walkComponents { c =>
      val islands = mutable.LinkedHashSet[Island]()
      val bitToIsland = mutable.HashMap[(BaseType, Int), Island]()
      def islandOf(b : Bit) = bitToIsland.get(b.bt -> b.bitId)

      // Generate the list of islands
      c.dslBody.walkStatements {
        case s: AssignmentStatement => {
          val targetBt = s.finalTarget
          val sourceBt : BaseType = s.source match {
            case bt: BaseType => bt
            case e: SubAccess => e.getBitVector match {
              case bt: BaseType => bt
              case _ => null
            }
            case _ => null
          }
          if (targetBt.isAnalog) {
            val targetRange = s.target match {
              case bt: BaseType => (0 until bt.getBitsWidth)
              case e: BitAssignmentFixed => (e.bitId to e.bitId)
              case e: RangedAssignmentFixed => (e.lo to e.hi)
              case _ => SpinalError(s"Unsupported statement $s")
            }
            val sourceRange = s.source match {
              case bt: BaseType => (0 until bt.getBitsWidth)
              case e: BitVectorBitAccessFixed => (e.bitId to e.bitId)
              case e: BitVectorRangedAccessFixed => (e.lo to e.hi)
              case w: WidthProvider => (0 until w.getWidth)
              case _ => SpinalError(s"Unsupported statement $s")
            }
            if(targetRange.size != sourceRange.size)
              SpinalError(s"WIDTH MISMATCH IN ANALOG ASSIGNMENT $s\n${s.getScalaLocationLong}")

            if(targetRange.size > 0) {
              if (sourceBt == null) SpinalError(":(")
              for (i <- 0 until targetRange.size) {
                val a = Bit(targetBt, targetRange.low + i, c.dslBody)
                val b = Bit(sourceBt, sourceRange.low + i, s.parentScope)
                val island: Island = (islandOf(a), islandOf(b)) match {
                  case (None, None) =>
                    val island = new Island()
                    islands += island
                    island
                  case (None, Some(island)) => island
                  case (Some(island), None) => island
                  case (Some(islandBt), Some(islandY)) =>
                    if(islandBt != islandY) {
                      for (e <- islandY.elements) bitToIsland(e.bt -> e.bitId) = islandBt
                      islandBt.absorb(islandY)
                      islands.remove(islandY)
                    }
                    islandBt
                }

                island.add(a)
                island.add(b)
                bitToIsland(a.bt -> a.bitId) = island
                //Do not add digital drivers into the island merge logic
                if (b.bt.isAnalog) bitToIsland(b.bt -> b.bitId) = island

              }
            }
            s.removeStatement()
          }
        }
        case _ =>
      }
      var seedId = 0
      val seeds = mutable.LinkedHashSet[BaseType]() //List of all analog signal being the "root" of a network
      class Group{
        val islands = ArrayBuffer[Island]()
      }
      val analogGroups = mutable.LinkedHashSet[Group]()
      val anlogToGroup = mutable.LinkedHashMap[BaseType, Group]()

      //Process the islands to generate seeds from component analog inouts, and build the group list for connections without inout analog
      islands.foreach(island => {
        val filtred = island.elements.map(_.bt).toSet
        val count = filtred.count(e => e.isInOut && e.component == c)
        count match {
          case 0 => { //No analog inout, will need to create a connection seed later on
            val groups = island.elements.map(b => anlogToGroup.get(b.bt)).filter(_.nonEmpty).map(_.get).toArray.distinct
            val finalGroup = groups.length match {
              case 0 => {
                val group = new Group()
                analogGroups += group
                group
              }
              case 1 => groups.head
              case _ =>{
                val ghead = groups.head
                for(group <- groups.tail){
                  groups.head.islands ++= group.islands
                  analogGroups -= group
                  for(i <- group.islands){
                    for(b <- i.elements){
                      anlogToGroup(b.bt) = ghead
                    }
                  }
                }
                ghead
              }
            }
            finalGroup.islands += island
            for(b <- island.elements){
              anlogToGroup(b.bt) = finalGroup
            }
          }
          case 1 => seeds += island.elements.find(e => e.bt.isInOut && e.bt.component == c).get.bt //Got a analog inout to host the connection
          case _ => PendingError("MULTIPLE INOUT interconnected in the same component"); null
        }
      })

      //For each group, this will generate a analog host signal
      for(group <- analogGroups)      c.rework{
        val width = group.islands.size
        val seed = Analog(Bits(width bits))
        seed.setName(s"analog_wrap_$seedId")
        seedId += 1
        for((island, i) <- group.islands.zipWithIndex) {
          val b = Bit(seed, i, c.dslBody)
          island.elements += b
          bitToIsland(b.bt -> b.bitId) = island
        }
        seeds += seed
      }

      val toComb = mutable.LinkedHashSet[BaseType]()

      //Generate all the statements associated to the seeds by aggreating consecutive bits connection and then flushing it into RTL
      for(seed <- seeds){
        case class AggregateKey(bt : BaseType, scope : ScopeStatement)
        case class AggregateCtx(seedOffset : Int, otherOffset : Int)
        var aggregates = mutable.LinkedHashMap[AggregateKey, AggregateCtx]()
        def flush(key : AggregateKey, ctx : AggregateCtx, seedUntil : Int): Unit ={
          val ctx = aggregates(key)
          aggregates -= key
          val width = seedUntil - ctx.seedOffset
          key.bt match {
            //Mutate directionless analog into combinatorial reader of the seed
            case bt if bt.isAnalog && bt.isDirectionLess => if(key.scope == c.dslBody) bt.parentScope.on{
              toComb += bt
              bt.compositeAssign = null
              bt match{
                case bt : Bool => bt := (seed match {
                  case seed : Bool => seed
                  case seed : BitVector => seed(ctx.seedOffset).setAsComb()
                })
                case bt : BitVector => bt(ctx.otherOffset, width bits) := (seed match {
                  case seed : Bool => seed.asBits
                  case seed : BitVector => seed(ctx.seedOffset, width bits).setAsComb()
                })
              }
            }
            //Handle analog inout of sub components
            case bt if bt.isAnalog && bt.isInOut => c.dslBody.on{
              val driver = bt match {
                case bt : BitVector if width != widthOf(bt) => bt(ctx.otherOffset, width bits).setAsComb()
                case _ => bt
              }
              seed match{
                case seed : BitVector if width != widthOf(seed)  || driver.getTypeObject != seed.getTypeObject => driver.getTypeObject match {
                  case `TypeBool` => {
                    seed(ctx.seedOffset).setAsComb() := False
                    seed.dlcLast.source = driver
                  }
                  case _ => {
                    seed(ctx.seedOffset, width bits).setAsComb() := seed(ctx.otherOffset, width bits).setAsComb().getZero
                    seed.dlcLast.source = driver
                  }
                }
                case bt => {
                  bt.assignFrom(driver)
                }
              }
            }
            //Handle tristate drivers to the seed
            case bt if !bt.isAnalog => {
              val tmp = key.scope.push()
              val enable = ConditionalContext.isTrue(seed.rootScopeStatement)
              tmp.restore()
              c.dslBody.on{
                val driver = bt.getTypeObject match {
                  case `TypeBool` => new AnalogDriverBool
                  case `TypeBits` => new AnalogDriverBits
                  case `TypeUInt` => new AnalogDriverUInt
                  case `TypeSInt` => new AnalogDriverSInt
                  case `TypeEnum` => new AnalogDriverEnum(bt.asInstanceOf[EnumEncoded].getDefinition)
                }
                driver.data = (bt match {
                  case bv : BitVector => bv.apply(ctx.otherOffset, width bits)
                  case _ => bt
                }).asInstanceOf[driver.T]
                driver.enable = enable
                seed match{
                  case seed : BitVector if width != widthOf(seed) || bt.getTypeObject == TypeBool => bt.getTypeObject match {
                    case `TypeBool` => {
                      seed(ctx.seedOffset).setAsComb() := seed(ctx.seedOffset).getZero
                      seed.dlcLast.source = driver
                    }
                    case _ => {
                      seed(ctx.seedOffset, width bits).setAsComb() := seed(ctx.otherOffset, width bits).getZero
                      seed.dlcLast.source = driver
                    }
                  }
                  case bt => bt.assignFrom(driver)
                }
              }
            }
          }
        }

        //Detect bitvector bits sequencial connections xxx(5 downto 2) => 3 bits
        for(bitId <- 0 until widthOf(seed)){
          val flushes = ArrayBuffer[(AggregateKey, AggregateCtx)]()
          bitToIsland.get(seed -> bitId) match {
            case Some(island) => {
              for((key, ctx) <- aggregates){
                island.elements.contains(Bit(key.bt, (bitId - ctx.seedOffset) + ctx.otherOffset, key.scope)) match {
                  case true => //Continue
                  case false => flushes += key -> ctx
                }
              }
              for(e <- flushes) flush(e._1, e._2, bitId)
              for(e <- island.elements if e.bt != seed){
                val key = AggregateKey(e.bt, e.scope)
                aggregates.get(key) match {
                  case Some(x) =>
                  case None => aggregates(key) = AggregateCtx(bitId, e.bitId)
                }
              }
            }
            case None => {
              for((key, ctx) <- aggregates){
                  flushes += key -> ctx
              }
              for(e <- flushes) flush(e._1, e._2, bitId)
            }
          }
        }
        for(e <- aggregates.toArray) flush(e._1, e._2, widthOf(seed))
      }
      toComb.foreach(_.setAsComb())
    }
  }
}







//class PhaseAnalog extends PhaseNetlist{
//
//  override def impl(pc: PhaseContext): Unit = {
//    import pc._
//
//    //Be sure that sub io assign parent component stuff
//    walkComponents(c => c.ioSet.withFilter(_.isInOut).foreach(io => {
//      io.foreachStatements {
//        case s@AssignmentStatement(_: BaseType, x: BaseType) if x.isAnalog && x.component == c.parent =>
//          s.dlcRemove()
//          x.dlcAppend(s)
//          s.target = x
//          s.source = io
//        case _ =>
//      }
//    }))
//
//    val islands        = mutable.LinkedHashSet[mutable.LinkedHashSet[(BaseType, Int)]]()
//    val analogToIsland = mutable.HashMap[(BaseType, Int),mutable.LinkedHashSet[(BaseType, Int)]]()
//
//    def addToIsland(that: BaseType, bit : Int, island: mutable.LinkedHashSet[(BaseType, Int)]): Unit = {
//      island += that -> bit
//      analogToIsland(that -> bit) = island
//    }
//
//    walkStatements{
//      case bt: BaseType if bt.isAnalog =>
//
//        //Manage islands
//        bt.foreachStatements {
//          case s@AssignmentStatement(x, y: BaseType) if y.isAnalog =>
//            if (s.finalTarget.component == y.component) {
//              var width = 0
//              val sourceOffset = 0
//              var targetOffset = 0
//              s.target match {
//                case bt : BaseType => width = s.finalTarget.getBitsWidth
//                case baf : BitAssignmentFixed => width = 1; targetOffset = baf.bitId
//              }
//              for(i <- 0 until width) (analogToIsland.get(bt, targetOffset+i), analogToIsland.get(y, sourceOffset+i)) match {
//                case (None, None) =>
//                  val island = mutable.LinkedHashSet[(BaseType, Int)]()
//                  addToIsland(bt, targetOffset+i, island)
//                  addToIsland(y, sourceOffset+i, island)
//                  islands += island
//                case (None, Some(island)) =>
//                  addToIsland(bt, targetOffset+i, island)
//                case (Some(island), None) =>
//                  addToIsland(y, sourceOffset+i, island)
//                case (Some(islandBt), Some(islandY)) =>
//                  islandY.foreach(e => addToIsland(e._1, e._2, islandBt))
//                  islands.remove(islandY)
//              }
//            }
//          case AssignmentStatement(x, y: BaseType) if !y.isAnalog =>
//        }
//
//        for(bit <- 0 until widthOf(bt)) {
//          if (!analogToIsland.contains(bt, bit)) {
//            val island = mutable.LinkedHashSet[(BaseType, Int)]()
//            addToIsland(bt, bit, island)
//            islands += island
//          }
//        }
//      case _ =>
//    }
//    /*
//        islands.foreach(island => {
//          //      if(island.size > 1){ //Need to reduce island because of VHDL/Verilog capabilities
//          val target = island.count(_.isInOut) match {
//            case 0 => island.head
//            case 1 => island.find(_.isInOut).get
//            case _ => PendingError("MULTIPLE INOUT interconnected in the same component"); null
//          }
//
//          //Remove target analog assignments
//          target.foreachStatements {
//            case s@AssignmentStatement(x, y: BaseType) if y.isAnalog && y.component == target.component => s.removeStatement()
//            case _ =>
//          }
//
//          //redirect island assignments to target
//          //drive isllands analogs from target as comb signal
//          for(bt <- island if bt != target){
//            val btStatements = ArrayBuffer[AssignmentStatement]()
//            bt.foreachStatements(btStatements += _)
//            btStatements.foreach {
//              case s@AssignmentStatement(_, x: BaseType) if !x.isAnalog => //analog driver
//                s.dlcRemove()
//                target.dlcAppend(s)
//                s.walkRemapExpressions(e => if (e == bt) target else e)
//              case s@AssignmentStatement(_, x: BaseType) if x.isAnalog && x.component.parent == bt.component => //analog connection
//                s.dlcRemove()
//                target.dlcAppend(s)
//                s.walkRemapExpressions(e => if (e == bt) target else e)
//              case _ =>
//            }
//
//            bt.removeAssignments()
//            bt.setAsComb()
//            bt.rootScopeStatement.push()
//            bt := target
//            bt.rootScopeStatement.pop()
//          }
//
//          //Convert target comb assignment into AnalogDriver nods
//          target.foreachStatements(s => {
//            s.source match {
//              case btSource: BaseType if btSource.isAnalog =>
//              case btSource =>
//                s.parentScope.push()
//                val enable = ConditionalContext.isTrue(target.rootScopeStatement)
//                s.parentScope.pop()
//                s.removeStatementFromScope()
//                target.rootScopeStatement.append(s)
//                val driver = btSource.getTypeObject match {
//                  case `TypeBool` => new AnalogDriverBool
//                  case `TypeBits` => new AnalogDriverBits
//                  case `TypeUInt` => new AnalogDriverUInt
//                  case `TypeSInt` => new AnalogDriverSInt
//                  case `TypeEnum` => new AnalogDriverEnum(btSource.asInstanceOf[EnumEncoded].getDefinition)
//                }
//                driver.data   = s.source.asInstanceOf[driver.T]
//                driver.enable = enable
//                s.source      = driver
//            }
//          })
//          //      }
//        })
//     */
//  }
//}


class MemTopology(val mem: Mem[_], val consumers : mutable.HashMap[Expression, ArrayBuffer[ExpressionContainer]] = mutable.HashMap[Expression, ArrayBuffer[ExpressionContainer]]()) {
  val writes                   = ArrayBuffer[MemWrite]()
  val readsAsync               = ArrayBuffer[MemReadAsync]()
  val readsSync                = ArrayBuffer[MemReadSync]()
  val readWriteSync            = ArrayBuffer[MemReadWrite]()
  val writeReadSameAddressSync = ArrayBuffer[(MemWrite, MemReadSync)]() //DISABLED

  var portCount = 0
  mem.foreachStatements(s => {
    portCount += 1
    s match {
      case p: MemWrite     => writes += p
      case p: MemReadAsync => readsAsync += p
      case p: MemReadSync  => readsSync += p
      case p: MemReadWrite => readWriteSync += p
    }
  })
}


trait PhaseMemBlackboxing extends PhaseNetlist {

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    val consumers = mutable.HashMap[Expression, ArrayBuffer[ExpressionContainer]]()
    val mems      = mutable.LinkedHashSet[Mem[_]]()

    walkBaseNodes{
      case mem: Mem[_] => mems += mem
      case ec: ExpressionContainer =>
        ec.foreachExpression{
          case port: MemPortStatement => consumers.getOrElseUpdate(port, ArrayBuffer[ExpressionContainer]()) += ec
          case _ =>
        }
      case _ =>
    }
    mems.foreach(mem => {
      if(mem.addressWidth != 0 && mem.width != 0) {
        doBlackboxing(pc, new MemTopology(mem, consumers))
      } else if(mem.width != 0){
        def wrapConsumers(oldSource: Expression, newSource: Expression): Unit ={
          consumers.get(oldSource) match {
            case None        =>
            case Some(array) => array.foreach(ec => {
              ec.remapExpressions{
                case e if e == oldSource => newSource
                case e                   => e
              }
            })
          }
        }

        mem.component.rework{
          val content = Bits(mem.width bits).allowOverride
          mem.foreachStatements{
            case port : MemWrite => {
              assert(port.aspectRatio == 1)
              val storage = port.clockDomain(Reg(Bits(mem.width bits)))
              storage.addTags(mem.getTags())
              if(mem.initialContent != null){
                assert(mem.initialContent.size == 1)
                storage.init(mem.initialContent.head)
              }
              content := storage
              when(port.writeEnable.asInstanceOf[Bool]){
                if(port.mask == null) {
                  storage := port.data.asInstanceOf[Bits]
                } else {
                  val dst = storage.subdivideIn(port.mask.getWidth slices)
                  val src = port.data.asInstanceOf[Bits].subdivideIn(port.mask.getWidth slices)
                  for(i <- 0 until port.mask.getWidth) {
                    when(port.mask.asInstanceOf[Bits](i)) {
                      dst(i) := src(i)
                    }
                  }
                }
              }
            }
            case port : MemReadAsync => {
              assert(port.aspectRatio == 1)
              assert(port.readUnderWrite == dontCare || port.readUnderWrite == writeFirst)
              val readValue = Bits(mem.width bits)
              readValue := content
              readValue.addTags(port.getTags())
              wrapConsumers(port, readValue)
            }
            case port : MemReadSync => {
              assert(port.aspectRatio == 1)
              assert(port.readUnderWrite == dontCare || port.readUnderWrite == readFirst)
              val buffer = Reg(Bits(mem.width bits))
              buffer.addTags(port.getTags())
              when(port.readEnable.asInstanceOf[Bool]){
                buffer := content
              }
              wrapConsumers(port, buffer)
            }
            case port : MemReadWrite => {
              assert(port.aspectRatio == 1)
              val storage = port.clockDomain(Reg(Bits(mem.width bits)))
              storage.addTags(mem.getTags())
              if(mem.initialContent != null){
                assert(mem.initialContent.size == 1)
                storage.init(mem.initialContent.head)
              }
              content := storage
              val buffer = Reg(Bits(mem.width bits))
              buffer.addTags(port.getTags())
              when(port.chipSelect.asInstanceOf[Bool]){
                when(port.writeEnable.asInstanceOf[Bool]){
                  if(port.mask == null) {
                    storage := port.data.asInstanceOf[Bits]
                  } else {
                    val dst = storage.subdivideIn(port.mask.getWidth slices)
                    val src = port.data.asInstanceOf[Bits].subdivideIn(port.mask.getWidth slices)
                    for(i <- 0 until port.mask.getWidth) {
                      when(port.mask.asInstanceOf[Bits](i)) {
                        dst(i) := src(i)
                      }
                    }
                  }
                } otherwise {
                  buffer := content
                }
              }
              wrapConsumers(port, buffer)
            }
          }

          if(mem.initialContent != null && content.dlcIsEmpty){
            assert(mem.initialContent.size == 1)
            content := mem.initialContent.head
          }
        }
        mem.removeStatement()
        mem.foreachStatements(s => s.removeStatement())
      }
    })
  }

  def doBlackboxing(pc: PhaseContext, typo: MemTopology): Unit

  def wrapConsumers(topo : MemTopology, oldSource: Expression, newSource: Expression): Unit ={
    topo.consumers.get(oldSource) match {
      case None        =>
      case Some(array) => array.foreach(ec => {
        ec.remapExpressions{
          case e if e == oldSource => newSource
          case e                   => e
        }
      })
    }
  }

  def removeMem(mem : Mem[_]): Unit ={
    mem.removeStatement()
    mem.foreachStatements(s => s.removeStatement())
  }


}


abstract class PhaseMemBlackBoxingWithPolicy(policy: MemBlackboxingPolicy) extends PhaseMemBlackboxing{

  override def doBlackboxing(pc: PhaseContext, typo: MemTopology): Unit = {

    if(policy.translationInterest(typo)) {
      val message = doBlackboxing(typo)
      if(message != null) policy.onUnblackboxable(typo,this,message)
    }
  }

  //Return null if success
  def doBlackboxing(memTopology: MemTopology) : String
}

class MemBlackboxOf(val mem : Mem[Data]) extends SpinalTag
class PhaseMemBlackBoxingDefault(policy: MemBlackboxingPolicy) extends PhaseMemBlackBoxingWithPolicy(policy){
  def doBlackboxing(topo: MemTopology): String = {
    val mem = topo.mem
    def wrapBool(that: Expression): Bool = that match {
      case that: Bool => that
      case that       =>
        val ret = Bool()
        ret.assignFrom(that)
        ret
    }

    def wrapConsumers(oldSource: Expression, newSource: Expression): Unit ={
      super.wrapConsumers(topo, oldSource, newSource)
    }

    def removeMem(): Unit ={
      super.removeMem(mem)
    }

    if (mem.initialContent != null) {
      return "Can't blackbox ROM"  //TODO
      //      } else if (topo.writes.size == 1 && topo.readsAsync.size == 1 && topo.portCount == 2) {
    } else if (topo.writes.size == 1 && (topo.readsAsync.nonEmpty || topo.readsSync.nonEmpty) && topo.writeReadSameAddressSync.isEmpty && topo.readWriteSync.isEmpty) {
      mem.component.rework {
        val wr = topo.writes(0)
        for (rd <- topo.readsAsync) {
          val clockDomain = wr.clockDomain
          val ctx = ClockDomainStack.set(clockDomain)

          val ram = new Ram_1w_1ra(
            wordWidth = mem.getWidth,
            wordCount = mem.wordCount,
            wrAddressWidth = wr.getAddressWidth,
            wrDataWidth = wr.data.getWidth,
            rdAddressWidth = rd.getAddressWidth,
            rdDataWidth = rd.getWidth,
            wrMaskWidth = if (wr.mask != null) wr.mask.getWidth else 1,
            wrMaskEnable = wr.mask != null,
            readUnderWrite = rd.readUnderWrite,
            technology = mem.technology
          )

          ram.addTag(new MemBlackboxOf(topo.mem.asInstanceOf[Mem[Data]]))

          ram.io.wr.en := wrapBool(wr.writeEnable) && clockDomain.isClockEnableActive
          ram.io.wr.addr.assignFrom(wr.address)
          ram.io.wr.data.assignFrom(wr.data)

          if (wr.mask != null)
            ram.io.wr.mask.assignFrom(wr.mask)
          else
            ram.io.wr.mask := B"1"

          ram.io.rd.addr.assignFrom(rd.address)
          wrapConsumers(rd, ram.io.rd.data)

          ram.setName(mem.getName())
          ctx.restore()
        }

        for (rd <- topo.readsSync) {
          val ram = new Ram_1w_1rs(
            wordWidth = mem.getWidth,
            wordCount = mem.wordCount,
            wrClock = wr.clockDomain,
            rdClock = rd.clockDomain,
            wrAddressWidth = wr.getAddressWidth,
            wrDataWidth = wr.data.getWidth,
            rdAddressWidth = rd.getAddressWidth,
            rdDataWidth = rd.getWidth,
            wrMaskWidth = if (wr.mask != null) wr.mask.getWidth else 1,
            wrMaskEnable = wr.mask != null,
            readUnderWrite = rd.readUnderWrite,
            technology = mem.technology
          )
          ram.addTag(new MemBlackboxOf(topo.mem.asInstanceOf[Mem[Data]]))

          ram.io.wr.en := wrapBool(wr.writeEnable) && wr.clockDomain.isClockEnableActive
          ram.io.wr.addr.assignFrom(wr.address)
          ram.io.wr.data.assignFrom(wr.data)

          if (wr.mask != null)
            ram.io.wr.mask.assignFrom(wr.mask)
          else
            ram.io.wr.mask := B"1"

          ram.io.rd.en := wrapBool(rd.readEnable) && rd.clockDomain.isClockEnableActive
          ram.io.rd.addr.assignFrom(rd.address)
          wrapConsumers(rd, ram.io.rd.data)

          ram.setName(mem.getName())
        }

        removeMem()
      }
    } else if (topo.portCount == 1 && topo.readWriteSync.size == 1) {

      mem.component.rework {
        val port = topo.readWriteSync.head

        val ram = port.clockDomain on new Ram_1wrs(
          wordWidth = port.width,
          wordCount = mem.wordCount*mem.width/port.width,
          technology = mem.technology,
          readUnderWrite = port.readUnderWrite,
          duringWrite = port.duringWrite,
          maskWidth = if (port.mask != null) port.mask.getWidth else 1,
          maskEnable = port.mask != null
        )

        ram.addTag(new MemBlackboxOf(topo.mem.asInstanceOf[Mem[Data]]))

        ram.io.addr.assignFrom(port.address)
        ram.io.en.assignFrom(wrapBool(port.chipSelect) && port.clockDomain.isClockEnableActive)
        ram.io.wr.assignFrom(port.writeEnable)
        ram.io.wrData.assignFrom(port.data)

        if (port.mask != null)
          ram.io.mask.assignFrom(port.mask)
        else
          ram.io.mask := B"1"

        wrapConsumers(port, ram.io.rdData)

        ram.setName(mem.getName())
        removeMem()
      }
    } else if (topo.portCount == 2 && topo.readWriteSync.size == 2) {

      mem.component.rework {
        val portA = topo.readWriteSync(0)
        val portB = topo.readWriteSync(1)

        val ram = new Ram_2wrs(
          wordWidth = mem.getWidth,
          wordCount = mem.wordCount,
          technology = mem.technology,
          portA_readUnderWrite = portA.readUnderWrite,
          portA_duringWrite = portA.duringWrite,
          portA_clock = portA.clockDomain,
          portA_addressWidth = portA.getAddressWidth,
          portA_dataWidth = portA.getWidth,
          portA_maskWidth = if (portA.mask != null) portA.mask.getWidth else 1,
          portA_maskEnable = portA.mask != null,
          portB_readUnderWrite = portB.readUnderWrite,
          portB_duringWrite = portB.duringWrite,
          portB_clock = portB.clockDomain,
          portB_addressWidth = portB.getAddressWidth,
          portB_dataWidth = portB.getWidth,
          portB_maskWidth = if (portB.mask != null) portB.mask.getWidth else 1,
          portB_maskEnable = portB.mask != null
        )

        ram.addTag(new MemBlackboxOf(topo.mem.asInstanceOf[Mem[Data]]))

        ram.io.portA.addr.assignFrom(portA.address)
        ram.io.portA.en.assignFrom(wrapBool(portA.chipSelect) && portA.clockDomain.isClockEnableActive)
        ram.io.portA.wr.assignFrom(portA.writeEnable)
        ram.io.portA.wrData.assignFrom(portA.data)
        ram.io.portA.mask.assignFrom((if (portA.mask != null) portA.mask else B"1"))
        wrapConsumers(portA, ram.io.portA.rdData)

        ram.io.portB.addr.assignFrom(portB.address)
        ram.io.portB.en.assignFrom(wrapBool(portB.chipSelect) && portB.clockDomain.isClockEnableActive)
        ram.io.portB.wr.assignFrom(portB.writeEnable)
        ram.io.portB.wrData.assignFrom(portB.data)
        ram.io.portB.mask.assignFrom((if (portB.mask != null) portB.mask else B"1"))
        wrapConsumers(portB, ram.io.portB.rdData)

        ram.setName(mem.getName())
        removeMem()
      }
    } else {
      return "Unblackboxable memory topology" //TODO
    }
    return null
  }
}

object classNameOf{
  def apply(that : Any): String = {
    val name = that.getClass.getSimpleName.replace("$",".").split("\\.").head
    if(name.nonEmpty) name else "unamed"
  }
}

class PhaseNameNodesByReflection(pc: PhaseContext) extends PhaseMisc{
  override def impl(pc : PhaseContext): Unit = {
    import pc._

    globalData.nodeAreNamed = true

    if (topLevel.getName() == null) topLevel.setName("toplevel", Nameable.DATAMODEL_WEAK)
    if(topLevel.definitionName == null) {
      topLevel.definitionName = pc.config.globalPrefix + classNameOf(topLevel)
    }
    for (c <- sortedComponents) {
      if(c != topLevel) {
        val pre = c match {
            case t: BlackBox => ""
            case _ => config.globalPrefix
          }
        if (c.definitionName == null) {
          val privateNsN = (if(config.privateNamespace) topLevel.definitionName + "_" else "")
          c.definitionName = pre + privateNsN + classNameOf(c)
        } else {
          c.definitionName = pre + c.definitionName
        }
      }
      if (c.definitionName == "") {
        c.definitionName = "unnamed"
      }
      c match {
        case bb: BlackBox if bb.isBlackBox => {
          val generic = bb.getGeneric
          if(generic != null) {
            Misc.reflect(generic, (name, obj) => {
              OwnableRef.proposal(obj, this)
              obj match {
                case obj: Nameable => obj.setName(name, Nameable.DATAMODEL_WEAK)
                case _ =>
              }
              obj match {
                case obj: Data => bb.genericElements ++= obj.flatten.map(o => {
                  o.addTag(GenericValue(o.head.source))
                  (o.getName(), o)
                })
                case _         => bb.genericElements += Tuple2(name, obj.asInstanceOf[Any])
              }
            })
          }
        }
        case _ =>
      }
    }
  }
}


class PhaseCollectAndNameEnum(pc: PhaseContext) extends PhaseMisc{

  override def impl(pc : PhaseContext): Unit = {
    import pc._

    //Collect all SpinalEnum
    walkDeclarations {
      case senum: SpinalEnumCraft[_] => enums.getOrElseUpdate(senum.spinalEnum, null) //Encodings will be added later
      case _ =>
    }

    //Provide a basic name for each of them (not unique)
    enums.keys.foreach(e => {
      val name = if(e.isNamed)
        e.getName()
      else
        e.getClass.getSimpleName.replace("$","")

      e.setName(name)
    })

    for (enumDef <- enums.keys) {
      Misc.reflect(enumDef, (name, obj) => {
        obj match {
          case obj: Nameable => obj.setName(name, Nameable.DATAMODEL_WEAK)
          case _ =>
        }
      })

      for (e <- enumDef.elements) {
        if (e.isUnnamed) {
          e.setName("e" + e.position, Nameable.DATAMODEL_WEAK)
        }
      }
    }

    //Identify similar enums in order to merge them
    val enumSet = mutable.LinkedHashSet[SpinalEnum]()
    walkDeclarations {
      case senum: SpinalEnumCraft[_] => enumSet += senum.spinalEnum
      case _ =>
    }
    val signatureToEnums = mutable.LinkedHashMap[Any, ArrayBuffer[SpinalEnum]]()
    for(e <- enumSet){
      signatureToEnums.getOrElseUpdate(e.getSignature(), ArrayBuffer[SpinalEnum]()) += e
    }

    val enumsToMerge =  mutable.LinkedHashMap[SpinalEnum, SpinalEnum]()
    for((_, list) <- signatureToEnums) {
      val target = list.head
      for(tail <- list.tail){
        enumsToMerge(tail) = target
        enums -= tail
      }
    }

    //Merge similar enums
    walkExpression {
      case e : EnumEncoded => enumsToMerge.get(e.getDefinition).foreach(e.swapEnum)
      case _ =>
    }

    //Provide unique name for all remaining enums
    val scope = pc.globalScope.newChild("")
    enums.keys.foreach(e => {
      e.setName(scope.allocateName(e.getName()))
    })
  }
}


object PhasePullClockDomains{
  def single(c : Component): Unit ={
    val cds = mutable.LinkedHashSet[ClockDomain]()
    c.dslBody.walkLeafStatements{
      case bt : BaseType if bt.isReg =>
        val cd = bt.clockDomain
        if(bt.isUsingResetSignal && (!cd.hasResetSignal && !cd.hasSoftResetSignal))
          SpinalError(s"MISSING RESET SIGNAL in the ClockDomain used by $bt\n${bt.getScalaLocationLong}")

        cds += cd
      case ls => ls.foreachClockDomain(cd => cds += cd)
    }

    c.rework{
      for(cd <- cds){
        cd.readClockWire
        if(cd.hasResetSignal)       cd.readResetWire
        if(cd.hasSoftResetSignal)   cd.readSoftResetWire
        if(cd.hasClockEnableSignal) cd.readClockEnableWire
      }
    }
  }

  def recursive(c : Component) : Unit = {
    single(c)
    c.children.foreach(recursive)
  }
}

class PhasePullClockDomains(pc: PhaseContext) extends PhaseNetlist{
  override def impl(pc : PhaseContext): Unit = {
    import pc._
    walkComponents(c => PhasePullClockDomains.single(c))
  }
}


class PhaseInferEnumEncodings(pc: PhaseContext, encodingSwap: (SpinalEnumEncoding) => SpinalEnumEncoding) extends PhaseMisc{

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    globalData.nodeAreInferringEnumEncoding = true

    val nodes           = ArrayBuffer[Expression with EnumEncoded]()
    val nodesInferrable = ArrayBuffer[Expression with InferableEnumEncoding]()
    val consumers       = mutable.HashMap[Expression , ArrayBuffer[Expression]]()
    var algo            = globalData.allocateAlgoIncrementale()

    def walkExpression(node: Expression): Unit ={
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
    walkStatements {
      case s: AssignmentStatement =>
        val finalTarget = s.finalTarget
        s.source match {
          case source: Expression with EnumEncoded => consumers.getOrElseUpdate(source, ArrayBuffer[Expression]()) += finalTarget
          case _ =>
        }
        s.foreachDrivingExpression(e => walkExpression(e))
      case s: SwitchStatement if s.value.getTypeObject == TypeEnum =>
        s.elements.foreach(_.keys.foreach {
          case key if key.getTypeObject == TypeEnum => consumers.getOrElseUpdate(s.value, ArrayBuffer[Expression]()) += key
        })
        s.foreachDrivingExpression(e => walkExpression(e))
      case s =>
        s.foreachDrivingExpression(e => walkExpression(e))
    }

    walkDeclarations{
      case e: Expression => walkExpression(e)
      case _             =>
    }

    //Prepear the feild
    nodesInferrable.foreach(node => {
      node.bootInferration()
    })

    nodes.foreach(senum => {
      senum.swapEncoding(encodingSwap(senum.getEncoding))
    })

    algo = globalData.allocateAlgoIncrementale()

    nodes.foreach(senum => {
      if(senum.propagateEncoding){

        def propagateOn(that : Expression): Unit = {
          that match {
            case that: InferableEnumEncoding =>
              if(that.algoIncrementale == algo) return

              that.algoIncrementale = algo

              if(that.encodingProposal(senum.getEncoding)) {
                that match {
                  case that: SpinalEnumCraft[_] =>
                    that.dlcForeach(s => propagateOn(s.source))
                    consumers.getOrElse(that, Nil).foreach(propagateOn(_))
                  case _ =>
                    that.foreachExpression(propagateOn(_))
                    consumers.getOrElse(that, Nil).foreach(propagateOn(_))
                }
              }
            case _ =>
          }
        }

        senum match {
          case senum : SpinalEnumCraft[_] =>
            senum.dlcForeach(s => propagateOn(s.source))
            consumers.getOrElse(senum, Nil).foreach(propagateOn(_))
          case _ =>
            senum.foreachExpression(propagateOn(_))
            consumers.getOrElse(senum, Nil).foreach(propagateOn(_))
        }
      }
    })

    //Feed enums with encodings
    enums.keys.toArray.distinct.foreach(enums(_) = mutable.LinkedHashSet[SpinalEnumEncoding]())
    nodes.foreach(senum => {
      enums(senum.getDefinition) += senum.getEncoding
    })

    //give a name to unnamed encodingss
    val unnamedEncodings = enums.valuesIterator.flatten.toSeq.distinct.withFilter(_.isUnnamed).foreach(_.setName("anonymousEnc", Nameable.DATAMODEL_WEAK))

    //Check that there is no encoding overlaping
    for((senum,encodings) <- enums){
      for(encoding <- encodings) {
        val reserveds = mutable.Map[BigInt, ArrayBuffer[SpinalEnumElement[_]]]()

        for(element <- senum.elements){
          val key = encoding.getValue(element)
          reserveds.getOrElseUpdate(key,ArrayBuffer[SpinalEnumElement[_]]()) += element
        }

        for((key,elements) <- reserveds){
          if(elements.length != 1){
            PendingError(s"Conflict in the $senum enumeration with the '$encoding' encoding with the key $key' and following elements:.\n${elements.mkString(", ")}\n\nEnumeration defined at :\n${senum.getScalaLocationLong}Encoding defined at :\n${encoding.getScalaLocationLong}")
          }
        }
      }
    }
  }
}

class PhaseDevice(pc : PhaseContext) extends PhaseMisc{
  override def impl(pc: PhaseContext): Unit = {
    if(pc.config.device.isVendorDefault || pc.config.device.vendor == Device.XILINX.vendor) {
      pc.walkDeclarations {
        case mem: Mem[_] => {
          var hit, withWrite = false
          mem.foreachStatements {
            case port: MemReadAsync => hit = true
            case port: MemWrite => withWrite = true
            case port: MemReadWrite => withWrite = true
            case port: MemReadSync =>
          }
          val alreadyTagged = mem.getTags().exists{
            case a : AttributeString if a.getName == "ram_style" => true
            case _ => false
          }
          if (hit && withWrite && !alreadyTagged) mem.addAttribute("ram_style", "distributed") //Vivado stupid gambling workaround Synth 8-6430
        }
        case bt: BaseType => {
          if (bt.isReg && (bt.hasTag(crossClockDomain) || bt.hasTag(crossClockBuffer))) {
            bt.addAttribute("async_reg", "true")
          }
        }
        case _ =>
      }
    }
    if(pc.config.device.vendor == Device.ALTERA.vendor){
      pc.walkDeclarations {
        case mem : Mem[_] => {
          var onlyDontCare = true
          mem.dlcForeach(e => e match {
            case port: MemWrite      =>
            case port: MemReadWrite  => onlyDontCare &= port.readUnderWrite == dontCare
            case port: MemReadSync   => onlyDontCare &= port.readUnderWrite == dontCare
            case port: MemReadAsync  => onlyDontCare &= port.readUnderWrite == dontCare
          })
          if(onlyDontCare) mem.addAttribute("ramstyle", "no_rw_check")
        }
        case _ =>
      }
    }
  }
}

class PhaseInferWidth(pc: PhaseContext) extends PhaseMisc{

  override def impl(pc: PhaseContext): Unit = {
    import pc._
    globalData.nodeAreInferringWidth = true

    var iterationCounter = 0

    while (true) {
      var somethingChange = false

      //Infer width on all expressions
      //Use post-order traversal so that a parent node can get the widths of its children before inferring width,
      // which could help reducing the number of iterations
      walkExpressionPostorder {
        case e: DeclarationStatement =>
        case e: Widthable =>
          val hasChange = e.inferWidth
          somethingChange = somethingChange || hasChange
        case _ =>
      }

      //Infer width on all nameable expression (BitVector)
      walkDeclarations {
        case e: Widthable =>
          val hasChange = e.inferWidth
          somethingChange = somethingChange || hasChange
        case _ =>
      }

      //Check in the width inferation is done, then check it and generate errors
      if (!somethingChange || iterationCounter == 10000) {
        val errors = mutable.ArrayBuffer[String]()

        def widthableCheck(e: Widthable): Unit = {
          if (e.inferWidth) {
            //Don't care about Reg width inference
            errors += s"Can't infer width on ${e.getScalaLocationLong}"
          }

          if (e.widthWhenNotInferred != -1 &&
            e.widthWhenNotInferred != e.getWidth) {
            errors += s"getWidth call result during elaboration differ from inferred width on\n${e.getScalaLocationLong}"
          }

          if(e.inferredWidth < 0){
            errors += s"Negative width on $e at ${e.getScalaLocationLong}"
          }

          if (e.inferredWidth > pc.config.bitVectorWidthMax) {
            errors += s"Way too big signal $e at ${e.getScalaLocationLong}"
          }
        }

        walkExpression {
          case e: DeclarationStatement =>
          case e: Widthable => widthableCheck(e)
          case e: WidthProvider =>
            if (e.getWidth < 0) {
              errors += s"Negative width on $e at ${e.getScalaLocationLong}"
            }
            if (e.getWidth > pc.config.bitVectorWidthMax) {
              errors += s"Way too big signal $e at ${e.getScalaLocationLong}"
            }
          case _ =>
        }
        walkDeclarations {
          case e: Widthable => {
            if(e.getWidth == 0 && e.isNamed) globalData.zeroWidths += (e.component -> e)
            widthableCheck(e)
          }
          case _ =>
        }

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
      case s: BitVector if s.getWidth == 0 =>
        s.foreachStatements(toRemove += _)
        s.removeStatement()
      case s: Mem[_] if s.getWidth == 0 =>
        s.foreachStatements(toRemove += _)
        s.removeStatement()
      case s: SpinalEnumCraft[_] if s.spinalEnum.elements.size < 2 =>
        s.foreachStatements(toRemove += _)
        s.removeStatement()
      case s => s.walkRemapExpressions(_.simplifyNode)
    }

    toRemove.foreach(_.removeStatement())


    //propagate literals over one basetype
    walkStatements{s =>
      s.remapDrivingExpressions{
        case e : BaseType if e.isComb && !e.isNamed && e.isDirectionLess && Statement.isSomethingToFullStatement(e) => e.head match {
          case DataAssignmentStatement(_, lit : Literal) => lit.clone //clone because Expression can only be once in the graph
          case _ => e
        }
        case e => e
      }
    }
  }
}


class PhaseNormalizeNodeInputs(pc: PhaseContext) extends PhaseNetlist{

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    walkStatements(s => {
      s.walkExpression(e => {
        e.normalizeInputs
      })
      s.normalizeInputs
    })

    walkComponents(c => {
      c.dslBody.walkDeclarations {
        case n: BitVector => n.removeTag(tagAutoResize)
        case _ =>
      }
    })
  }
}


class PhaseCheckCombinationalLoops() extends PhaseCheck{

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    val walkingId = GlobalData.get.allocateAlgoIncrementale()
    val okId      = GlobalData.get.allocateAlgoIncrementale()

    def walk(path : List[(BaseNode)], node: BaseNode): Unit = {
      val newPath = node :: path

      if (node.algoIncrementale == walkingId) {
        val ordred  = newPath.reverseIterator
        val filtred = ordred.dropWhile((e) => (e != node)).drop(1).toArray

        if (!filtred.exists(e => e.isInstanceOf[SpinalTagReady] && e.asInstanceOf[SpinalTagReady].hasTag(noCombinatorialLoopCheck))) {
          val wellNameLoop = new StringBuilder()

          for(n <- filtred.reverseIterator) n match{
            case n: DeclarationStatement => wellNameLoop ++= s"    >>> ${n.toString()} at ${n.getScalaLocationShort} >>>\n"
            case _                       =>
          }

          val multiLineLoop = filtred.reverseIterator.filter(!_.isInstanceOf[AssignmentStatement]).map(n => "    " + n.toString).foldLeft("")(_ + "\n" + _)
          PendingError(s"COMBINATORIAL LOOP :\n  Partial chain :\n${wellNameLoop}\n  Full chain :${multiLineLoop}")
        }

      }else if (node.algoIncrementale != okId) {
        node.algoIncrementale = walkingId
        node match {
          case node: BaseType =>
            if(node.isComb) {
              node.algoIncrementale = walkingId
              node.foreachStatements(s => walk(newPath, s))
              node.algoIncrementale = okId
            }
          case node: AssignmentStatement =>
            node.foreachDrivingExpression(e => walk(newPath, e))
            node.walkParentTreeStatementsUntilRootScope(s => walk(newPath, s))
          case node: TreeStatement =>
            if (node.algoIncrementale != okId) {
              node.foreachDrivingExpression(e => walk(newPath, e))
              node.algoIncrementale = okId
            }
          case node: Mem[_]       =>
          case node: MemReadSync  =>
          case node: MemReadWrite =>
          case node: MemWrite     =>
          case node: Expression   =>
            node.foreachDrivingExpression(e => walk(newPath, e))
          case node: AssertStatement =>
            node.foreachDrivingExpression(e => walk(newPath, e))
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

    val solved = mutable.HashMap[Bool, immutable.Set[Bool]]()
    def areSynchronous(a : ClockDomain, b : ClockDomain): Boolean ={
      ClockDomain.areSynchronous(a,b,solved)
    }



    walkStatements(s => {
      var walked = 0

      def issueRaw(syncDriver: BaseNode with ScalaLocated, otherClockDomain: ClockDomain, path : List[(BaseNode)], dstCd : ClockDomain): Unit = {
        val wellNameLoop = new StringBuilder()

        for(n <- path) n match{
          case n: DeclarationStatement =>
            wellNameLoop ++= s"      >>> ${n.toString()} at ${n.getScalaLocationShort} >>>\n"
          case _  =>
        }
        val multiLineLoop = path.map(n => "      " + n.toString).foldLeft("")(_ + "\n" + _)

        PendingError(
          s"""CLOCK CROSSING VIOLATION :
             |- Source            : ${syncDriver} ${syncDriver.getScalaLocationShort}
             |- Source clock      : ${otherClockDomain.clock}
             |- Destination       : ${s} ${s.getScalaLocationShort}
             |- Destination clock : ${dstCd.clock}
             |- Source declaration :
             |${syncDriver.getScalaLocationLong}
             |- Destination declaration :
             |${s.getScalaLocationLong}
             |- Connection path :
             |${wellNameLoop}
             """.stripMargin
        )
      }

      def checkMem(mem : Mem[_], path: List[(BaseNode)], targetCd : ClockDomain) : Unit ={
        val newPath = mem :: path
        mem.foreachStatements{
          case s : MemReadSync =>
          case s : MemReadAsync =>
          case s : MemWrite => if(!areSynchronous(s.clockDomain, targetCd)) issueRaw(s, s.clockDomain, newPath, targetCd)
          case s : MemReadWrite => if(!areSynchronous(s.clockDomain, targetCd)) issueRaw(s, s.clockDomain, newPath, targetCd)
        }
      }


      def walk(node: BaseNode, path: List[(BaseNode)], clockDomain: ClockDomain): Unit = {
        if(node.algoIncrementale == walked) return

        node.algoIncrementale = walked

        val newPath = node :: path

        def issue(syncDriver: BaseNode with ScalaLocated, otherClockDomain: ClockDomain): Unit = {
          issueRaw(syncDriver, otherClockDomain, newPath, clockDomain)
        }

        //Add tag to the toplevel inputs and blackbox inputs as a report
        node match {
          case bt : BaseType if bt.component == topLevel || bt.component.isInBlackBoxTree && !bt.isDirectionLess=> bt.addTag(ClockDomainReportTag(clockDomain))
          case _ =>
        }

        node match {
          case node: SpinalTagReady if node.hasTag(crossClockDomain) =>
          case node: SpinalTagReady if node.hasTag(classOf[ClockDomainTag]) =>
            if(!areSynchronous(node.getTag(classOf[ClockDomainTag]).get.clockDomain, clockDomain)) {
              issue(node.asInstanceOf[BaseNode with ScalaLocated], node.getTag(classOf[ClockDomainTag]).get.clockDomain)
            }
          case node: BaseType =>
            if (node.isReg) {
              if(!areSynchronous(node.clockDomain, clockDomain)) {
                issue(node, node.clockDomain)
              }
            } else {
              node.foreachStatements(s => walk(s, newPath, clockDomain))
            }
          case node: AssignmentStatement =>
            node.foreachDrivingExpression(e => walk(e, newPath, clockDomain))
            node.walkParentTreeStatementsUntilRootScope(s => walk(s, newPath, clockDomain))
          case node: TreeStatement =>
            node.foreachDrivingExpression(e => walk(e, newPath, clockDomain))
          case node: Mem[_] => {
            ???
          }
          case node: MemReadAsync => {
            checkMem(node.mem, newPath.tail, clockDomain)
            node.foreachDrivingExpression(e => walk(e, newPath, clockDomain))
          }
          case node: MemReadSync =>
            if(!areSynchronous(node.clockDomain, clockDomain)) {
              issue(node, node.clockDomain)
            }
          case node: MemReadWrite =>
            if(!areSynchronous(node.clockDomain, clockDomain)) {
              issue(node, node.clockDomain)
            }
          case node: Expression =>
            node.foreachDrivingExpression(e => walk(e, newPath, clockDomain))
        }
      }



      s match {
        case s: BaseType if s.hasTag(classOf[ClockDomainTag]) =>
          if (!s.isReg) {
            // if it not a reg, perform the check if the ClockDomainTag is present
            walked = GlobalData.get.allocateAlgoIncrementale()
            s.foreachStatements(as => walk(as, as :: s :: Nil, s.getTag(classOf[ClockDomainTag]).get.clockDomain))
          }
          else {
            PendingError(s"Can't add ClockDomainTag to registers:\n" + s.getScalaLocationLong)
          }
        case s: BaseType if s.isReg && !s.hasTag(crossClockDomain) =>
          walked = GlobalData.get.allocateAlgoIncrementale()
          s.foreachStatements(as => walk(as, as :: s :: Nil, s.clockDomain))
        case s: MemReadSync if !s.hasTag(crossClockDomain) =>
          if (s.hasTag(classOf[ClockDomainTag])) {
            PendingError(s"Can't add ClockDomainTag to memory ports:\n" + s.getScalaLocationLong)
          }
          walked = GlobalData.get.allocateAlgoIncrementale()
          s.foreachDrivingExpression(as => walk(as, as :: s :: Nil, s.clockDomain))
          checkMem(s.mem, s :: Nil, s.clockDomain)
        case s: MemReadWrite if !s.hasTag(crossClockDomain) =>
          if (s.hasTag(classOf[ClockDomainTag])) {
            PendingError(s"Can't add ClockDomainTag to memory ports:\n" + s.getScalaLocationLong)
          }
          walked = GlobalData.get.allocateAlgoIncrementale()
          s.foreachDrivingExpression(as => walk(as, as :: s :: Nil, s.clockDomain))
          checkMem(s.mem, s :: Nil, s.clockDomain)
        case s: MemWrite if !s.hasTag(crossClockDomain) =>
          if (s.hasTag(classOf[ClockDomainTag])) {
            PendingError(s"Can't add ClockDomainTag to memory ports:\n" + s.getScalaLocationLong)
          }
          walked = GlobalData.get.allocateAlgoIncrementale()
          s.foreachDrivingExpression(as => walk(as, as :: s :: Nil, s.clockDomain))
        case _ =>
      }
    })
  }
}

class PhaseNextifyTag(val dest : BaseType) extends SpinalTag
class PhaseNextifyReg() extends PhaseNetlist{
  override def impl(pc: PhaseContext) = {
    pc.walkDeclarations{
      case bt : BaseType if bt.hasTag(classOf[PhaseNextifyTag])=> {
        bt.isReg match {
          case false => ???
          case true => {
            val dests = bt.getTags().collect{ case e : PhaseNextifyTag  => e.dest }
            val seed = dests.head //Used to implement the ff input logic

            dests.foreach(_.unfreeze())

            for(other <- dests.filter(_ != seed)) {
              other.component.rework{other := seed.pull()} //pull to ensure it work with in/out
            }

            seed.allowOverride()
            bt.parentScope.onHead(seed := bt) //Default value

            bt.foreachStatements{
              case s : InitAssignmentStatement =>
              case s : DataAssignmentStatement => {
                //Move the assignment to the seed
                s.dlcRemove()
                s.target match {
                  case t : BaseType => s.target = seed
                  case t : RangedAssignmentFixed => t.out = seed.asInstanceOf[BitVector]
                  case t : RangedAssignmentFloating => t.out = seed.asInstanceOf[BitVector]
                  case t : BitAssignmentFixed => t.out = seed.asInstanceOf[BitVector]
                  case t : BitAssignmentFloating => t.out = seed.asInstanceOf[BitVector]
                }
                seed.dlcAppend(s)
              }
            }

            bt.component.rework(bt := seed)
          }
        }
      }
      case _ =>
    }
  }
}

class PhaseRemoveUselessStuff(postClockPulling: Boolean, tagVitals: Boolean) extends PhaseNetlist {

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    val okId = globalData.allocateAlgoIncrementale()

    def propagate(root: Statement, vital: Boolean): Unit = {
      if(root.algoIncrementale == okId) return

      root.algoIncrementale = okId

      val pending = mutable.ArrayStack[Statement](root)

      def propagate(s: Statement) = {
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
            def propCached(that : BaseType): Unit ={
              s.component.pulledDataCache.get(that) match {
                case Some(x) => propagate(x.asInstanceOf[BaseType])
                case None =>    propagate(that)
              }
            }
            if(cd.hasResetSignal) propCached(cd.reset)
            if(cd.hasSoftResetSignal) propCached(cd.softReset)
            if(cd.hasClockEnableSignal) propCached(cd.clockEnable)
          })
        }

        s match {
          case s: BaseType =>
            if(vital)
              s.setAsVital()
            s.foreachStatements(propagate)
          case s: AssignmentStatement =>
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
            s.walkParentTreeStatements(propagate) //Could be walkParentTreeStatementsUntilRootScope but then should symplify removed TreeStatements
          case s: WhenStatement =>
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
          case s: SwitchStatement =>
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
          case s: AssertStatement =>
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
            s.walkParentTreeStatements(propagate)
          case s: Mem[_] => s.foreachStatements{
            case p: MemWrite     => propagate(p)
            case p: MemReadWrite => propagate(p)
            case p: MemReadSync  =>
            case p: MemReadAsync =>
          }
          case s: MemWrite =>
            s.isVital |= vital
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
          case s: MemReadWrite =>
            s.isVital |= vital
            propagate(s.mem)
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
          case s: MemReadSync =>
            s.isVital |= vital
            propagate(s.mem)
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
          case s: MemReadAsync =>
            s.isVital |= vital
            propagate(s.mem)
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
        }
      }
    }

    //Propagate all vital signals (drive toplevel output and blackboxes inputs)
    topLevel.getAllIo.withFilter(bt => !bt.isDirectionLess).foreach(propagate(_, tagVitals))
    walkComponents{
      case c: BlackBox if c.isBlackBox => c.getAllIo.withFilter(_.isInputOrInOut).foreach(propagate(_, tagVitals))
      case c if c.withVitalOutputs => c.getAllIo.withFilter(bt => bt.isOutputOrInOut).foreach(propagate(_, tagVitals))
      case c =>
    }

    val keepNamed = !pc.config.removePruned
    walkStatements{
      case s: BaseType => if((keepNamed || s.dontSimplify) && s.isNamed && (s.namePriority >= Nameable.USER_WEAK || s.isVital)) propagate(s, false)
      case s: DeclarationStatement => if(keepNamed && s.isNamed) propagate(s, false)
      case s: AssertStatement      => if(s.kind == AssertStatementKind.ASSERT || pc.config.isSystemVerilog) propagate(s, false)
      case s: TreeStatement        =>
      case s: AssignmentStatement  =>
      case s: MemWrite             =>
      case s: MemReadWrite         =>
      case s: MemReadSync          =>
      case s: MemReadAsync         =>
    }

    walkStatements(s => {
      if(s.algoIncrementale != okId){
        s.removeStatement()
      }
    })

    def removeEmptyChilds(c: Component): Unit ={
      val keep = ArrayBuffer[Component]()
      c.children.foreach { child =>
        removeEmptyChilds(child)
        if(!child.isLogicLess) keep += child
      }
      c.children.clear()
      c.children ++= keep
    }

    if(!keepNamed) removeEmptyChilds(topLevel)
  }
}


class PhaseRemoveIntermediateUnnameds(onlyTypeNode: Boolean) extends PhaseNetlist{

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    val koId = globalData.allocateAlgoIncrementale()

    walkDeclarations(e => e.algoInt = 0)

    //Count the number of driving reference done on each ref.source
    walkDrivingExpression {
      case ref: DeclarationStatement =>
        ref.algoInt += 1
      case _ =>
    }

    walkStatements(s => if(s.algoIncrementale != koId){
      s.walkRemapDrivingExpressions {
        case ref: BaseType =>
          if (ref.algoInt == 1 && ref.isComb && ref.isDirectionLess && (!onlyTypeNode || ref.isTypeNode) && ref.canSymplifyIt && Statement.isSomethingToFullStatement(ref) /*&& ref != excepted*/ ) {
            //TODO IR keep it
            ref.algoInt = 0
            val head = ref.head
            ref.algoIncrementale = koId
            head.algoIncrementale = koId
            head.source
          } else {
            ref
          }
        case e => e
      }
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
      case s: SwitchStatement =>
        var failed = false
        s.elements.foreach{element =>
          if(element.keys.size > 1){
            val fliter = mutable.HashSet[BigInt]()
            val toRemove = ArrayBuffer[Expression]()
            element.keys.foreach { e =>
              var special = false
              val v = e match {
                case lit: EnumLiteral[_] => BigInt(lit.senum.position)
                case lit: Literal => lit.getValue()
                case kb : SwitchStatementKeyBool if kb.key != null && !kb.key.withDontCare => kb.key.value
                case _ => special = true; BigInt(0)
              }
              if(!special) {
                if (fliter.contains(v)) {
                  if(s.removeDuplication){
                    toRemove += e
                  } else {
                    PendingError(s"DUPLICATED ELEMENTS IN SWITCH IS(...) STATEMENT. value=$v\n" + element.getScalaLocationLong)
                    failed = true
                  }
                }
                fliter += v
              }
            }
            element.keys --= toRemove
          }
        }
        if(!failed && s.isFullyCoveredWithoutDefault && !s.coverUnreachable) {
          if (s.defaultScope != null && !s.defaultScope.isEmpty) {
            PendingError(s"UNREACHABLE DEFAULT STATEMENT on \n" + s.getScalaLocationLong)
          }
          s.defaultScope = s.elements.last.scopeStatement
          s.elements.remove(s.elements.length - 1)
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
        if(io != null) for(bt <- io.flatten){
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
      val autoPullOn = mutable.LinkedHashSet[Expression]()
      c.dslBody.walkStatements(s => {
        var error = false

        s match {
          case s : InitialAssignmentStatement =>
          case s: AssignmentStatement =>
            val bt = s.finalTarget

            if (!(bt.isDirectionLess && bt.component == c) && !(bt.isOutputOrInOut && bt.component == c) && !(bt.isInputOrInOut && bt.component.parent == c)) {
              val identifier = if(c == null) "toplevel" else s"$c component"
              PendingError(s"HIERARCHY VIOLATION : $bt is driven by ${s.source}, but isn't accessible in the $identifier.\n${s.getScalaLocationLong}")
              error = true
            }

            if(!error && !bt.isInOut){
              val rootScope = s.rootScopeStatement
              var ptr       = s.parentScope

              while(ptr.parentStatement != null && ptr != rootScope){
                ptr = ptr.parentStatement.parentScope
              }

              if(ptr != rootScope){
                PendingError(s"SCOPE VIOLATION : $bt is assigned outside its declaration scope at \n${s.getScalaLocationLong}")
              }
            }
          case s : MemPortStatement => {
            if(s.mem.component != s.component){
              PendingError(s"SCOPE VIOLATION : memory port $s was created in another component than its memory ${s.mem} \n${s.getScalaLocationLong}")
            }
          }
          case _ =>
        }

        if(!error) s.walkDrivingExpressions {
          case bt: BaseType =>
            if (!(bt.component == c) && !(bt.isInputOrInOut && bt.component.parent == c) && !(bt.isOutputOrInOut && bt.component.parent == c)) {
              if(bt.component == null || bt.getComponents().head != pc.topLevel){
                PendingError(s"OLD NETLIST RE-USED : $bt is used to drive the $s statement, but was defined in another netlist.\nBe sure you didn't defined a hardware constant as a 'val' in a global scala object.\n${s.getScalaLocationLong}")
              } else {
                if(c.withHierarchyAutoPull){
                  autoPullOn += bt
                } else {
                  PendingError(s"HIERARCHY VIOLATION : $bt is used to drive the $s statement, but isn't readable in the $c component\n${s.getScalaLocationLong}")
                }
              }
            }
          case s : MemPortStatement =>{
            if(s.mem.component != c){
              PendingError(s"OLD NETLIST RE-USED : Memory port $s of memory ${s.mem} is used to drive the $s statement, but was defined in another netlist.\nBe sure you didn't defined a hardware constant as a 'val' in a global scala object.\n${s.getScalaLocationLong}")
            }
          }
          case _ =>
        }
      })

      if(autoPullOn.nonEmpty) c.rework{
        c.dslBody.walkStatements(s =>
          s.walkRemapDrivingExpressions(e =>
            if(autoPullOn.contains(e)){
              e.asInstanceOf[BaseType].pull()
            } else {
              e
            }
          )
        )
      }

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

    val regToComb = ArrayBuffer[BaseType]()

    walkStatements{
      case bt: BaseType if bt.isReg && !bt.hasTag(AllowPartialyAssignedTag)=>
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
          var withInit = false
          bt.foreachStatements{
            case s : InitAssignmentStatement => withInit = true
            case _ =>
          }
          if(assignedBits.isEmpty) {
            if(withInit){
              regToComb += bt
              if(bt.isVital && !bt.hasTag(unsetRegIfNoAssignementTag)){
                if(bt.scalaTrace == null){
                  PendingError(s"UNASSIGNED REGISTER $bt with init value, please apply the allowUnsetRegToAvoidLatch tag if that's fine\n${bt.getScalaLocationLong}")
                } else {
                  SpinalWarning(s"UNASSIGNED REGISTER $bt with init value, please apply the allowUnsetRegToAvoidLatch tag if that's fine\n${bt.getScalaLocationLong}")
                }

              }
            }else if(bt.isVital) {
              PendingError(s"UNASSIGNED REGISTER $bt, defined at\n${bt.getScalaLocationLong}")
            }
          }else {
            if(!withInit) {
              val unassignedBits = new AssignedBits(bt.getBitsWidth)
              unassignedBits.add(bt.getBitsWidth - 1, 0)
              unassignedBits.remove(assignedBits)
              PendingError(s"PARTIALLY ASSIGNED REGISTER $bt, unassigned bit mask is ${unassignedBits.toBinaryString}, defined at\n${bt.getScalaLocationLong}")
            }
          }
        }
      case _ =>
    }

    for(bt <- regToComb){
      bt.setAsComb()
      val statements = ArrayBuffer[AssignmentStatement]()

      bt.foreachStatements(statements += _)

      statements.foreach{
        case s: InitAssignmentStatement =>
          s.insertNext(DataAssignmentStatement(s.target, s.source).setScalaLocated(s))
          s.removeStatement()
      }
    }
  }
}

class PhaseCheck_noLatchNoOverride(pc: PhaseContext) extends PhaseCheck{

  override def impl(pc : PhaseContext): Unit = {
    import pc._

    walkComponentsExceptBlackbox(c => {
      val subInputsPerScope = mutable.HashMap[ScopeStatement, ArrayBuffer[BaseType]]()
      c.children.foreach(_.getAllIo.withFilter(_.isInput).foreach(input => subInputsPerScope.getOrElseUpdate(input.rootScopeStatement, ArrayBuffer[BaseType]()) += input))

      def walkBody(body: ScopeStatement, checkOverlap : Boolean): mutable.HashMap[BaseType, AssignedBits] = {
        val assigneds = mutable.HashMap[BaseType, AssignedBits]()

        def getOrEmpty(bt: BaseType) = assigneds.getOrElseUpdate(bt, new AssignedBits(bt.getBitsWidth))

        def getOrEmptyAdd(bt: BaseType, src: AssignedBits): Boolean = {
          var dst : AssignedBits = null
          var wasExisting = true
          assigneds.get(bt) match {
            case None => {
              dst = new AssignedBits(bt.getBitsWidth)
              assigneds(bt) = dst
              wasExisting = false
            }
            case Some(x) => dst = x
          }
          val ret = src.isFull && wasExisting &&  !bt.hasTag(allowAssignmentOverride)
          dst.add(src)
          ret
        }

        def getOrEmptyAdd3(bt: BaseType, hi: Int, lo: Int): Boolean = {
          var dst : AssignedBits = null
          var wasExisting = true
          assigneds.get(bt) match {
            case None => {
              dst = new AssignedBits(bt.getBitsWidth)
              assigneds(bt) = dst
              wasExisting = false
            }
            case Some(x) => dst = x
          }
          val ret = hi == dst.width-1 && lo == 0 && wasExisting && !bt.hasTag(allowAssignmentOverride)
          dst.add(hi, lo)
          ret
        }

        def getOrEmptyAdd2(bt: BaseType, src: AssignedRange): Boolean = getOrEmptyAdd3(bt, src.hi, src.lo)
        def noPoison(that : AssignmentStatement) = !checkOverlap || (that.source match {
          case lit : Literal if lit.hasPoison() => false
          case _ => true
        })
        body.foreachStatements {
          case s: DataAssignmentStatement =>  //Omit InitAssignmentStatement
            if(!s.finalTarget.isAnalog && noPoison(s)) {
              s.target match {
                case bt: BaseType => if (getOrEmptyAdd3(bt, bt.getBitsWidth - 1, 0) && checkOverlap) {
                  PendingError(s"ASSIGNMENT OVERLAP completely the previous one of $bt\n${s.getScalaLocationLong}")
                }
                case e: BitVectorAssignmentExpression =>
                  val bt = e.finalTarget
                  if (getOrEmptyAdd2(bt, e.getMinAssignedBits) && checkOverlap) {
                    PendingError(s"ASSIGNMENT OVERLAP completely the previous one of $bt\n${s.getScalaLocationLong}")
                  }
              }
            }
          case s: WhenStatement =>
            val whenTrue  = walkBody(s.whenTrue, checkOverlap)
            val whenFalse = walkBody(s.whenFalse, checkOverlap)

            for ((bt, assigned) <- whenTrue) {
              whenFalse.get(bt) match {
                case Some(otherBt) => getOrEmptyAdd(bt, otherBt.intersect(assigned))
                case None => getOrEmpty(bt)
              }
            }
            whenFalse.foreach(p => getOrEmpty(p._1))
          case s: SwitchStatement =>
            val stuffs = if(s.isFullyCoveredWithoutDefault){
              s.elements.map(e => walkBody(e.scopeStatement, checkOverlap))
            } else if(s.defaultScope != null){
              s.elements.map(e => walkBody(e.scopeStatement, checkOverlap)) += walkBody(s.defaultScope, checkOverlap)
            } else {
              s.elements.foreach(e => walkBody(e.scopeStatement, checkOverlap).foreach(e => getOrEmpty(e._1)))
              null
            }

            if(stuffs != null) {
              val mix = mutable.HashMap[BaseType, AssignedBits]()
              for (stuff <- stuffs) {
                for ((bt, assigned) <- stuff) {
                  mix.update(bt, assigned)
                }
              }

              for((bt, assigned) <- mix){
                var continue = true
                val iterator = stuffs.iterator
                while(iterator.hasNext && continue){
                  iterator.next().get(bt) match {
                    case None => {
                      assigned.clear()
                      continue = false
                    }
                    case Some(branch) =>{
                      assigned.intersect(branch)
                    }
                  }
                }
              }

              for ((bt, assigned) <- mix) {
                if(getOrEmptyAdd(bt,assigned) && checkOverlap){
                  PendingError(s"ASSIGNMENT OVERLAP completely the previous one of $bt\n ${s.getScalaLocationLong}")
                }
              }
            }
          case s =>
        }


        def finalCheck(bt : BaseType): Unit ={
          // Hold off until suffix parent is processed
          if (bt.isSuffix)
            return
          if (bt.isInstanceOf[Suffixable]) {
            if (bt.dlcIsEmpty)
              return bt.asInstanceOf[Suffixable].elements.filter(_._2.isInstanceOf[BaseType]).foreach(e => finalCheck(e._2.asInstanceOf[BaseType]))
          }

          val assignedBits = getOrEmpty(bt)
          if ((bt.isVital || !bt.dlcIsEmpty) && bt.rootScopeStatement == body && !assignedBits.isFull){
            if(bt.isComb) {
              val unassignedBits = new AssignedBits(bt.getBitsWidth)

              unassignedBits.add(bt.getBitsWidth - 1, 0)
              unassignedBits.remove(assignedBits)

              if (!unassignedBits.isEmpty) {
                if (bt.dlcIsEmpty)
                  PendingError(s"NO DRIVER ON $bt, defined at\n${bt.getScalaLocationLong}")
                else if (!bt.hasTag(noLatchCheck)) {
                  if (unassignedBits.isFull)
                    PendingError(s"LATCH DETECTED from the combinatorial signal $bt, defined at\n${bt.getScalaLocationLong}")
                  else
                    PendingError(s"LATCH DETECTED from the combinatorial signal $bt, unassigned bit mask " +
                      s"is ${unassignedBits.toBinaryString}, defined at\n${bt.getScalaLocationLong}")
                }
              }
            }
          }
        }

        //Final checks usages
        if(!checkOverlap) {
          body.foreachDeclarations {
            case bt: BaseType => finalCheck(bt)
            case _ =>
          }
          subInputsPerScope.get(body).foreach(_.foreach(finalCheck))
        }

        assigneds
      }
      walkBody(c.dslBody, true)
      walkBody(c.dslBody, false)
    })
  }
}



class PhaseGetInfoRTL(prunedSignals: mutable.Set[BaseType], unusedSignals: mutable.Set[BaseType], counterRegisters: Ref[Int], blackboxesSourcesPaths: mutable.LinkedHashSet[String])(pc: PhaseContext) extends PhaseCheck {

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    //    val targetAlgoId = GlobalData.get.algoId
    //    Node.walk(walkNodesDefautStack,node => {node.algoId = targetAlgoId})
    walkStatements{
      case bt: BaseType if !bt.isVital && (!bt.isInstanceOf[BitVector] || bt.asInstanceOf[BitVector].inferredWidth != 0) && !bt.hasTag(unusedTag) && bt.isNamed && !bt.getName().startsWith(globalData.anonymSignalPrefix) =>
        prunedSignals += bt
      case bt: BaseType if bt.isVital && bt.isReg =>
        counterRegisters.value += bt.getBitsWidth
      case _ =>
    }

    walkComponents{
      case bb: BlackBox if bb.isBlackBox => bb.listRTLPath.foreach(path => blackboxesSourcesPaths += path)
      case _            =>
    }

    val usedId = GlobalData.get.allocateAlgoIncrementale()

    walkStatements(s => {
      s.walkDrivingExpressions(e => e.algoIncrementale = usedId)
      s match {
        case s: MemReadSync  => s.algoIncrementale = usedId
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

class PhasePropagateNames(pc: PhaseContext) extends PhaseMisc {
  override def impl(pc: PhaseContext) : Unit = {
    import pc._
    val algoId = globalData.allocateAlgoIncrementale() //Allows to avoid chaining allocated names

    // All unamed signals are cleaned up to avoid composite / partial name side effects
    walkStatements{
      case bt : BaseType if bt.isUnnamed => bt.unsetName()
      case _ =>
    }

    // propagate all named signals names to their unamed drivers
    walkStatements{
      case dst : BaseType => if (dst.isNamed && dst.algoIncrementale != algoId) {
        def explore(bt: BaseType, depth : Int): Unit = {
          bt.foreachStatements{s =>
            s.walkDrivingExpressions{
              case src : BaseType => if(src.isUnnamed || (src.algoIncrementale == algoId && src.algoInt > depth)){
                src.unsetName()
                src.setWeakName(globalData.anonymSignalPrefix + "_" + dst.getName())
                src.algoIncrementale = algoId
                src.algoInt = depth
                explore(src, depth + 1)
              }
              case _ =>
            }
          }
        }
        explore(dst, 0)
      }
      case _ =>
    }
  }
}

class PhaseAllocateNames(pc: PhaseContext) extends PhaseMisc{

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    for (enumDef <- enums.keys) {
      if (enumDef.isWeak)
        enumDef.setName(globalScope.allocateName(enumDef.getName()))
      else
        globalScope.iWantIt(enumDef.getName(),s"Reserved name ${enumDef.getName()} is not free for ${enumDef.toString()}")
    }


    for((senum, encodings) <- enums;
        encodingsScope = new NamingScope(duplicationPostfix);
        encoding <- encodings){

      reservedKeyWords.foreach(encodingsScope.allocateName(_))
      for (el <- senum.elements) {
        el.setName(encodingsScope.allocateName(el.getName()))
      }

      if (encoding.isWeak)
        encoding.setName(encodingsScope.allocateName(encoding.getName()))
      else
        encodingsScope.iWantIt(encoding.getName(),s"Reserved name ${encoding.getName()} is not free for ${encoding.toString()}")
    }

    def allocate(c : Component): Unit ={
      if (c.isInstanceOf[BlackBox] && c.asInstanceOf[BlackBox].isBlackBox)
        globalScope.lockName(c.definitionName)
      else if (!c.definitionNameNoMerge)
        c.definitionName = globalScope.allocateName(c.definitionName)
    }
    for (parent <- sortedComponents.reverse) {
      for(c <- parent.children) {
        allocate(c)
      }
    }
    allocate(topLevel)

    globalScope.lockScope()

    for (c <- sortedComponents.reverse) {
      c.allocateNames(pc.globalScope)
    }
  }
}

class PhaseStdLogicVectorAtTopLevelIo() extends PhaseNetlist {

  override def impl(pc: PhaseContext): Unit = {

    pc.topLevel.rework {

      def wrapIO[T <: BitVector](io: T): Unit = {

        val newIO = Bits(io.getWidth bits)

        newIO.setName(io.getName())
        io.unsetName()

        io.dir match {
          case `in`  =>
            in(newIO)
            io.assignFromBits(newIO)
          case `out` =>
            out(newIO)
            newIO := B(io)
        }

        io.setAsDirectionLess().allowDirectionLessIo
      }

      val ioList = pc.topLevel.getAllIo.toArray

      ioList.foreach {
        case io: UInt if io.isInput | io.isOutput => wrapIO(io)
        case io: SInt if io.isInput | io.isOutput => wrapIO(io)
        case _ =>
      }

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


class PhaseCreateComponent(gen: => Component)(pc: PhaseContext) extends PhaseNetlist{

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    val defaultClockDomain = ClockDomain.external("",frequency = config.defaultClockDomainFrequency)


    Engine.create {
      val ctx = ClockDomainStack.set(defaultClockDomain)
      native //Avoid unconstructable during phase
      binarySequential
      binaryOneHot
      val top = gen
      fiber.hardFork(ctx.globalData.elab.runSync()).setName("global_elab")
      if(top.isInBlackBoxTree){
        SpinalError(s"The toplevel can't be a BlackBox (${top.getClass.getSimpleName})")
      }
      ctx.restore()
//      assert(DslScopeStack.get != null, "The SpinalHDL context seems wrong, did you included the idslplugin in your scala build scripts ? This is a Scala compiler plugin, see https://github.com/SpinalHDL/SpinalTemplateSbt/blob/666dcbba79181659d0c736eb931d19ec1dc17a25/build.sbt#L13.")
    }

//    //Ensure there is no prepop tasks remaining, as things can be quite aggresively context switched since the fiber update
//    var hadPrePop = true
//    while(hadPrePop) {
//      hadPrePop = false
//      pc.walkComponents { c =>
//        assert(c.prePopTasks.isEmpty)
////        if (c.prePopTasks.nonEmpty) {
////          c.rework(
////            c.prePop()
////          )
////          hadPrePop = true
////        }
//      }
//    }

    pc.checkGlobalData()
  }
}


/**
  * Initialize all registers not initialized
  */
class PhaseInitReg() extends PhaseNetlist {
  override def impl(pc: PhaseContext): Unit = {
    import pc._

    SpinalProgress("Initialize all registers not initialized")


    walkComponents{ comp =>
      comp.rework{
        comp.dslBody.walkStatements{

          case bt: BaseType if bt.isReg =>

            SpinalInfo(s"Init register ${bt.toString}")

            if(!bt.hasInit){
              bt match{
                case d: SInt                        => d.init(0)
                case d: Bits                        => d.init(0)
                case d: UInt                        => d.init(0)
                case d: Bool                        => d.init(False)
                case d: SpinalEnumCraft[SpinalEnum] => d.init(d.spinalEnum.elements(0))
                case _                              =>
              }
            }

          case _ =>
        }
      }
    }
  }
}


class PhaseDummy(doThat : => Unit) extends PhaseMisc {
  override def impl(pc : PhaseContext): Unit = {
    doThat
  }
}

class PhaseFillRegsInit() extends Phase{
  override def impl(pc: PhaseContext): Unit = {
    pc.walkDeclarations {
      case bt: BaseType if bt.isReg && bt.clockDomain.canInit && !bt.hasTag(crossClockBuffer) && !bt.hasTag(crossClockDomain) && !bt.hasTag(noInit) => {
        if (!bt.hasInit) {
          bt.parentScope.on {
            bt.init(bt match {
              case bt : SpinalEnumCraft[_] => bt.spinalEnum.elements.head
              case bt => bt.getZero
            })
          }
        } else {
          bt.foreachStatements{
            case s : InitAssignmentStatement => assert(s.target == bt)
            case _ =>
          }
        }
      }
      case bt: BaseType if bt.isReg && !bt.clockDomain.canInit =>
//        println(s"Can't init ${bt.getRtlPath()}")
      case _ =>
    }
  }

  override def hasNetlistImpact: Boolean = true
}

class PhaseRandomizedMem() extends PhaseNetlist {
  override def impl(pc: PhaseContext): Unit = {
    pc.walkDeclarations{
      case mem : Mem[_] if mem.initialContent == null => {
        mem.initBigInt(Array.fill(mem.wordCount)(BigInt.apply(mem.width, Random)))
      }
      case _ =>
    }
  }
}

class PhaseCheckAsyncResetsSources() extends PhaseCheck {
  override def impl(pc: PhaseContext): Unit = {
    val cds = new mutable.LinkedHashSet[ClockDomain]()
    pc.walkDeclarations{
      case bt : BaseType if bt.isReg && bt.clockDomain.reset != null && bt.clockDomain.config.resetKind == ASYNC => {
        if(bt.component.getClass.getSimpleName != "BufferCC") {
          cds += bt.clockDomain
        }
      }
      case _ =>
    }

    for(cd <- cds){
      cd.reset.getDrivingReg(false) match {
        case null => {
          println(s"Can't find driver of ${cd.reset}")
        }
        case driver => {
          println(s"${cd.reset} reset for clock ${cd.clock} is clocked by ${driver.clockDomain.clock}")
          println(s"- FF : ${driver}")
          if(driver.clockDomain.clock != cd.clock){
            println(s"- Mismatch clock !!")
          }
        }
      }
    }
  }
}

object SpinalVhdlBoot{
  def apply[T <: Component](config : SpinalConfig)(gen : => T) : SpinalReport[T] ={
    if(config.debugComponents.nonEmpty){
      return singleShot(config)(gen)
    }
    try {
      singleShot(config)(gen)
    } catch {
      case e: NullPointerException =>
        println(
          """
            |ERROR !
            |A null pointer access has been detected in the JVM.
            |This could happen when in your SpinalHDL description, you access an signal which is only defined further.
            |For instance :
            |  val result = Bool()
            |  result := a ^ b  //a and b can't be accessed there because they are only defined one line below (Software rule of execution order)
            |  val a,b = Bool()
          """.stripMargin)
        System.out.flush()
        throw e
      case e: Throwable => {
        println("\n**********************************************************************************************")
        val errCnt = SpinalError.getErrorCount()
        SpinalWarning(s"Elaboration failed (${errCnt} error" + (if(errCnt > 1){s"s"} else {s""}) + s").\n" +
          s"          Spinal will restart with scala trace to help you to find the problem.")
        println("**********************************************************************************************\n")
        System.out.flush()

        //Fill the ScalaLocated object which had trigger into the scalaLocatedCompoments
        GlobalData.get.applyScalaLocated()

        return singleShot(config.copy(debugComponents = GlobalData.get.scalaLocatedComponents))(gen)
      }
    }
  }

  def singleShot[T <: Component](config: SpinalConfig)(gen: => T): SpinalReport[T] = ScopeProperty.sandbox{
    val pc = new PhaseContext(config)
    pc.globalData.phaseContext = pc
    pc.globalData.anonymSignalPrefix = if(config.anonymSignalPrefix == null) "zz" else config.anonymSignalPrefix

    val prunedSignals   = mutable.Set[BaseType]()
    val unusedSignals   = mutable.Set[BaseType]()
    val counterRegister = Ref[Int](0)
    val blackboxesSourcesPaths  = new mutable.LinkedHashSet[String]()

    SpinalProgress("Elaborate components")

    val phases = ArrayBuffer[Phase]()

    phases += new PhaseCreateComponent(gen)(pc)
    phases += new PhaseDummy(SpinalProgress("Checks and transforms"))
    phases ++= config.transformationPhases
    phases ++= config.memBlackBoxers
    if(config.onlyStdLogicVectorAtTopLevelIo){
      phases += new PhaseStdLogicVectorAtTopLevelIo()
    }
    phases += new PhaseDeviceSpecifics(pc)
    phases += new PhaseApplyIoDefault(pc)

    phases += new PhaseNameNodesByReflection(pc)
    phases += new PhaseCollectAndNameEnum(pc)

    phases += new PhaseCheckIoBundle()
    phases += new PhaseCheckHiearchy()
    phases += new PhaseAnalog()
    phases += new PhaseNextifyReg()
    phases += new PhaseRemoveUselessStuff(false, false)
    phases += new PhaseRemoveIntermediateUnnameds(true)

    phases += new PhasePullClockDomains(pc)

    phases += new PhaseInferEnumEncodings(pc,e => e)
    phases += new PhaseInferWidth(pc)
    phases += new PhaseNormalizeNodeInputs(pc)
    phases += new PhaseRemoveIntermediateUnnameds(false)
    phases += new PhaseSimplifyNodes(pc)

    phases += new PhaseCompletSwitchCases()
    phases += new PhaseRemoveUselessStuff(true, true)
    phases += new PhaseRemoveIntermediateUnnameds(false)

    phases += new PhaseCheck_noLatchNoOverride(pc)
    phases += new PhaseCheck_noRegisterAsLatch()
    phases += new PhaseCheckCombinationalLoops()
    phases += new PhaseCheckCrossClock()

    phases += new PhasePropagateNames(pc)
    phases += new PhaseAllocateNames(pc)
    phases += new PhaseDevice(pc)

    phases += new PhaseGetInfoRTL(prunedSignals, unusedSignals, counterRegister, blackboxesSourcesPaths)(pc)
    val report = new SpinalReport[T]()
    report.globalData = pc.globalData
    phases += new PhaseDummy(SpinalProgress("Generate VHDL"))
    phases += new PhaseVhdl(pc, report)

    for(inserter <-config.phasesInserters){
      inserter(phases)
    }

    for(phase <- phases){
      if(config.verbose) SpinalProgress(s"${phase.getClass.getSimpleName}")
      pc.doPhase(phase)
    }

    if(prunedSignals.nonEmpty){
      SpinalWarning(s"${prunedSignals.size} signals were pruned. You can call printPruned on the backend report to get more informations.")
    }

    pc.checkGlobalData()

//    SpinalInfo(s"Number of registers : ${counterRegister.value}")


    report.toplevel = pc.topLevel.asInstanceOf[T]
    report.prunedSignals ++= prunedSignals
    report.unusedSignals ++= unusedSignals
    report.counterRegister = counterRegister.value
    report.blackboxesSourcesPaths ++= blackboxesSourcesPaths

    report
  }
}



object SpinalVerilogBoot{

  def apply[T <: Component](config: SpinalConfig)(gen: => T): SpinalReport[T] ={
    if(config.debugComponents.nonEmpty){
      return singleShot(config)(gen)
    }
    try {
      singleShot(config)(gen)
    } catch {
      case e: NullPointerException =>
        println(
          """
            |ERROR !
            |A null pointer access has been detected in the JVM.
            |This could happen when in your SpinalHDL description, you access an signal which is only defined further.
            |For instance :
            |  val result = Bool()
            |  result := a ^ b  //a and b can't be accessed there because they are only defined one line below (Software rule of execution order)
            |  val a,b = Bool()
          """.stripMargin)
        System.out.flush()
        throw e
      case e: Throwable => {
        println(e.getStackTrace.mkString("\n"))
        println("\n**********************************************************************************************")
        val errCnt = SpinalError.getErrorCount()
        SpinalWarning(s"Elaboration failed (${errCnt} error" + (if(errCnt > 1){s"s"} else {s""}) + s").\n" +
          s"          Spinal will restart with scala trace to help you to find the problem.")
        println("**********************************************************************************************\n")
        System.out.flush()

        //Fill the ScalaLocated object which had trigger into the scalaLocatedCompoments
        GlobalData.get.applyScalaLocated()
        return singleShot(config.copy(debugComponents = GlobalData.get.scalaLocatedComponents))(gen)
      }
    }
  }

  def singleShot[T <: Component](config: SpinalConfig)(gen : => T): SpinalReport[T] = ScopeProperty.sandbox{
    val pc = new PhaseContext(config)
    pc.globalData.phaseContext = pc
    pc.globalData.anonymSignalPrefix = if(config.anonymSignalPrefix == null) "_zz" else config.anonymSignalPrefix

    val prunedSignals    = mutable.Set[BaseType]()
    val unusedSignals    = mutable.Set[BaseType]()
    val counterRegister  = Ref[Int](0)
    val blackboxesSourcesPaths  = new mutable.LinkedHashSet[String]()

    SpinalProgress("Elaborate components")

    val phases = ArrayBuffer[Phase]()

    phases += new PhaseCreateComponent(gen)(pc)
    phases += new PhaseDummy(SpinalProgress("Checks and transforms"))
    phases ++= config.transformationPhases
    phases ++= config.memBlackBoxers
    phases += new PhaseDeviceSpecifics(pc)
    phases += new PhaseApplyIoDefault(pc)

    phases += new PhaseNameNodesByReflection(pc)
    phases += new PhaseCollectAndNameEnum(pc)

    phases += new PhaseCheckIoBundle()
    phases += new PhaseCheckHiearchy()
    phases += new PhaseAnalog()
    phases += new PhaseNextifyReg()
    phases += new PhaseRemoveUselessStuff(false, false)
    phases += new PhaseRemoveIntermediateUnnameds(true)

    phases += new PhasePullClockDomains(pc)

    phases += new PhaseInferEnumEncodings(pc,e => if(e == `native`) binarySequential else e)
    phases += new PhaseInferWidth(pc)
    phases += new PhaseNormalizeNodeInputs(pc)
    phases += new PhaseRemoveIntermediateUnnameds(false)
    phases += new PhaseSimplifyNodes(pc)

    phases += new PhaseCompletSwitchCases()
    phases += new PhaseRemoveUselessStuff(true, true)
    phases += new PhaseRemoveIntermediateUnnameds(false)

    phases += new PhaseCheck_noLatchNoOverride(pc)
    phases += new PhaseCheck_noRegisterAsLatch()
    phases += new PhaseCheckCombinationalLoops()
    phases += new PhaseCheckCrossClock()

    phases += new PhasePropagateNames(pc)
    phases += new PhaseAllocateNames(pc)
    phases += new PhaseDevice(pc)

    phases += new PhaseGetInfoRTL(prunedSignals, unusedSignals, counterRegister, blackboxesSourcesPaths)(pc)

    phases += new PhaseDummy(SpinalProgress("Generate Verilog"))

    val report = new SpinalReport[T]()
    report.globalData = pc.globalData
    phases += new PhaseVerilog(pc, report)

    for(inserter <-config.phasesInserters){
      inserter(phases)
    }

    for(phase <- phases){
      if(config.verbose) SpinalProgress(s"${phase.getClass.getSimpleName}")
      pc.doPhase(phase)
    }

    if(prunedSignals.nonEmpty){
      SpinalWarning(s"${prunedSignals.size} signals were pruned. You can call printPruned on the backend report to get more informations.")
    }

//    SpinalInfo(s"Number of registers : ${counterRegister.value}")

    pc.checkGlobalData()
    report.toplevel = pc.topLevel.asInstanceOf[T]
    report.prunedSignals ++= prunedSignals
    report.unusedSignals ++= unusedSignals
    report.counterRegister = counterRegister.value
    report.blackboxesSourcesPaths ++= blackboxesSourcesPaths

    report
  }
}

