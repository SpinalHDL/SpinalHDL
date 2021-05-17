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

import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import spinal.core.internals._

import scala.io.Source


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
    if (DslScopeStack.nonEmpty) SpinalError("dslScope stack is not empty :(")
    if (ClockDomainStack.nonEmpty) SpinalError("dslClockDomain stack is not empty :(")
  }

  def checkPendingErrors() = if(globalData.pendingErrors.nonEmpty)
    SpinalError()

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
    checkPendingErrors()
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
            scope.push()
            node.assignFrom(defaultValue.that)
            scope.pop()
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

    //Be sure that sub io assign parent component stuff
    walkComponents(c => c.ioSet.withFilter(_.isInOut).foreach(io => {
      io.foreachStatements {
        case s@AssignmentStatement(_: BaseType, x: BaseType) if x.isAnalog && x.component == c.parent =>
          s.dlcRemove()
          x.dlcAppend(s)
          s.target = x
          s.source = io
        case _ =>
      }
    }))

    val analogs        = ArrayBuffer[BaseType]()
    val islands        = mutable.LinkedHashSet[mutable.LinkedHashSet[BaseType]]()
    val analogToIsland = mutable.HashMap[BaseType,mutable.LinkedHashSet[BaseType]]()

    def addToIsland(that: BaseType, island: mutable.LinkedHashSet[BaseType]): Unit = {
      island += that
      analogToIsland(that) = island
    }

    //val wrapped = mutable.HashMap[BaseType, BaseType]()

    walkStatements{
      case bt: BaseType if bt.isAnalog =>
        analogs += bt

        //Manage islands
        bt.foreachStatements {
          case s@AssignmentStatement(x, y: BaseType) if y.isAnalog =>
            if (s.finalTarget.component == y.component) {
              (analogToIsland.get(bt), analogToIsland.get(y)) match {
                case (None, None) =>
                  val island = mutable.LinkedHashSet[BaseType]()
                  addToIsland(bt, island)
                  addToIsland(y, island)
                  islands += island
                case (None, Some(island)) =>
                  addToIsland(bt, island)
                case (Some(island), None) =>
                  addToIsland(y, island)
                case (Some(islandBt), Some(islandY)) =>
                  islandY.foreach(addToIsland(_, islandBt))
                  islands.remove(islandY)
              }
            }
          case AssignmentStatement(x, y: BaseType) if !y.isAnalog =>
        }

        if(!analogToIsland.contains(bt)){
          val island = mutable.LinkedHashSet[BaseType]()
          addToIsland(bt,island)
          islands += island
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

      //Remove target analog assignments
      target.foreachStatements {
        case s@AssignmentStatement(x, y: BaseType) if y.isAnalog && y.component == target.component => s.removeStatement()
        case _ =>
      }

      //redirect island assignments to target
      //drive isllands analogs from target as comb signal
      for(bt <- island if bt != target){
        val btStatements = ArrayBuffer[AssignmentStatement]()
        bt.foreachStatements(btStatements += _)
        btStatements.foreach {
          case s@AssignmentStatement(_, x: BaseType) if !x.isAnalog => //analog driver
            s.dlcRemove()
            target.dlcAppend(s)
            s.walkRemapExpressions(e => if (e == bt) target else e)
          case s@AssignmentStatement(_, x: BaseType) if x.isAnalog && x.component.parent == bt.component => //analog connection
            s.dlcRemove()
            target.dlcAppend(s)
            s.walkRemapExpressions(e => if (e == bt) target else e)
          case _ =>
        }

        bt.removeAssignments()
        bt.setAsComb()
        bt.rootScopeStatement.push()
        bt := target
        bt.rootScopeStatement.pop()
      }

      //Convert target comb assignment into AnalogDriver nods
      target.foreachStatements(s => {
        s.source match {
          case btSource: BaseType if btSource.isAnalog =>
          case btSource =>
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
            driver.data   = s.source.asInstanceOf[driver.T]
            driver.enable = enable
            s.source      = driver
        }
      })
      //      }
    })
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


class MemTopology(val mem: Mem[_], val consumers : mutable.HashMap[Expression, ArrayBuffer[ExpressionContainer]]) {
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
      if(mem.addressWidth != 0) {
        doBlackboxing(pc, new MemTopology(mem, consumers))
      } else{
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
          val content = Bits(mem.width bits)
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

    def removeMem(): Unit ={
      mem.removeStatement()
      mem.foreachStatements(s => s.removeStatement())
    }

    if (mem.initialContent != null) {
      return "Can't blackbox ROM"  //TODO
      //      } else if (topo.writes.size == 1 && topo.readsAsync.size == 1 && topo.portCount == 2) {
    } else if (topo.writes.size == 1 && (topo.readsAsync.nonEmpty || topo.readsSync.nonEmpty) && topo.writeReadSameAddressSync.isEmpty && topo.readWriteSync.isEmpty) {
      mem.component.rework {
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
            ram.io.wr.mask := B"1"

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

        val ram = new Ram_1wrs(
          wordWidth = mem.getWidth,
          wordCount = mem.wordCount,
          technology = mem.technology,
          readUnderWrite = port.readUnderWrite,
          maskWidth = if (port.mask != null) port.mask.getWidth else 1,
          maskEnable = port.mask != null
        )

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
          portA_clock = portA.clockDomain,
          portA_addressWidth = portA.address.getWidth,
          portA_dataWidth = portA.getWidth,
          portA_maskWidth = if (portA.mask != null) portA.mask.getWidth else 1,
          portA_maskEnable = portA.mask != null,
          portB_readUnderWrite = portB.readUnderWrite,
          portB_clock = portB.clockDomain,
          portB_addressWidth = portB.address.getWidth,
          portB_dataWidth = portB.getWidth,
          portB_maskWidth = if (portB.mask != null) portB.mask.getWidth else 1,
          portB_maskEnable = portB.mask != null
        )

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
  def apply(that : Any): String = that.getClass.getSimpleName.replace("$",".").split("\\.").head
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
        if (c.definitionName == null) {
          val pre = c match {
            case t: BlackBox => ""
            case _ => config.globalPrefix
          }
          val privateNsN = (if(config.privateNamespace) topLevel.definitionName + "_" else "")
          c.definitionName = pre + privateNsN + classNameOf(c)
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
    walkDeclarations {
      case enum: SpinalEnumCraft[_] => enums.getOrElseUpdate(enum.spinalEnum, null) //Encodings will be added later
      case _ =>
    }

    val scope = pc.globalScope.newChild("")

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
          case obj: Nameable => obj.setName(scope.getUnusedName(name), Nameable.DATAMODEL_WEAK)
          case _ =>
        }
      })

      for (e <- enumDef.elements) {
        if (e.isUnnamed) {
          e.setName(scope.getUnusedName("e" + e.position), Nameable.DATAMODEL_WEAK)
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
    })
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

    nodes.foreach(enum => {
      enum.swapEncoding(encodingSwap(enum.getEncoding))
    })

    algo = globalData.allocateAlgoIncrementale()

    nodes.foreach(enum => {
      if(enum.propagateEncoding){

        def propagateOn(that : Expression): Unit = {
          that match {
            case that: InferableEnumEncoding =>
              if(that.algoIncrementale == algo) return

              that.algoIncrementale = algo

              if(that.encodingProposal(enum.getEncoding)) {
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
    enums.keys.toArray.distinct.foreach(enums(_) = mutable.LinkedHashSet[SpinalEnumEncoding]())
    nodes.foreach(enum => {
      enums(enum.getDefinition) += enum.getEncoding
    })

    //give a name to unnamed encodingss
    val unnamedEncodings = enums.valuesIterator.flatten.toSeq.distinct.withFilter(_.isUnnamed).foreach(_.setName("anonymousEnc", Nameable.DATAMODEL_WEAK))

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

class PhaseDevice(pc : PhaseContext) extends PhaseMisc{
  override def impl(pc: PhaseContext): Unit = {
    pc.walkDeclarations {
      case mem: Mem[_] => {
        var hit = false
        mem.foreachStatements {
          case port: MemReadAsync => hit = true
          case _ =>
        }
        if (hit) mem.addAttribute("ram_style", "distributed") //Vivado stupid gambling workaround Synth 8-6430
      }
      case bt : BaseType =>{
        if(bt.isReg && (bt.hasTag(crossClockDomain) || bt.hasTag(crossClockBuffer))){
          bt.addAttribute("async_reg", "true")
        }
      }
      case _ =>
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

          if (e.inferredWidth > 4096) {
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
            if (e.getWidth > 4096) {
              errors += s"Way too big signal $e at ${e.getScalaLocationLong}"
            }
          case _ =>
        }

        walkDeclarations {
          case e: Widthable => widthableCheck(e)
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
    def getSyncronous(that : Bool) : immutable.Set[Bool] = {
      solved.get(that) match {
        case Some(sync) => sync
        case None => {
          var sync = scala.collection.immutable.Set[Bool]()

          //Collect all the directly syncronous Bool
          sync += that
          that.foreachTag {
            case tag : ClockSyncTag => sync += tag.a; sync += tag.b
            case tag : ClockDrivedTag => sync ++= getSyncronous(tag.driver)
            case _ =>
          }

          //Lock for driver inferation
          if (that.hasOnlyOneStatement && that.head.parentScope == that.rootScopeStatement && that.head.source.isInstanceOf[Bool] && that.head.source.asInstanceOf[Bool].isComb) {
            sync ++= getSyncronous(that.head.source.asInstanceOf[Bool])
          }

          //Cache result
          solved(that) = sync

          sync
        }
      }
    }
    def areSynchronousBool(a : Bool, b : Bool): Boolean = getSyncronous(a).contains(b) || getSyncronous(b).contains(a) || getSyncronous(a).intersect(getSyncronous(b)).nonEmpty

    def areSynchronous(a : ClockDomain, b : ClockDomain): Boolean ={
      a == b || a.clock == b.clock || areSynchronousBool(a.clock, b.clock)
      //          if(a.isSynchronousWith(b)){
      //            true
      //          }else{
      //            def getDriver(that : Bool): Bool ={
      //              if(that.hasOnlyOneStatement && that.head.parentScope == that.rootScopeStatement && that.head.source.isInstanceOf[Bool] && that.head.source.asInstanceOf[Bool].isComb){
      //                getDriver(that.head.source.asInstanceOf[Bool])
      //              }else{
      //                that
      //              }
      //            }
      //            if(getDriver(a.clock) == getDriver(b.clock)){
      //              a.setSynchronousWith(b)
      //              true
      //            }else{
      //              false
      //            }
      //          }
    }

    //        class SyncGroup{
    //          val clocks = mutable.HashSet[Bool]()
    //        }
    //
    //        val clockToGroup = mutable.HashMap[Bool, SyncGroup]()
    //
    //        def getClockGroup(clock : Bool) : SyncGroup = {
    //          if(!clockToGroup.contains(clock)){
    //
    //          }
    //          clockToGroup.contains(clock)
    //        }


    walkStatements(s => {
      var walked = 0

      def walk(node: BaseNode, path: List[(BaseNode)], clockDomain: ClockDomain): Unit = {
        if(node.algoIncrementale == walked) return

        node.algoIncrementale = walked

        val newPath = node :: path

        def issue(syncDriver: BaseNode with ScalaLocated, otherClockDomain: ClockDomain): Unit = {
          val wellNameLoop = new StringBuilder()

          for(n <- newPath) n match{
            case n: DeclarationStatement =>
              wellNameLoop ++= s"      >>> ${n.toString()} at ${n.getScalaLocationShort} >>>\n"
            case _  =>
          }
          val multiLineLoop = newPath.map(n => "      " + n.toString).foldLeft("")(_ + "\n" + _)

          PendingError(
            s"""CLOCK CROSSING VIOLATION :
               |- Source            : ${syncDriver} ${syncDriver.getScalaLocationShort}
               |- Source clock      : ${otherClockDomain.clock}
               |- Destination       : ${s} ${s.getScalaLocationShort}
               |- Destination clock : ${clockDomain.clock}
               |- Source declaration :
               |${syncDriver.getScalaLocationLong}
               |- Destination declaration :
               |${s.getScalaLocationLong}
               |- Connection path :
               |${wellNameLoop}
             """.stripMargin
          )
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
          case node: Mem[_] =>
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
        case s: MemReadWrite if !s.hasTag(crossClockDomain) =>
          if (s.hasTag(classOf[ClockDomainTag])) {
            PendingError(s"Can't add ClockDomainTag to memory ports:\n" + s.getScalaLocationLong)
          }
          walked = GlobalData.get.allocateAlgoIncrementale()
          s.foreachDrivingExpression(as => walk(as, as :: s :: Nil, s.clockDomain))
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
            if(cd.hasResetSignal) propagate(cd.reset)
            if(cd.hasSoftResetSignal) propagate(cd.softReset)
            if(cd.hasClockEnableSignal) propagate(cd.clockEnable)
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
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
          case s: MemReadWrite =>
            propagate(s.mem)
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
          case s: MemReadSync =>
            propagate(s.mem)
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
          case s: MemReadAsync =>
            propagate(s.mem)
            s.walkExpression{ case e: Statement => propagate(e) case _ => }
        }
      }
    }

    //Propagate all vital signals (drive toplevel output and blackboxes inputs)
    topLevel.getAllIo.withFilter(bt => bt.isOutputOrInOut).foreach(propagate(_, tagVitals))
    walkComponents{
      case c: BlackBox if c.isBlackBox => c.getAllIo.withFilter(_.isInputOrInOut).foreach(propagate(_, tagVitals))
      case c =>
    }

    walkStatements{
      case s: DeclarationStatement => if(s.isNamed) propagate(s, false)
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
      case s: SwitchStatement if s.isFullyCoveredWithoutDefault =>
        if(s.defaultScope != null && !s.defaultScope.isEmpty){
          PendingError(s"UNREACHABLE DEFAULT STATEMENT on \n" + s.getScalaLocationLong)
        }
        s.defaultScope = s.elements.last.scopeStatement
        s.elements.remove(s.elements.length-1)
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
      c.dslBody.walkStatements(s => {
        var error = false

        s match {
          case s: AssignmentStatement =>
            val bt = s.finalTarget

            if (!(bt.isDirectionLess && bt.component == c) && !(bt.isOutputOrInOut && bt.component == c) && !(bt.isInputOrInOut && bt.component.parent == c)) {
              PendingError(s"HIERARCHY VIOLATION : $bt is driven by ${s.source}, but isn't accessible in the $c component.\n${s.getScalaLocationLong}")
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
          case _ =>
        }

        if(!error) s.walkExpression {
          case bt: BaseType =>
            if (!(bt.component == c) && !(bt.isInputOrInOut && bt.component.parent == c) && !(bt.isOutputOrInOut && bt.component.parent == c)) {
              if(bt.component == null || bt.getComponents().head != pc.topLevel){
                PendingError(s"OLD NETLIST RE-USED : $bt is used to drive the $s statement, but was defined in another netlist.\nBe sure you didn't defined a hardware constant as a 'val' in a global scala object.\n${s.getScalaLocationLong}")
              } else {
                PendingError(s"HIERARCHY VIOLATION : $bt is used to drive the $s statement, but isn't readable in the $c component\n${s.getScalaLocationLong}")
              }
            }
          case _ =>
        }
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
                SpinalWarning(s"UNASSIGNED REGISTER $bt with init value, please apply the allowUnsetRegToAvoidLatch tag if that's fine")
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
                else if (unassignedBits.isFull)
                  PendingError(s"LATCH DETECTED from the combinatorial signal $bt, defined at\n${bt.getScalaLocationLong}")
                else
                  PendingError(s"LATCH DETECTED from the combinatorial signal $bt, unassigned bit mask " +
                    s"is ${unassignedBits.toBinaryString}, defined at\n${bt.getScalaLocationLong}")
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


class PhaseAllocateNames(pc: PhaseContext) extends PhaseMisc{

  override def impl(pc: PhaseContext): Unit = {
    import pc._

    for (enumDef <- enums.keys) {
      if (enumDef.isWeak)
        enumDef.setName(globalScope.allocateName(enumDef.getName()))
      else
        globalScope.iWantIt(enumDef.getName(),s"Reserved name ${enumDef.getName()} is not free for ${enumDef.toString()}")
    }


    for((enum, encodings) <- enums;
        encodingsScope = new NamingScope(duplicationPostfix);
        encoding <- encodings){

      if (encoding.isWeak)
        encoding.setName(encodingsScope.allocateName(encoding.getName()))
      else
        encodingsScope.iWantIt(encoding.getName(),s"Reserved name ${encoding.getName()} is not free for ${encoding.toString()}")
    }

    for (c <- sortedComponents) {
      if (c.isInstanceOf[BlackBox] && c.asInstanceOf[BlackBox].isBlackBox)
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
      defaultClockDomain.push()
      native //Avoid unconstructable during phase
      binarySequential
      binaryOneHot
      gen
      defaultClockDomain.pop()
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
            |  val result = Bool
            |  result := a ^ b  //a and b can't be accessed there because they are only defined one line below (Software rule of execution order)
            |  val a,b = Bool
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
            |  val result = Bool
            |  result := a ^ b  //a and b can't be accessed there because they are only defined one line below (Software rule of execution order)
            |  val a,b = Bool
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
