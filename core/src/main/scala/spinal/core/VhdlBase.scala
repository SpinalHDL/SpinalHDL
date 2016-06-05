/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal.core

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC18F on 07.01.2015.
 */


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

trait VhdlBase extends VhdlVerilogBase{

  val vhdlKeyWords = Set[String]("in", "out", "buffer", "inout", "entity", "component", "architecture","type","open","block","access","or","and","xor","nand","nor")

//  override def getReservedKeyword(): Iterable[String] = {
//    return reservedKeyWords
//  }


  def emitSignal(ref: Node, typeNode: Node): String = {
    s"  signal ${emitReference(ref)} : ${emitDataType(typeNode)};\n"
  }


  def emitClockEdge(clock: Bool, edgeKind: EdgeKind): String = {
    s"${
      edgeKind match {
        case RISING => "rising_edge"
        case FALLING => "falling_edge"
      }
    }(${emitReference(clock)}) then\n"
  }


  def emitEnumLiteral[T <: SpinalEnum](enum : SpinalEnumElement[T],encoding: SpinalEnumEncoding) : String = {
    if(encoding.isNative)
      return "pkg_enum." + enum.getName()
    else
      return enum.parent.getName() + "_" + encoding.getName() + "_" + enum.getName()
  }

  def emitEnumType[T <: SpinalEnum](enum : SpinalEnumCraft[T]) : String = emitEnumType(enum.blueprint,enum.encoding)

  def emitEnumType(enum : SpinalEnum,encoding: SpinalEnumEncoding) : String = {
    if(encoding.isNative)
      return enum.getName()
    else
      return enum.getName() + "_" + encoding.getName() + "_type"
  }

  def emitDataType(node: Node, constrained: Boolean = true) = node match {
    case bool: Bool => "std_logic"
    case uint: UInt => s"unsigned${if (constrained) emitRange(uint) else ""}"
    case sint: SInt => s"signed${if (constrained) emitRange(sint) else ""}"
    case bits: Bits => s"std_logic_vector${if (constrained) emitRange(bits) else ""}"
    case mem: Mem[_] => s"${emitReference(mem)}_type"
    case enum: SpinalEnumCraft[_] => emitEnumType(enum)
    case _ => throw new Exception("Unknown datatype"); ""
  }

  def emitDirection(baseType: BaseType) = baseType.dir match {
    case `in` => "in"
    case `out` => "out"
    case _ => throw new Exception("Unknown direction"); ""
  }


  def emitRange(node: Node) = s"(${node.getWidth - 1} downto 0)"

  def emitReference(node: Node): String = {
    node match {
      case n: Nameable => {
        n.getNameElseThrow
      }
    }
  }

}
