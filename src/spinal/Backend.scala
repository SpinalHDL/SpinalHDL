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

package spinal

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC18F on 07.01.2015.
 */


object Backend {
  def resetAll: Unit = {
    when.stack.reset
    switch.stack.reset
    Component.stack.reset
    ClockDomain.stack.reset
    Node.areInferringWidth = false
    Node.getWidthWalkedSet.clear()
    Node.widthInferredCheck.clear()
  }
}

object BackendToComponentBridge {
  var defaultReset: Bool = null
  var defaultClock: Bool = null
}

class Backend {

  val components = ArrayBuffer[Component]()
  var sortedComponents = ArrayBuffer[Component]()
  val globalScope = new Scope()

  // val uniqueNameAllocator = new UniqueNameAllocator()
  def addReservedKeyWord(scope: Scope): Unit = {

  }

  def elaborate[T <: Component](gen: () => T): T = {
    Backend.resetAll

    //Default clock
    val clock = in.Bool()
    clock.setName("clk")
    clock.isIo = true
    BackendToComponentBridge.defaultClock = clock

    //Default reset
    val reset = in.Bool()
    reset.setName("reset")
    reset.isIo = true
    BackendToComponentBridge.defaultReset = reset

    //default clockDomain
    val defaultClockDomain = ClockDomain(clock, reset)

    ClockDomain.push(defaultClockDomain)
    val topLevel = gen()
    ClockDomain.pop(defaultClockDomain)

    elaborate(topLevel)
    topLevel
  }

  //TODO
  //TODO no cross clock domain violation
  //TODO generate test bench
  //TODO

  protected def elaborate(topLevel: Component): Unit = {
    SpinalInfoPhase("Start analysis and transform")

    addReservedKeyWord(globalScope)
    addComponent(topLevel)
    sortedComponents = components.sortWith(_.level > _.level)

    SpinalInfoPhase("Get names from reflection")
    nameNodesByReflection



    //Component connection
    SpinalInfoPhase("Transform connection")
    configureComponentIo
    //moveInputRegisterToParent
    pullClockDomains
    check_noNull_noCrossHierarchy_noInputRegister
    addInOutBinding
    allowNodesToReadInputOfKindComponent


    //Node width
    SpinalInfoPhase("Infer nodes's bit width")
    inferWidth
    propagateBaseTypeWidth
    normalizeNodeInputs
    checkInferedWidth

    //Check
    SpinalInfoPhase("Check Combinational Loops")
    checkCombinationalLoops


    //Simplify nodes
    SpinalInfoPhase("Simplify graph's nodes")
    fillNodeConsumer
    deleteUselessBaseTypes
    simplifyBlacBoxGenerics




    SpinalInfoPhase("Finalise")

    //Name patch
    nameBinding
    simplifyBlackBoxIoNames

    //Finalise
    addNodeIntoComponent
    removeEmptyComponent
    allocateNames

    printStates


    def nameNodesByReflection(): Unit = {
      if (topLevel.getName() == null) topLevel.setWeakName("toplevel")
      for (c <- sortedComponents) {
        c.nameElements()
        nameComponentDeclaration(c)
      }

    }


    def printStates: Unit = {
      var counter = 0
      walkNodes2(_ => counter = counter + 1)
      SpinalInfo(s"Graph has $counter nodes")
    }

    def nameBinding: Unit = {
      for (c <- components) {
        for ((bindedOut, bind) <- c.outBindingHosted) {
          if (bindedOut.component.isNamed && bindedOut.isNamed) {
            bind.setWeakName(bindedOut.component.getName() + "_" + bindedOut.getName())
          }
        }
      }

      walkNodes2(node => node match {
        case node: BaseType => {
          if (node.isInput && node.inputs(0) != null && node.inputs(0).isInstanceOf[Nameable]) {
            val nameable = node.inputs(0).asInstanceOf[Nameable]
            if (nameable.isUnnamed && node.component.isNamed && node.isNamed) {
              nameable.setWeakName(node.component.getName() + "_" + node.getName())
            }
          }
        }
        case _ =>
      })
    }


    def check_noNull_noCrossHierarchy_noInputRegister: Unit = {
      val errors = mutable.ArrayBuffer[String]()
      walkNodes2(node => {

        node match {
          case node: BaseType => {
            val in = node.inputs(0)
            if (in != null) {
              if (node.isInput && in.isInstanceOf[Reg] && in.component == node.component) {
                errors += s"Input register are not allowed $node"
              } else {
                val inIsIo = in.isInstanceOf[BaseType] && in.asInstanceOf[BaseType].isIo
                if (node.isIo) {
                  if (node.isInput) {
                    if (in.component != node.component.parent && !(!in.component.isTopLevel && inIsIo && in.component.parent == node.component.parent))
                      errors += s"Input $node is not assigned by parent component but an other"
                  } else if (node.isOutput) {
                    if (in.component != node.component && !(inIsIo && node.component == in.component.parent))
                      errors += s"Output $node is not assigned by his component but an other"
                  } else errors += s"No direction specified on IO $node"
                } else {
                  if (in.component != node.component && !(inIsIo && node.component == in.component.parent))
                    errors += s"Node $node is assigned outside his component "
                }
              }
            } else {
              if (!(node.isInput && node.component.isTopLevel) && !(node.isOutput && node.component.isInstanceOf[BlackBox]))
                errors += s"No driver on $node"
            }
          }
          case _ => {
            for (in <- node.inputs) {
              if (in == null) {
                errors += s"No driver on $node"
              } else {
                if (in.component != node.component && !(in.isInstanceOf[BaseType] && in.asInstanceOf[BaseType].isIo && node.component == in.component.parent))
                  errors += s"Node is drived outside his component $node"
              }
            }
          }
        }
      })
      if (!errors.isEmpty)
        SpinalError(errors)
    }

    def allocateNames = {
      for (c <- sortedComponents) {
        addReservedKeyWord(c.localScope)
        c.allocateNames

        if (c.isInstanceOf[BlackBox])
          globalScope.lockName(c.definitionName)
        else
          c.definitionName = globalScope.allocateName(c.definitionName)
      }
    }

    def configureComponentIo(): Unit = {
      components.foreach(_.io.flatten.foreach(_._2.isIo = true))
    }

    def normalizeNodeInputs: Unit = {
      walkNodes(walker_matchWidth)
    }
    def addInOutBinding: Unit = {
      walkNodes(walker_addBinding)
    }

    /*
        def moveInputRegisterToParent: Unit = {
          walkNodes2((node) => {
            node match {
              case regSignal: BaseType => {
                if (regSignal.isReg && regSignal.isInput) {
                  val reg = regSignal.inputs(0)
                  val regSignalClone = regSignal.clone
                  reg.component = regSignal.component.parent
                  regSignalClone.inputs(0) = reg
                  regSignalClone.component = reg.component
                  regSignal.inputs(0) = regSignalClone
                }
              }
              case _=>
            }
          })
        }
    */

    def pullClockDomains: Unit = {
      walkNodes(walker_pullClockDomains)
    }

    def deleteUselessBaseTypes: Unit = {
      walkNodes(walker_deleteUselessBaseTypes)
    }

    def removeEmptyComponent = {

      sortedComponents = sortedComponents.filter(c => {
        if (c.nodes == null) {
          //unused component
          if (c.parent != null) c.parent.kinds -= c
          false
        } else if (c.isInstanceOf[BlackBox]) {
          false
        } else {
          true
        }
      })
      components.clear()
      components ++= sortedComponents
    }
    def fillNodeConsumer: Unit = {
      walkNodes(walker_nodeConsumer)
    }

    def nameComponentDeclaration(c: Component): Unit = {
      c.definitionName = c.getClass.getSimpleName
    }


    def checkInferedWidth: Unit = {
      Node.widthInferredCheck.foreach(_())
      val errors = mutable.ArrayBuffer[String]()
      walkNodes2(node => {
        node match {
          case extract: ExtractBool => {
            extract.bitId match {
              case lit: IntLiteral => {
                if (lit.value < 0 || lit.value >= extract.bitVector.getWidth) {
                  errors += s"Static bool extraction (bit ${lit.value}) is outside the range (${extract.bitVector.getWidth - 1} downto 0) of ${extract.bitVector}"
                }
              }
              case _ =>
            }

          }
          case extract: ExtractBitsVector => {
            val hi = extract.bitIdHi.value
            val lo = extract.bitIdLo.value
            val width = extract.bitVector.getWidth
            if (hi >= width || lo < 0) {
              errors += s"Static bits extraction ($hi downto $lo) is outside the range (${width - 1} downto 0) of ${extract.bitVector}"
            }

          }
          case _ =>
        }
      })
      if (!errors.isEmpty)
        SpinalError(errors)
    }




    def walkNodesDefautStack = {
      val nodeStack = mutable.Stack[Node]()

      topLevel.getNodeIo.foreach(nodeStack.push(_))
      components.foreach(c => {
        c match {
          case blackBox: BlackBox => blackBox.getNodeIo.filter(_.isInput).foreach(nodeStack.push(_))
          case _ =>
        }
        c.additionalNodesRoot.foreach(nodeStack.push(_))
      })
      nodeStack
    }

    def walkNodes(walker: (Node, mutable.Stack[Node]) => Unit, nodeStack: mutable.Stack[Node] = walkNodesDefautStack): Unit = {
      val walkedNodes = mutable.Set[Node]()


      while (!nodeStack.isEmpty) {
        val pop = nodeStack.pop()
        if (pop != null && walkedNodes.contains(pop) == false) {
          walker(pop, nodeStack)
          walkedNodes += pop
        }

      }

      def addNodeToStack(node: Node): Unit = {
        nodeStack.push(node)
      }
    }

    def walkNodes2(walker: (Node) => Unit, nodeStack: mutable.Stack[Node] = walkNodesDefautStack): Unit = {
      walkNodes((node, stack) => {
        walker(node)
        node.inputs.foreach(stack.push(_))
      }, nodeStack)
    }

    def walkNodesBlackBoxGenerics = {
      val nodeStack = mutable.Stack[Node]()
      components.foreach(_ match {
        case blackBox: BlackBox => {
          blackBox.generic.flatten.foreach(tuple => nodeStack.push(tuple._2))
        }
        case _ =>
      })
      nodeStack
    }


    def inferWidth: Unit = {
      Node.areInferringWidth = true
      val nodes = ArrayBuffer[Node]()
      walkNodes2(nodes += _, walkNodesDefautStack ++ walkNodesBlackBoxGenerics)


      def checkAll: Unit = {
        val errors = mutable.ArrayBuffer[String]()
        for (node <- nodes) {
          if (node.inferWidth || node.inferWidth) {
            node match {
              case baseType: BaseType =>
                errors += s"Can't infer width on $node"
              case _ =>
            }
          }
        }
        if (!errors.isEmpty)
          SpinalError(errors)
      }

      var iterationCounter = 0
      while (true) {
        iterationCounter = iterationCounter + 1
        var somethingChange = false
        for (node <- nodes) {
          val hasChange = node.inferWidth

          somethingChange = somethingChange || hasChange
        }

        if (!somethingChange || iterationCounter == nodes.size) {
          checkAll
          return
        }
      }
    }



    def allowNodesToReadInputOfKindComponent = {
      walkNodes2(node => {
        for (i <- 0 until node.inputs.size) {
          val input = node.inputs(i)
          input match {
            case baseTypeInput: BaseType => {
              if (baseTypeInput.isInput && baseTypeInput.component.parent == node.component) {
                node.inputs(i) = baseTypeInput.inputs(0)
              }
            }
            case _ =>
          }
        }
      })
    }

    def simplifyBlackBoxIoNames: Unit = {
      for (c <- components) c match {
        case bb: BlackBox => {
          for ((eName, e) <- bb.io.flatten) {
            if (e.isWeak) {
              e.setWeakName(eName.substring(3, eName.size))
            }
          }
          for ((eName, e) <- bb.generic.flatten) {
            if (e.isWeak && eName != "generic") {
              e.setWeakName(eName.substring(8, eName.size))
            }
          }
        }
        case _ =>
      }
    }

    def propagateBaseTypeWidth: Unit = {
      walkNodes2(node => {
        node match {
          case node: BaseType => {
            val width = node.getWidth

            node.inputs(0) match {
              case that: Reg => {
                that.inferredWidth = width
                walk(that.getInitialValue)
                walk(that.getDataInput)
              }
              case _ => walk(node.inputs(0))
            }
            walk(node.inputs(0))

            def walk(that: Node): Unit = that match {
              case that: Multiplexer => {
                that.inferredWidth = width
                walk(that.inputs(1))
                walk(that.inputs(2))
              }
              case _ =>
            }

          }
          case _ =>
        }
      })
    }

    def walker_matchWidth(node: Node, stack: mutable.Stack[Node]): Unit = {
      node.inputs.foreach(stack.push(_))
      node.normalizeInputs
    }

    def checkCombinationalLoops: Unit = {
      val errors = mutable.ArrayBuffer[String]()
      val pendingNodes: mutable.Stack[Node] = walkNodesDefautStack
      val walkedNodes = mutable.Set[Node]()
      val localNodes = mutable.Set[Node]()
      val stack = mutable.Stack[Node]()

      while (!pendingNodes.isEmpty) {
        val pop = pendingNodes.pop()
        if (pop != null && !walkedNodes.contains(pop)) {
          localNodes.clear()
          walk(pop)
        }
      }

      if (!errors.isEmpty)
        SpinalError(errors)

      def addPendingNode(node: Node) = {
        if (!walkedNodes.contains(node)) pendingNodes.push(node)
      }

      def walk(node: Node): Unit = {
        if (node == null) return
        if (node.isInstanceOf[Reg]) {
          val reg = node.asInstanceOf[Reg]
          walkedNodes += node
          addPendingNode(reg.getDataInput)
          addPendingNode(reg.getInitialValue)
          addPendingNode(reg.getClock)
          if(reg.clockDomain.resetKind == ASYNC)
            walk(reg.getReset)
          else
            addPendingNode(reg.getReset)
          addPendingNode(reg.getClockEnable)

        } else {
          if (localNodes.contains(node)) {
            val it = stack.iterator
            val loopStack = mutable.Stack[Node](node)
            var v: Node = null
            do {
              v = it.next
              loopStack.push(v)
            } while (v != node)
            val wellNameLoop = loopStack.reverseIterator.filter(n => n.isInstanceOf[Nameable] && n.asInstanceOf[Nameable].isNamed).map(_.asInstanceOf[Nameable].getName()).reduceLeft(_ + " -> " + _)
            val multiLineLoop = loopStack.reverseIterator.map(n => "      " + n.toString).reduceLeft(_ + "\n" + _)
            errors += s"  Combinational loop ! ${wellNameLoop}\n${multiLineLoop}"
          } else {
            if (!walkedNodes.contains(node)) {
              walkedNodes += node
              localNodes += node
              stack.push(node)
              for (in <- node.inputs) {
                walk(in)
              }
              stack.pop()
              localNodes -= node
            }
          }
        }
      }
    }



    def walker_pullClockDomains(node: Node, stack: mutable.Stack[Node]): Unit = {
      node match {
        case reg: Reg => {
          Component.push(reg.component)
          reg.inputs(2) = reg.clockDomain.readClock
          reg.inputs(3) = reg.clockDomain.readReset
          reg.inputs(4) = reg.clockDomain.readClockEnable
          Component.pop(reg.component)
        }
        case _ =>
      }
      node.inputs.foreach(stack.push(_))
    }

    def walker_nodeConsumer(node: Node, stack: mutable.Stack[Node]): Unit = {
      node.inputs.foreach(n => if (n != null) n.consumers += node)
      node.inputs.foreach(stack.push(_))
    }

    def walker_deleteUselessBaseTypes(node: Node, stack: mutable.Stack[Node]): Unit = {
      node match {
        case node: BaseType => {
          if (node.isUnnamed && !node.isIo && node.consumers.size == 1 && !node.dontSimplify) {
            if (!node.isReg || node.consumers(0).isInstanceOf[BaseType]) {
              val consumerInputs = node.consumers(0).inputs
              val inputConsumer = node.inputs(0).consumers
              for (i <- 0 until consumerInputs.size)
                if (consumerInputs(i) == node)
                  consumerInputs(i) = node.inputs(0)
              inputConsumer -= node
              inputConsumer += node.consumers(0)
            }
          }
        }

        case _ =>
      }
      node.inputs.foreach(stack.push(_))
    }

    def simplifyBlacBoxGenerics: Unit = {
      components.foreach(_ match {
        case blackBox: BlackBox => {
          blackBox.generic.flatten.foreach(tuple => {
            val signal = tuple._2
            walk(signal, signal)
            def walk(node: Node, first: Node): Unit = node match {
              case node: BaseType => {
                first.inputs(0) = node.inputs(0)
                first.inputs(0).inferredWidth = first.inferredWidth
                walk(node.inputs(0), first)
              }
              case lit: Literal =>
              case _ => throw new Exception("BlackBox generic can be literal")
            }
          })
        }
        case _ =>
      })
    }


    /*
        def addOutputsBinding: Unit = {
          for (component <- components) {
            Component.push(component)
            for (kind <- component.kinds) {
              for (output <- kind.getNodeIo if output.isOutput) {
                val outBind = OutBinding(nodeInput, node.component)
                node.inputs(i) = outBind
              }
            }
            Component.pop(component)
          }


          /*
                walkNodes2(node => {
                  node match {*/
        }*/

    def walker_addBinding(node: Node, stack: mutable.Stack[Node]): Unit = {


      if (node.isInstanceOf[BaseType] && node.component.parent != null) {
        val baseType = node.asInstanceOf[BaseType]
        if (baseType.isInput) {
          baseType.inputs(0) match {
            case input: Reg =>
            case input: BaseType => {
              input.dontSimplifyIt
              //   if(input.isUnnamed) input.setName(baseType.component.getName() + "_" + baseType.getName())
            }
            case _ => {
              val inBinding = baseType.clone
              inBinding.inputs(0) = baseType.inputs(0)
              baseType.inputs(0) = inBinding
              inBinding.dontSimplifyIt
              inBinding.component = node.component.parent
              //inBinding.setName(baseType.component.getName() + "_" + baseType.getName())
            }
          }

        }
      }

      node.inputs.foreach(stack.push(_))

      for (i <- 0 until node.inputs.size) {
        val nodeInput = node.inputs(i)
        nodeInput match {
          case nodeInput: BaseType => {
            if (nodeInput.isIo && nodeInput.isOutput && (nodeInput.component.parent == node.component || (nodeInput.component.parent == node.component.parent && nodeInput.component != node.component))) {
              val outBind = OutBinding(nodeInput, nodeInput.component.parent)
              node.inputs(i) = outBind
            }

            node.inputs(i) = getFirstReadableNode(node.inputs(i), node.component)

            def getFirstReadableNode(node: Node, component: Component): Node = node match {
              case baseType: BaseType => {
                if (baseType.isIo && baseType.isOutput && baseType.component == component) {
                  getFirstReadableNode(baseType.inputs(0), component)
                } else {
                  baseType
                }
              }
              case n: Node => n
            }
          }
          case _ =>
        }
      }

    }

    def addNodeIntoComponent: Unit = {
      walkNodes2(node => {
        val comp = node.component
        if (comp.nodes == null) comp.nodes = new ArrayBuffer[Node]
        comp.nodes += node
      })
    }

    def addComponent(c: Component): Unit = {
      components += c
      c.kinds.foreach(addComponent(_))
    }

  }


  def emitReference(node: Node): String = {
    node match {
      case n: Nameable => {
        n.getNameElseThrow
      }

    }

    //THROW
  }

}
