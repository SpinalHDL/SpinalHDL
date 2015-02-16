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


import jdk.nashorn.internal.objects.Global

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC18F on 07.01.2015.
 */



class BackendReport[T <: Component](val topLevel: T) {

}

class Backend {
  var globalData = GlobalData.reset
  val components = ArrayBuffer[Component]()
  var sortedComponents = ArrayBuffer[Component]()
  val globalScope = new Scope()
  val reservedKeyWords = mutable.Set[String]()
  var topLevel: Component = null
  val enums = mutable.Set[SpinalEnum]()
  var forceMemToBlackboxTranslation = false

  def addReservedKeyWordToScope(scope: Scope): Unit = {
    reservedKeyWords.foreach(scope.allocateName(_))
  }


  def elaborate[T <: Component](gen: () => T): BackendReport[T] = {


    //Default clock
    val clock = in.Bool()
    clock.setName("clk")
   // clock.isIo = true
   // BackendToComponentBridge.defaultClock = clock

    //Default reset
    val reset = in.Bool()
    reset.setName("reset")
   // reset.isIo = true
  // BackendToComponentBridge.defaultReset = reset

    //default clockDomain
    val defaultClockDomain = ClockDomain(clock, reset)

    ClockDomain.push(defaultClockDomain)
    topLevel = Component(gen())
    ClockDomain.pop(defaultClockDomain)

    elaborate(topLevel.asInstanceOf[T])
  }

  //TODO
  //TODO ROM support
  //TODO

  protected def elaborate[T <: Component](topLevel: T): BackendReport[T] = {
    SpinalInfoPhase("Start analysis and transform")

    remplaceMemByBlackBox

    addReservedKeyWordToScope(globalScope)
    addComponent(topLevel)
    sortedComponents = components.sortWith(_.level > _.level)



    //notifyNodes(StartPhase)

    SpinalInfoPhase("Get names from reflection")
    nameNodesByReflection
    collectAndNameEnum

    //Component connection
    SpinalInfoPhase("Transform connection")
    pullClockDomains
    check_noNull_noCrossHierarchy_noInputRegister
    addInOutBinding
    allowNodesToReadOutputs
    allowNodesToReadInputOfKindComponent


    //Node width
    SpinalInfoPhase("Infer nodes's bit width")
    inferWidth
    propagateBaseTypeWidth
    normalizeNodeInputs
    checkInferedWidth

    //Check
    SpinalInfoPhase("Check combinational loops")
    checkCombinationalLoops
    SpinalInfoPhase("Check cross clock domains")
    checkCrossClockDomains


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
    addNodesIntoComponent
    orderComponentsNodes
    allocateNames
    removeComponentThatNeedNoHdlEmit


    printStates


    new BackendReport(topLevel)

  }

  def nameNodesByReflection(): Unit = {
    if (topLevel.getName() == null) topLevel.setWeakName("toplevel")
    for (c <- sortedComponents) {
      c.nameElements()
      nameComponentDeclaration(c)
    }

  }


  //  def notifyNodes(phase: BackendPhase) : Unit = {
  //    walkNodes2(node => node.notify(phase))
  //  }

  //TODO  more
  def remplaceMemByBlackBox: Unit = {
    class MemTopo(val mem: Mem[_]) {
      val writes = ArrayBuffer[MemWrite]()
      val readsAsync = ArrayBuffer[MemReadAsync]()
      val readsSync = ArrayBuffer[MemReadSync]()
    }
    val memsTopo = mutable.Map[Mem[_], MemTopo]()

    def topoOf(mem: Mem[_]) = memsTopo.getOrElseUpdate(mem, new MemTopo(mem))

    walkNodes2(node => node match {
      case write: MemWrite => topoOf(write.getMem).writes += write
      case readAsync: MemReadAsync => topoOf(readAsync.getMem).readsAsync += readAsync
      case readSync: MemReadSync => topoOf(readSync.getMem).readsSync += readSync
      case _ =>
    })



    for ((mem, topo) <- memsTopo.iterator if forceMemToBlackboxTranslation || mem.forceMemToBlackboxTranslation) {
      if (topo.writes.size == 1 && topo.readsAsync.size == 1 && topo.readsSync.size == 0) {
        val wr = topo.writes(0)
        val rd = topo.readsAsync(0)
        ClockDomain.push(wr.getClockDomain)
        Component.push(mem.component)
        val ram = Component(new Ram_1c_1w_1ra(mem.getWidth, mem.wordCount, rd.writeToReadKind))

        // ram.io.clk := wr.getClockDomain.readClock

        ram.io.wr.en := wr.getEnable.allowSimplifyIt
        ram.io.wr.addr := wr.getAddress.allowSimplifyIt
        ram.io.wr.data := wr.getData.allowSimplifyIt

        ram.io.rd.addr := rd.getAddress.allowSimplifyIt
        rd.getData.allowSimplifyIt := ram.io.rd.data

        ram.setCompositeName(mem)
        Component.pop(mem.component)
        ClockDomain.pop(wr.getClockDomain)
      } else if (topo.writes.size == 1 && topo.readsAsync.size == 0 && topo.readsSync.size == 1) {
        val wr = topo.writes(0)
        val rd = topo.readsSync(0)
        if (rd.getClockDomain.clock == wr.getClockDomain.clock) {
          ClockDomain.push(wr.getClockDomain)
          Component.push(mem.component)
          val ram = Component(new Ram_1c_1w_1rs(mem.getWidth, mem.wordCount, rd.writeToReadKind))


          //  ram.io.clk := wr.getClockDomain.readClock

          ram.io.wr.en := wr.getEnable.allowSimplifyIt
          ram.io.wr.addr := wr.getAddress.allowSimplifyIt
          ram.io.wr.data := wr.getData.allowSimplifyIt

          ram.io.rd.en := rd.getEnable.allowSimplifyIt
          ram.io.rd.addr := rd.getAddress.allowSimplifyIt
          rd.getData.allowSimplifyIt := ram.io.rd.data

          ram.setCompositeName(mem)
          Component.pop(mem.component)
          ClockDomain.pop(wr.getClockDomain)
        }
      }
    }

  }

  def printStates: Unit = {
    var counter = 0
    walkNodes2(_ => counter = counter + 1)
    //SpinalInfo(s"Graph has $counter nodes")
  }


  def nameBinding: Unit = {
    for (c <- components) {
      for ((bindedOut, bind) <- c.kindsOutputsToBindings) {
        if (bind.isUnnamed && bindedOut.component.isNamed && bindedOut.isNamed) {
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


  def collectAndNameEnum: Unit = {
    walkNodes2(node => {
      node match {
        case enum: SpinalEnumCraft[_] => enums += enum.blueprint
        case _ =>
      }
    })

    for (enumDef <- enums) {
      Misc.reflect(enumDef, (name, obj) => {
        obj match {
          case obj: Nameable => obj.setWeakName(name)
          case _ =>
        }
      })
      for (e <- enumDef.values) {
        if (e.isUnnamed) {
          e.setWeakName("s" + e.id)
        }
      }
      if (enumDef.isWeak) {
        var name = enumDef.getClass.getSimpleName
        if (name.endsWith("$"))
          name = name.substring(0, name.length - 1)
        enumDef.setWeakName(name)
      }
    }
  }


  def check_noNull_noCrossHierarchy_noInputRegister: Unit = {
    val errors = mutable.ArrayBuffer[String]()
    walkNodes2(node => {

      node match {
        case node: BaseType => {
          val in = node.inputs(0)
          if (in != null) {
            if (node.isInput && in.isInstanceOf[Reg] && in.component == node.component) {
              errors += s"Input register are not allowed ${node.getScalaLocationString}"
            } else {
              val inIsIo = in.isInstanceOf[BaseType] && in.asInstanceOf[BaseType].isIo
              if (node.isIo) {
                if (node.isInput) {
                  if (in.component != node.component.parent && !(!in.component.isTopLevel && inIsIo && in.component.parent == node.component.parent))
                    errors += s"Input $node is not assigned by parent component but an other at ${node.getScalaTraceString}"
                } else if (node.isOutput) {
                  if (in.component != node.component && !(inIsIo && node.component == in.component.parent))
                    errors += s"Output $node is not assigned by his component but an other at ${node.getScalaTraceString}"
                } else errors += s"No direction specified on IO ${node.getScalaLocationString}"
              } else {
                if (in.component != node.component && !(inIsIo && node.component == in.component.parent))
                  errors += s"Node $node is assigned outside his component at ${node.getScalaTraceString}"
              }
            }
          } else {
            if (!(node.isInput && node.component.isTopLevel) && !(node.isOutput && node.component.isInstanceOf[BlackBox]))
              errors += s"No driver on ${node.getScalaLocationString}"
          }
        }
        case _ => {
          for (in <- node.inputs) {
            if (in == null) {
              errors += s"No driver on ${node.getScalaLocationString}"
            } else {
              if (in.component != node.component && !(in.isInstanceOf[BaseType] && in.asInstanceOf[BaseType].isIo && node.component == in.component.parent))
                errors += s"Node is drived outside his component ${node.getScalaLocationString}"
            }
          }
        }
      }
    })
    if (!errors.isEmpty)
      SpinalError(errors)
  }

  def allocateNames = {
    for (enumDef <- enums) {
      if (enumDef.isWeak)
        enumDef.setName(globalScope.allocateName(enumDef.getName()));
      else
        globalScope.iWantIt(enumDef.getName())
    }
    for (c <- sortedComponents) {
      addReservedKeyWordToScope(c.localScope)
      c.allocateNames

      if (c.isInstanceOf[BlackBox])
        globalScope.lockName(c.definitionName)
      else
        c.definitionName = globalScope.allocateName(c.definitionName)
    }


  }

  /*def configureComponentIo(): Unit = {
    components.foreach(_.io.flatten.foreach(_._2.isIo = true))
  }*/

  def normalizeNodeInputs: Unit = {
    walkNodes(walker_matchWidth)
  }
  def addInOutBinding: Unit = {
    walkNodes(walker_addInOutBinding)
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

  def removeComponentThatNeedNoHdlEmit = {

    sortedComponents = sortedComponents.filter(c => {
      if (c.isInBlackBoxTree) {
        false
      } else if (c.nodes.size == 0) {
        if (c.parent != null) c.parent.kinds -= c
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
    globalData.nodeWidthInferredCheck.foreach(_())
    val errors = mutable.ArrayBuffer[String]()
    walkNodes2(node => {
      node match {
        case extract: ExtractBool => {
          extract.bitId match {
            case lit: IntLiteral => {
              if (lit.value < 0 || lit.value >= extract.bitVector.getWidth) {
                errors += s"Static bool extraction (bit ${lit.value}) is outside the range (${extract.bitVector.getWidth - 1} downto 0) of ${extract.bitVector} at\n${extract.getScalaTraceString}"
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
            errors += s"Static bits extraction ($hi downto $lo) is outside the range (${width - 1} downto 0) of ${extract.bitVector} at\n${extract.getScalaTraceString}"
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
      if (pop != null && walkedNodes.contains(pop) == false && pop.component != null) {
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
    globalData.nodeAreInferringWidth = true
    val nodes = ArrayBuffer[Node]()
    walkNodes2(nodes += _, walkNodesDefautStack ++ walkNodesBlackBoxGenerics)


    def checkAll: Unit = {
      val errors = mutable.ArrayBuffer[String]()
      for (node <- nodes) {
        if (node.inferWidth || node.inferWidth) {
          node match {
            case baseType: BaseType =>
              errors += s"Can't infer width on ${node.getScalaLocationString}"
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


  def allowNodesToReadOutputs: Unit = {
    val outputsBuffers = mutable.Map[BaseType, BaseType]()
    walkNodes2(node => {
      for (i <- 0 until node.inputs.size) {
        node.inputs(i) match {
          case baseTypeInput: BaseType => {
            if (baseTypeInput.isOutput && baseTypeInput.component.parent != node.component) {
              val buffer = outputsBuffers.getOrElseUpdate(baseTypeInput, {
                val buffer = baseTypeInput.clone()
                buffer.inputs(0) = baseTypeInput.inputs(0)
                baseTypeInput.inputs(0) = buffer
                buffer.component = baseTypeInput.component
                buffer
              })
              node.inputs(i) = buffer
            }
          }
          case _ =>
        }
      }
    })
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

  def checkCrossClockDomains: Unit = {
    val errors = mutable.ArrayBuffer[String]()
    walkNodes2(node => {
      node match {
        case syncNode: SyncNode => {
          if (!syncNode.hasTag(crossClockDomain)) {
            val consumerCockDomain = syncNode.getClockDomain
            for (syncInput <- syncNode.getSynchronousInputs) {
              check(syncInput)
              def check(that: Node): Unit = {
                that match {
                  case syncDriver: SyncNode => {
                    if (syncDriver.getClockDomain.clock != consumerCockDomain.clock) {
                      errors += s"Synchronous element ${syncNode.getScalaLocationStringShort} is drived by ${syncDriver.getScalaLocationStringShort} but they don't have the same clock domain. Register declaration at\n${syncNode.getScalaTraceString}"
                    }
                  }
                  case _ => that.inputs.foreach(input => if (input != null) check(input))
                }
              }
            }
          }
        }
        case _ =>
      }
    })

    if (!errors.isEmpty)
      SpinalError(errors)
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

      if (node.isInstanceOf[SyncNode]) {
        val syncNode = node.asInstanceOf[SyncNode]
        walkedNodes += node

        syncNode.getSynchronousInputs.foreach(addPendingNode(_))
        syncNode.getAsynchronousInputs.foreach(walk(_))
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
    //  if (!node.isInBlackBoxTree) {
    node match {
      case delay: SyncNode => {
        Component.push(delay.component)
        delay.inputs(SyncNode.getClockInputId) = delay.getClockDomain.readClock
        if (delay.isUsingReset)
          delay.inputs(SyncNode.getClockResetId) = delay.getClockDomain.readReset
        delay.inputs(SyncNode.getClockEnableId) = delay.getClockDomain.readClockEnable
        Component.pop(delay.component)
      }
      case _ =>
    }
    //  }
    node.inputs.foreach(stack.push(_))
  }

  def walker_nodeConsumer(node: Node, stack: mutable.Stack[Node]): Unit = {
    node.inputs.foreach(n => if (n != null) n.consumers += node)
    node.inputs.foreach(stack.push(_))
  }

  def walker_deleteUselessBaseTypes(node: Node, stack: mutable.Stack[Node]): Unit = {
    node match {
      case node: BaseType => {
        if (node.isUnnamed && !node.isIo && node.consumers.size == 1 && node.canSymplifyIt) {
          val consumer = node.consumers(0)
          val input = node.inputs(0)
          if (!node.isDelay || consumer.isInstanceOf[BaseType]) {
            // don't allow to put a non base type on component inputs
            if (input.isInstanceOf[BaseType] || !consumer.isInstanceOf[BaseType] || !consumer.asInstanceOf[BaseType].isInput) {
              //don't allow to jump from kind to kind
              val isKindOutputBinding = node.component.kindsOutputsBindings.contains(node)
              if (!(isKindOutputBinding && (!consumer.isInstanceOf[BaseType] || node.component == consumer.component.parent))) {
                val consumerInputs = consumer.inputs
                val inputConsumer = input.consumers

                if (isKindOutputBinding) {
                  val newBind = consumer.asInstanceOf[BaseType]
                  node.component.kindsOutputsBindings += newBind
                  node.component.kindsOutputsToBindings += (input.asInstanceOf[BaseType] -> newBind)
                }

                for (i <- 0 until consumerInputs.size)
                  if (consumerInputs(i) == node)
                    consumerInputs(i) = input
                inputConsumer -= node
                inputConsumer += consumer
              }
            }
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


  def walker_addInOutBinding(node: Node, stack: mutable.Stack[Node]): Unit = {
    if (node.isInstanceOf[BaseType] && node.component.parent != null) {
      val baseType = node.asInstanceOf[BaseType]
      if (baseType.isInput) {
        val inBinding = baseType.clone //To be sure that there is no need of resize between it and node
        inBinding.inputs(0) = baseType.inputs(0)
        baseType.inputs(0) = inBinding
        inBinding.component = node.component.parent
      }
    }

    node.inputs.foreach(stack.push(_))

    for (i <- 0 until node.inputs.size) {
      val nodeInput = node.inputs(i)
      nodeInput match {
        case nodeInput: BaseType => {
          if (nodeInput.isIo && nodeInput.isOutput && (nodeInput.component.parent == node.component || (nodeInput.component.parent == node.component.parent && nodeInput.component != node.component))) {
            val into = nodeInput.component.parent
            val bind = into.kindsOutputsToBindings.getOrElseUpdate(nodeInput, {
              val bind = nodeInput.clone
              into.kindsOutputsToBindings.put(nodeInput, bind)
              into.kindsOutputsBindings += bind
              bind.component = into
              bind.inputs(0) = nodeInput
              bind
            })

            node.inputs(i) = bind
          }
        }
        case _ =>
      }
    }

  }

  def addNodesIntoComponent: Unit = {
    walkNodes2(node => {
      node.component.nodes += node
    }, {
      val stack = walkNodesDefautStack
      for (c <- components) {
        c.nodes = ArrayBuffer[Node]()
      }
      stack
    })
  }


  def orderComponentsNodes: Unit = {
    for (c <- components) {
      c.nodes = c.nodes.sortWith(_.instanceCounter < _.instanceCounter)
    }
  }


  def addComponent(c: Component): Unit = {
    components += c
    c.kinds.foreach(addComponent(_))
  }

}
