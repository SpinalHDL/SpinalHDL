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
  var jsonReportPath = ""
  var defaultClockDomainFrequancy : IClockDomainFrequency = UnknownFrequency()

  def addReservedKeyWordToScope(scope: Scope): Unit = {
    reservedKeyWords.foreach(scope.allocateName(_))
  }


  def elaborate[T <: Component](gen: () => T): BackendReport[T] = {


    //default clockDomain
    val defaultClockDomain = ClockDomain("",defaultClockDomainFrequancy)

    ClockDomain.push(defaultClockDomain)
    topLevel = gen()
//    if(topLevel.isUnnamed)topLevel.setWeakName("toplevel")
    ClockDomain.pop(defaultClockDomain)


    def checkGlobalData: Unit = {
      if (!GlobalData.get.clockDomainStack.isEmpty) SpinalWarning("clockDomain stack is not empty :(")
      if (!GlobalData.get.componentStack.isEmpty) SpinalWarning("componentStack stack is not empty :(")
      if (!GlobalData.get.switchStack.isEmpty) SpinalWarning("switchStack stack is not empty :(")
      if (!GlobalData.get.whenStack.isEmpty) SpinalWarning("whenStack stack is not empty :(")
    }
    checkGlobalData
    val ret = elaborate(topLevel.asInstanceOf[T])
    checkGlobalData


    globalData.postBackendTask.foreach(_())
    val reports = globalData.jsonReports


    val builder = new mutable.StringBuilder
    builder ++= "{\n"
    builder ++= "\"reports\":[\n"
    if(globalData.jsonReports.size != 0) {
      builder ++= "  ";
      builder ++= globalData.jsonReports.reduceLeft(_ + ",\n" + _).replace("\n", "\n  ")
    }
    builder ++= "  ]\n"
    builder ++= "}\n"
    val out = new java.io.FileWriter(jsonReportPath + ".json")
    out.write(builder.toString())
    out.flush();
    out.close();


    ret
  }

  //TODO bundle element disable method
  //TODO General cleaning
  //TODO better VHDL package, less function
  //TODO SyncNodes could return a latency per input ?
  //TODO switch case nodes in replacement of when emulation
  //TODO Union support
  //TODO 1 bit * 3 bit => 3 bit in place of 4 bit ?
  //TODO function to check if 2 clocks are drived from same signal and apply it to mem blackbox inference and cross clock checker(rd.getClockDomain.clock == wr.getClockDomain.clock)
  protected def elaborate[T <: Component](topLevel: T): BackendReport[T] = {
    SpinalInfoPhase("Start analysis and transform")

    remplaceMemByBlackBox_simplifyWriteReadWithSameAddress

    addReservedKeyWordToScope(globalScope)
    addComponent(topLevel)
    sortedComponents = components.sortWith(_.level > _.level)



    //notifyNodes(StartPhase)

    SpinalInfoPhase("Get names from reflection")
    nameNodesByReflection
    collectAndNameEnum

    //Component connection
    SpinalInfoPhase("Transform connection")
    // allowLiteralToCrossHierarchy
    pullClockDomains
    check_noNull_noCrossHierarchy_noInputRegister_noDirectionLessIo

    addInOutBinding
    allowNodesToReadOutputs
    allowNodesToReadInputOfKindComponent


    //Node width
    SpinalInfoPhase("Infer nodes's bit width")
    inferWidth
    simplifyNodes
    propagateBaseTypeWidth
    normalizeNodeInputs
    checkInferedWidth


    //Check
    SpinalInfoPhase("Check combinational loops")
    checkCombinationalLoops
    SpinalInfoPhase("Check that there is no incomplet assignement")
    check_noAsyncNodeWithIncompletAssignment
    SpinalInfoPhase("Check cross clock domains")
    checkCrossClockDomains


    //Simplify nodes
    SpinalInfoPhase("Simplify graph's nodes")
    fillNodeConsumer
    dontSymplifyBasetypeWithComplexAssignement
    deleteUselessBaseTypes
    simplifyBlacBoxGenerics


    SpinalInfoPhase("Finalise")

    //Name patch
    nameBinding
    //simplifyBlackBoxIoNames

    //Finalise
    addNodesIntoComponent
    orderComponentsNodes
    allocateNames
    removeComponentThatNeedNoHdlEmit


    printStates


    new BackendReport(topLevel)

  }

  def nameNodesByReflection(): Unit = {
    globalData.nodeAreNamed = true
    if (topLevel.getName() == null) topLevel.setWeakName("toplevel")
    for (c <- sortedComponents) {
      c.nameElements()
      nameComponentDeclaration(c)
      c match {
        case bb: BlackBox => {
          bb.getGeneric.genNames
        }
        case _ =>
      }
    }



  }


  //TODO  more
  def remplaceMemByBlackBox_simplifyWriteReadWithSameAddress: Unit = {
    class MemTopo(val mem: Mem[_]) {
      val writes = ArrayBuffer[MemWrite]()
      val readsAsync = ArrayBuffer[MemReadAsync]()
      val readsSync = ArrayBuffer[MemReadSync]()
      val writeReadSync = ArrayBuffer[(MemWrite, MemReadSync)]()
      val writeOrReadSync = ArrayBuffer[(MemWriteOrRead_writePart, MemWriteOrRead_readPart)]()
    }
    val memsTopo = mutable.Map[Mem[_], MemTopo]()

    def topoOf(mem: Mem[_]) = memsTopo.getOrElseUpdate(mem, new MemTopo(mem))

    walkNodes2(node => node match {
      case write: MemWrite => {
        val memTopo = topoOf(write.getMem)
        val readSync = memTopo.readsSync.find(readSync => readSync.originalAddress == write.originalAddress).getOrElse(null)
        if (readSync == null) {
          memTopo.writes += write
        } else {
          memTopo.readsSync -= readSync
          memTopo.writeReadSync += (write -> readSync)
          readSync.sameAddressThan(write)
        }
      }
      case readAsync: MemReadAsync => topoOf(readAsync.getMem).readsAsync += readAsync
      case readSync: MemReadSync => {
        val memTopo = topoOf(readSync.getMem)
        val write = memTopo.writes.find(write => readSync.originalAddress == write.originalAddress).getOrElse(null)
        if (write == null) {
          memTopo.readsSync += readSync
        } else {
          memTopo.writes -= write
          memTopo.writeReadSync += (write -> readSync)
          readSync.sameAddressThan(write)
        }
      }
      case writePart: MemWriteOrRead_writePart => {
        val memTopo = topoOf(writePart.getMem)
        if (memTopo.writeOrReadSync.count(_._1 == writePart) == 0) {
          memTopo.writeOrReadSync += (writePart -> writePart.readPart)
        }
      }
      case readPart: MemWriteOrRead_readPart => {
        val memTopo = topoOf(readPart.getMem)
        if (memTopo.writeOrReadSync.count(_._2 == readPart) == 0) {
          memTopo.writeOrReadSync += (readPart.writePart -> readPart)
        }
      }
      case _ =>
    })



    for ((mem, topo) <- memsTopo.iterator if forceMemToBlackboxTranslation || mem.forceMemToBlackboxTranslation
                                          if mem.initialContent == null) {

      if (topo.writes.size == 1 && topo.readsAsync.size == 1 && topo.readsSync.size == 0 && topo.writeReadSync.size == 0 && topo.writeOrReadSync.size == 0) {
        val wr = topo.writes(0)
        val rd = topo.readsAsync(0)
        val clockDomain = wr.getClockDomain
        clockDomain.push
        Component.push(mem.component)

        val ram = Component(new Ram_1c_1w_1ra(mem.getWidth, mem.wordCount, rd.writeToReadKind))
        val enable = clockDomain.isClockEnableActive

        ram.io.wr.en := wr.getEnable.allowSimplifyIt() && enable
        ram.io.wr.addr := wr.getAddress.allowSimplifyIt()
        ram.io.wr.data := wr.getData.allowSimplifyIt()

        ram.io.rd.addr := rd.getAddress.allowSimplifyIt()
        rd.getData.allowSimplifyIt() := ram.io.rd.data

        ram.setCompositeName(mem)
        Component.pop(mem.component)
        clockDomain.pop
      } else if (topo.writes.size == 1 && topo.readsAsync.size == 0 && topo.readsSync.size == 1 && topo.writeReadSync.size == 0 && topo.writeOrReadSync.size == 0) {
        val wr = topo.writes(0)
        val rd = topo.readsSync(0)
        if (rd.getClockDomain.clock == wr.getClockDomain.clock) {
          val clockDomain = wr.getClockDomain

          clockDomain.push
          Component.push(mem.component)

          val ram = Component(new Ram_1c_1w_1rs(mem.getWidth, mem.wordCount, rd.writeToReadKind))
          val enable = clockDomain.isClockEnableActive

          ram.io.wr.en := wr.getEnable.allowSimplifyIt() && enable
          ram.io.wr.addr := wr.getAddress.allowSimplifyIt()
          ram.io.wr.data := wr.getData.allowSimplifyIt()

          ram.io.rd.en := rd.getEnable.allowSimplifyIt() && enable
          ram.io.rd.addr := rd.getAddress.allowSimplifyIt()
          rd.getData.allowSimplifyIt() := ram.io.rd.data

          ram.generic.useReadEnable = {
            val lit = ram.io.rd.en.getLiteral[BoolLiteral]
            lit == null || lit.value == false
          }

          ram.setCompositeName(mem)
          Component.pop(mem.component)
          clockDomain.pop
        }
      } else if (topo.writes.size == 0 && topo.readsAsync.size == 0 && topo.readsSync.size == 0 && topo.writeReadSync.size == 1 && topo.writeOrReadSync.size == 0) {
        val wr = topo.writeReadSync(0)._1
        val rd = topo.writeReadSync(0)._2
        if (rd.getClockDomain.clock == wr.getClockDomain.clock) {
          val clockDomain = wr.getClockDomain

          clockDomain.push
          Component.push(mem.component)

          val ram = Component(new Ram_1wrs(mem.getWidth, mem.wordCount, rd.writeToReadKind))
          val enable = clockDomain.isClockEnableActive

          ram.io.addr := wr.getAddress.allowSimplifyIt()
          ram.io.wr.en := wr.getEnable.allowSimplifyIt() && enable
          ram.io.wr.data := wr.getData.allowSimplifyIt()

          ram.io.rd.en := rd.getEnable.allowSimplifyIt() && enable
          rd.getData.allowSimplifyIt() := ram.io.rd.data

          ram.generic.useReadEnable = {
            val lit = ram.io.rd.en.getLiteral[BoolLiteral]
            lit == null || lit.value == false
          }

          ram.setCompositeName(mem)
          Component.pop(mem.component)
          clockDomain.pop
        }
      } else if (topo.writes.size == 0 && topo.readsAsync.size == 0 && topo.readsSync.size == 0 && topo.writeReadSync.size == 0 && topo.writeOrReadSync.size == 1) {
        val wr = topo.writeOrReadSync(0)._1
        val rd = topo.writeOrReadSync(0)._2
        if (rd.getClockDomain.clock == wr.getClockDomain.clock) {
          val clockDomain = wr.getClockDomain

          clockDomain.push
          Component.push(mem.component)

          val ram = Component(new Ram_1wors(mem.getWidth, mem.wordCount, rd.writeToReadKind))
          val enable = clockDomain.isClockEnableActive

          ram.io.addr := wr.getAddress.allowSimplifyIt()
          ram.io.cs := wr.getChipSelect.allowSimplifyIt() && enable
          ram.io.we := wr.getWriteEnable.allowSimplifyIt()
          ram.io.wrData := wr.getData.allowSimplifyIt()

          rd.getData.allowSimplifyIt() := ram.io.rdData

          ram.setCompositeName(mem)
          Component.pop(mem.component)
          clockDomain.pop
        }
      }
    }
  }

  def printStates: Unit = {
    var counter = 0
    walkNodes2(_ => counter = counter + 1)
    SpinalInfo(s"Graph has $counter nodes")
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


  def check_noAsyncNodeWithIncompletAssignment: Unit = {


    val errors = mutable.ArrayBuffer[String]()

    walkNodes2(node => node match {
      case signal: BaseType if !signal.isDelay => {

        val signalRange = new AssignedRange(signal.getWidth - 1, 0)

        def walk(nodes: ArrayBuffer[Node]): AssignedBits = {
          val assignedBits = new AssignedBits

          for (node <- nodes) node match {
            case wn: WhenNode => {
              assignedBits.add(AssignedBits.intersect(walk(ArrayBuffer(wn.whenTrue)), walk(ArrayBuffer(wn.whenFalse))))
            }
            case an: AssignementNode => {
              assignedBits.add(an.getAssignedBits)
            }
            case man: MultipleAssignmentNode => return walk(man.inputs)
            case nothing: NoneNode =>
            case _ => assignedBits.add(signalRange)
          }
          assignedBits
        }

        val assignedBits = walk(signal.inputs)

        val unassignedBits = AssignedBits()
        unassignedBits.add(signalRange)
        unassignedBits.remove(assignedBits)
        if (!unassignedBits.isEmpty)
          errors += s"Incomplet assignment is detected on $signal, unassigned bit mask is ${unassignedBits.value.toString(2)}}, declared at ${signal.getScalaLocationString}"


      }
      case _ =>
    })

    if (!errors.isEmpty)
      SpinalError(errors)
  }

  //clone is to week, lose tag and don't symplify :(
  def allowLiteralToCrossHierarchy: Unit = {
    walkNodes2(consumer => {
      for (consumerInputId <- 0 until consumer.inputs.size) {
        val consumerInput = consumer.inputs(consumerInputId)
        consumerInput match {
          case litBaseType: BaseType if litBaseType.inputs(0).isInstanceOf[Literal] => {
            val lit: Literal = litBaseType.inputs(0).asInstanceOf[Literal]
            val c = if (consumer.isInstanceOf[BaseType] && consumer.asInstanceOf[BaseType].isInput) consumer.component.parent else consumer.component
            Component.push(c)
            val newBt = litBaseType.clone()
            val newLit: Literal = lit.clone()

            newBt.inputs(0) = newLit
            consumer.inputs(consumerInputId) = newBt
            Component.pop(c)

          }
          case _ =>
        }
      }
    })
  }

  def check_noNull_noCrossHierarchy_noInputRegister_noDirectionLessIo: Unit = {
    val errors = mutable.ArrayBuffer[String]()


    for(c <- components){
      try{
        val io = c.reflectIo
        for(bt <- io.flatten){
          if(bt.isDirectionLess){
            errors += s"Direction less signal into io def ${bt.getScalaLocationString}"
          }
        }
      }catch{
        case _ : Throwable =>
      }

    }


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
                  if (in.component != node.component.parent && !(!in.component.isTopLevel && inIsIo && in.component.parent == node.component.parent)) {
                    if (in.component == node.component)
                      errors += s"Input $node can't be assigned from inside at ${node.getScalaTraceString}"
                    else
                      errors += s"Input $node is not assigned by parent component but an other at ${node.getScalaTraceString}"
                  }
                } else if (node.isOutput) {
                  if (in.component != node.component && !(inIsIo && node.component == in.component.parent))
                    errors += s"Output $node is not assigned by his component but an other at ${node.getScalaTraceString}"
                } else
                  errors += s"No direction specified on IO ${node.getScalaLocationString}"
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



  def normalizeNodeInputs: Unit = {
    walkNodes(walker_matchWidth)
  }

  def addInOutBinding: Unit = {
    walkNodes(walker_addInOutBinding)
  }



  def pullClockDomains: Unit = {
    walkNodes(walker_pullClockDomains)
  }

  def dontSymplifyBasetypeWithComplexAssignement: Unit = {
    walkNodes2(node => {
      node match {
        case baseType: BaseType => {
          baseType.inputs(0) match {
            case wn: WhenNode => baseType.dontSimplifyIt()
            case an: AssignementNode => baseType.dontSimplifyIt()
            case man: MultipleAssignmentNode => baseType.dontSimplifyIt()
            case _ =>
          }
        }
        case _ =>
      }
    })
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
    if(c.definitionName == null)
      c.definitionName = c.getClass.getSimpleName
  }


  def checkInferedWidth: Unit = {
    val errors = mutable.ArrayBuffer[String]()
    walkNodes2(node => {
      val error = node.checkInferedWidth
      if (error != null) errors += error
    })

    for(checker <- globalData.widthCheckers){
      val error = checker.check()
      if(error != null) errors += error + s", ${checker.consumer.getWidth} bit assigned by ${checker.provider.getWidth} bit\n  consumer is ${checker.consumer.getScalaLocationString}\n  provider is ${checker.provider.getScalaLocationString}"
    }
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
        blackBox.getGeneric.flatten.foreach(_ match {
          case bt: BaseType => nodeStack.push(bt)
          case _ =>
        })
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
        if (node.inferWidth && !node.isInstanceOf[Reg]) {
          //Don't care about Reg width inference
          errors += s"Can't infer width on ${node.getScalaLocationString}"
        }
        if (node.widthWhenNotInferred != -1 && node.widthWhenNotInferred != node.getWidth) {
          errors += s"getWidth call result during elaboration differ from inferred width on ${node.getScalaLocationString}"
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
            case that: WhenNode => {
              that.inferredWidth = width
              walk(that.whenTrue)
              walk(that.whenFalse)
            }
            case that: MultipleAssignmentNode => {
              that.inferredWidth = width
              for (node <- that.inputs) {
                walk(node)
              }
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
              val walked = mutable.Set[Object]() //TODO upgrade it to the check bit by bit
              check(syncInput)
              def check(that: Node): Unit = {
                if(walked.contains(that)) return;
                walked += that
                if (!that.hasTag(crossClockDomain)) {
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
    val partialAssignements = mutable.Map[Node, AssignedBits]() //Case where extract than assign different bits of the same signal
    val extractAssignements = mutable.Map[Node, AssignedBits]()
    while (!pendingNodes.isEmpty) {
      val pop = pendingNodes.pop()
      if (pop != null && !walkedNodes.contains(pop)) {
        localNodes.clear()
        partialAssignements.clear()
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
        //End of comb path
        val syncNode = node.asInstanceOf[SyncNode]
        walkedNodes += node

        syncNode.getSynchronousInputs.foreach(addPendingNode(_))
        syncNode.getAsynchronousInputs.foreach(walk(_))
      } else if (localNodes.contains(node)) {
        //there is a loop !
        val it = stack.iterator
        val loopStack = mutable.Stack[Node](node)
        var v: Node = null
        do {
          v = it.next
          loopStack.push(v)
        } while (v != node)
        val wellNameLoop = loopStack.reverseIterator.filter(n => n.isInstanceOf[Nameable] && n.asInstanceOf[Nameable].isNamed).map(that => that.component.getClass.getSimpleName + "." + that.asInstanceOf[Nameable].getName()).foldLeft("")(_ + _ + " -> ")
        val multiLineLoop = loopStack.reverseIterator.map(n => "      " + n.toString).reduceLeft(_ + "\n" + _)
        errors += s"  Combinational loop ! ${wellNameLoop}\n${multiLineLoop}"
      } else if (!walkedNodes.contains(node)) {
        //Not already walked
        walkedNodes += node
        localNodes += node
        node match {
          case an: AssignementNode => {
            val bv = an.getOutBaseType
            val pa = partialAssignements.getOrElseUpdate(bv, AssignedBits())
            val ab = an.getScopeBits
            pa.add(ab)


            if (extractAssignements.contains(bv)) {
              val notAllowedBits = extractAssignements.get(bv).get
              if (!AssignedBits.intersect(notAllowedBits, ab).isEmpty) {
                continueLocalWith(node.inputs) //Continue => errors come at next iteration (wanted)
              } else {
                //Nothing to do, extract node inputs already walked
              }
            } else {
              continueLocalWith(node.inputs)
            }

            pa.remove(ab)
            if (pa.isEmpty) partialAssignements.remove(bv)
          }
          case extract: Extract => {
            val bv = extract.getBitVector
            val ea = extractAssignements.getOrElseUpdate(bv, AssignedBits())
            val ab = extract.getScopeBits
            ea.add(ab)


            if (partialAssignements.contains(bv)) {
              val notAllowedBits = partialAssignements.get(bv).get
              if (!AssignedBits.intersect(notAllowedBits, ab).isEmpty) {
                continueLocalWith(node.inputs) //Continue => errors come at next iteration (wanted)
              } else {
                continueLocalWith(extract.getParameterNodes)
              }
            } else {
              continueLocalWith(node.inputs)
            }

            ea.remove(ab)
            if (ea.isEmpty) extractAssignements.remove(bv)
          }
          case _ => continueLocalWith(node.inputs)
        }

        def continueLocalWith(inputs: Iterable[Node]): Unit = {
          stack.push(node)
          for (in <- inputs) {
            walk(in)
          }
          stack.pop()
        }

        localNodes -= node
      }
    }
  }


  def walker_pullClockDomains(node: Node, stack: mutable.Stack[Node]): Unit = {
    //  if (!node.isInBlackBoxTree) {
    node match {
      case delay: SyncNode => {
        Component.push(delay.component)
        delay.inputs(SyncNode.getClockInputId) = delay.getClockDomain.readClockWire
        if (delay.isUsingReset)
          delay.inputs(SyncNode.getClockResetId) = delay.getClockDomain.readResetWire
        delay.inputs(SyncNode.getClockEnableId) = delay.getClockDomain.readClockEnableWire
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

  def simplifyNodes: Unit = {
    fillNodeConsumer
    walkNodes2(_.simplifyNode)
    removeNodeConsumer
  }

  def removeNodeConsumer : Unit = {
    walkNodes2(_.consumers.clear())
  }

  def simplifyBlacBoxGenerics: Unit = {
    components.foreach(_ match {
      case blackBox: BlackBox => {
        blackBox.getGeneric.flatten.foreach(tuple => {
          val signal = tuple
          if (signal.isInstanceOf[BaseType]) {
            val baseType = signal.asInstanceOf[BaseType]
            walk(baseType, baseType)
            def walk(node: Node, first: Node): Unit = node match {
              case node: BaseType => {
                first.inputs(0) = node.inputs(0)
                first.inputs(0).inferredWidth = first.inferredWidth
                walk(node.inputs(0), first)
              }
              case lit: Literal =>
              case _ => throw new Exception("BlackBox generic must be literal")
            }
          }

        })
      }
      case _ =>
    })
  }

  //  def simplifyBlacBoxGenerics: Unit = {
  //    components.foreach(_ match {
  //      case blackBox: BlackBox => {
  //        blackBox.generic.flatten.foreach(tuple => {
  //          val signal = tuple._2
  //          walk(signal, signal)
  //          def walk(node: Node, first: Node): Unit = node match {
  //            case node: BaseType => {
  //              first.inputs(0) = node.inputs(0)
  //              first.inputs(0).inferredWidth = first.inferredWidth
  //              walk(node.inputs(0), first)
  //            }
  //            case lit: Literal =>
  //            case _ => throw new Exception("BlackBox generic can be literal")
  //          }
  //        })
  //      }
  //      case _ =>
  //    })
  //  }


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
