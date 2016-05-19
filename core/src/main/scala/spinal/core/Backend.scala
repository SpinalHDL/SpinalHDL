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

class BackendReport[T <: Component](val toplevel: T) {

}

trait Phase{
  def run()
}

class MultiPhase extends Phase{
  val phases = ArrayBuffer[Phase]()

  override def run(): Unit = {
    phases.foreach(_.run)
  }
}

class Backend {
  var globalData = GlobalData.reset
  val components = ArrayBuffer[Component]()
  var sortedComponents = ArrayBuffer[Component]()
  val globalScope = new Scope()
  val reservedKeyWords = mutable.Set[String]()
  var topLevel: Component = null
  val enums = mutable.Map[SpinalEnum,mutable.Set[SpinalEnumEncoding]]()
  var forceMemToBlackboxTranslation = false
  var jsonReportPath = ""
  var defaultClockDomainFrequency : IClockDomainFrequency = UnknownFrequency()

  def addReservedKeyWordToScope(scope: Scope): Unit = {
    reservedKeyWords.foreach(scope.allocateName(_))
  }

  def elaborate[T <: Component](gen: () => T): BackendReport[T] = {
    SpinalInfoPhase("Start elaboration")

    //default clockDomain
    val defaultClockDomain = ClockDomain.external("",frequency = defaultClockDomainFrequency)

    ClockDomain.push(defaultClockDomain)
    topLevel = gen()
//    if(topLevel.isUnnamed)topLevel.setWeakName("toplevel")
    ClockDomain.pop(defaultClockDomain)

    def checkGlobalData() : Unit = {
      if (!GlobalData.get.clockDomainStack.isEmpty) SpinalWarning("clockDomain stack is not empty :(")
      if (!GlobalData.get.componentStack.isEmpty) SpinalWarning("componentStack stack is not empty :(")
      if (!GlobalData.get.switchStack.isEmpty) SpinalWarning("switchStack stack is not empty :(")
      if (!GlobalData.get.conditionalAssignStack.isEmpty) SpinalWarning("conditionalAssignStack stack is not empty :(")
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
    out.flush()
    out.close()

    ret
  }

  //TODO implicit area base class for each base type that implement operator
  //TODO General cleaning
  //TODO switch case nodes in replacement of when emulation
  //TODO Union support
  //TODO better Mem support (user specifyed blackbox)
  //TODO Mux node with n inputs instead of fixed 2
  //TODO better assignement error localisation   (when  otherwise)
  //TODO non bundle that should be bundle into a bundle should be warned
  protected def elaborate[T <: Component](topLevel: T): BackendReport[T] = {
    SpinalInfoPhase("Start analysis and transform")
    addReservedKeyWordToScope(globalScope)



    buildComponentsList(topLevel)
    applyComponentIoDefaults()
    walkNodesBlackBoxGenerics()
    replaceMemByBlackBox_simplifyWriteReadWithSameAddress()


    sortedComponents = components.sortWith(_.level > _.level)

    //trickDontCares()

    //notifyNodes(StartPhase)

    SpinalInfoPhase("Get names from reflection")
    nameNodesByReflection()
    collectAndNameEnum()

    //Component connection
    SpinalInfoPhase("Transform connections")
    // allowLiteralToCrossHierarchy
    pullClockDomains()
    check_noNull_noCrossHierarchy_noInputRegister_noDirectionLessIo()

    addInOutBinding()
    allowNodesToReadOutputs()
    allowNodesToReadInputOfKindComponent()


    //Node width
    SpinalInfoPhase("Infer nodes's bit width")
    postWidthInferationChecks()
    inferWidth()
    simplifyNodes()
    inferWidth()
    propagateBaseTypeWidth()
    normalizeNodeInputs()
    checkInferredWidth()

    //Check
    SpinalInfoPhase("Check combinatorial loops")
    checkCombinationalLoops2()
    SpinalInfoPhase("Check cross clock domains")
    checkCrossClockDomains()


    //Simplify nodes
    SpinalInfoPhase("Simplify graph's nodes")
    fillNodeConsumer()
    dontSymplifyBasetypeWithComplexAssignement()
    deleteUselessBaseTypes()
   // convertWhenToDefault()
    SpinalInfoPhase("Check that there is no incomplete assignment")
    check_noAsyncNodeWithIncompleteAssignment()
    simplifyBlacBoxGenerics()

    SpinalInfoPhase("Finalise")

    //Name patch
    nameBinding()
    //simplifyBlackBoxIoNames

    //Finalise
    addNodesIntoComponent()
    orderComponentsNodes()
    allocateNames()
    removeComponentThatNeedNoHdlEmit()

    printStates()

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

  def replaceMemByBlackBox_simplifyWriteReadWithSameAddress(): Unit = {
    class MemTopo(val mem: Mem[_]) {
      val writes = ArrayBuffer[MemWrite]()
      val readsAsync = ArrayBuffer[MemReadAsync]()
      val readsSync = ArrayBuffer[MemReadSync]()
      val writeReadSync = ArrayBuffer[(MemWrite, MemReadSync)]()
      val writeOrReadSync = ArrayBuffer[(MemWriteOrRead_writePart, MemWriteOrRead_readPart)]()
    }
    val memsTopo = mutable.Map[Mem[_], MemTopo]()

    def topoOf(mem: Mem[_]) = memsTopo.getOrElseUpdate(mem, new MemTopo(mem))

    Node.walk(walkNodesDefautStack,node => node match {
      case write: MemWrite => {
        val memTopo = topoOf(write.getMem)
        val readSync = memTopo.readsSync.find(readSync => readSync.originalAddress == write.originalAddress).orNull
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
        val write = memTopo.writes.find(write => readSync.originalAddress == write.originalAddress).orNull
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

      if (topo.writes.size == 1 && topo.readsAsync.size == 1 && topo.readsSync.isEmpty && topo.writeReadSync.isEmpty && topo.writeOrReadSync.isEmpty) {
        val wr = topo.writes(0)
        val rd = topo.readsAsync(0)
        val clockDomain = wr.getClockDomain
        clockDomain.push()
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
        clockDomain.pop()
      } else if (topo.writes.size == 1 && topo.readsAsync.isEmpty && topo.readsSync.size == 1 && topo.writeReadSync.isEmpty && topo.writeOrReadSync.isEmpty) {
        val wr = topo.writes(0)
        val rd = topo.readsSync(0)
        if (rd.getClockDomain.clock == wr.getClockDomain.clock) {
          val clockDomain = wr.getClockDomain

          clockDomain.push()
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
          clockDomain.pop()
        }
      } else if (topo.writes.isEmpty && topo.readsAsync.isEmpty && topo.readsSync.isEmpty && topo.writeReadSync.size == 1 && topo.writeOrReadSync.isEmpty) {
        val wr = topo.writeReadSync(0)._1
        val rd = topo.writeReadSync(0)._2
        if (rd.getClockDomain.clock == wr.getClockDomain.clock) {
          val clockDomain = wr.getClockDomain

          clockDomain.push()
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
          clockDomain.pop()
        }
      } else if (topo.writes.isEmpty && topo.readsAsync.isEmpty && topo.readsSync.isEmpty && topo.writeReadSync.isEmpty && topo.writeOrReadSync.size == 1) {
        val wr = topo.writeOrReadSync(0)._1
        val rd = topo.writeOrReadSync(0)._2
        if (rd.getClockDomain.clock == wr.getClockDomain.clock) {
          val clockDomain = wr.getClockDomain

          clockDomain.push()
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
          clockDomain.pop()
        }
      }
    }
    components.clear()
    buildComponentsList(topLevel)
  }

  def printStates(): Unit = {
    var counter = 0
    Node.walk(walkNodesDefautStack,_ => counter = counter + 1)
    SpinalInfo(s"Graph has $counter nodes")
  }


  def nameBinding(): Unit = {
    for (c <- components) {
      for ((bindedOut, bind) <- c.kindsOutputsToBindings) {
        if (bind.isUnnamed && bindedOut.component.isNamed && bindedOut.isNamed) {
          bind.setWeakName(bindedOut.component.getName() + "_" + bindedOut.getName())
        }
      }
    }

    Node.walk(walkNodesDefautStack,node => node match {
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


  def collectAndNameEnum(): Unit = {
    Node.walk(walkNodesDefautStack,node => {
      node match {
        case enum: SpinalEnumCraft[_] => enums.getOrElseUpdate(enum.blueprint,mutable.Set[SpinalEnumEncoding]()).add(enum.encoding)
        case _ =>
      }
    })

    for (enumDef <- enums.keys) {
      Misc.reflect(enumDef, (name, obj) => {
        obj match {
          case obj: Nameable => obj.setWeakName(name)
          case _ =>
        }
      })
      for (e <- enumDef.values) {
        if (e.isUnnamed) {
          e.setWeakName("s" + e.position)
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
  def convertWhenToDefault() : Unit = {
    val startContext = mutable.Set[WhenContext]()
    Node.walk(walkNodesDefautStack,node => node match {
      case whenNode : WhenNode => {
        val whenContext = whenNode.w
        if(whenContext.parentElseWhen == null && whenContext.childElseWhen != null) startContext.add(whenContext)
      }
      case _ =>
    })
    val symplifyThem = mutable.Set[WhenContext]()
    for(startContext <- startContext){
      //TODO
//      var ptr = startContext
//      while(ptr.childElseWhen != null){
//        ptr = ptr.childElseWhen
//      }
//      symplifyThem.add(ptr.parentElseWhen)
    }
//    Node.walk(walkNodesDefautStack,node => node match { Patch me, should update consumers ref
//      case whenNode : WhenNode if(symplifyThem.contains(whenNode.w)) => whenNode.inputs(2) = whenNode.inputs(2).inputs(1)
//      case _ =>
//    })
  }

  def check_noAsyncNodeWithIncompleteAssignment(): Unit = {
    val errors = mutable.ArrayBuffer[String]()

    Node.walk(walkNodesDefautStack,node => node match {
      case signal: BaseType if !signal.isDelay => {

        val signalRange = new AssignedRange(signal.getWidth - 1, 0)

        def walk(nodes: ArrayBuffer[Node]): AssignedBits = {
          val assignedBits = new AssignedBits(signal.getBitsWidth)

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

        val unassignedBits = new AssignedBits(signal.getBitsWidth)
        unassignedBits.add(signalRange)
        unassignedBits.remove(assignedBits)
        if (!unassignedBits.isEmpty)
          errors += s"Incomplete assignment is detected on $signal, unassigned bit mask " +
                    s"is ${unassignedBits.toBinaryString}, declared at\n${signal.getScalaLocationLong}"
      }
      case _ =>
    })

    if (errors.nonEmpty)
      SpinalError(errors)
  }

  // Clone is to weak, loses tag and don't symplify :(
  def allowLiteralToCrossHierarchy(): Unit = {
    Node.walk(walkNodesDefautStack,consumer => {
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

  def check_noNull_noCrossHierarchy_noInputRegister_noDirectionLessIo(): Unit = {
    val errors = mutable.ArrayBuffer[String]()

    for(c <- components){
      try{
        val io = c.reflectIo
        for(bt <- io.flatten){
          if(bt.isDirectionLess){
            errors += s"Direction less signal into io def ${bt.getScalaLocationLong}"
          }
        }
      }catch{
        case _ : Throwable =>
      }

    }
    if(!errors.isEmpty)
      SpinalError(errors)

    Node.walk(walkNodesDefautStack,node => {
      node match {
        case node: BaseType => {
          val nodeInput0 = node.inputs(0)
          if (nodeInput0 != null) {
            if (node.isInput && nodeInput0.isInstanceOf[Reg] && nodeInput0.component == node.component) {
              errors += s"Input register are not allowed \n${node.getScalaLocationLong}"
            } else {
              val nodeInput0IsIo = nodeInput0.isInstanceOf[BaseType] && nodeInput0.asInstanceOf[BaseType].isIo
              if (node.isIo) {
                if (node.isInput) {
                  if (nodeInput0.component != node.component.parent && !(!nodeInput0.component.isTopLevel && nodeInput0IsIo && nodeInput0.component.parent == node.component.parent)) {
                    if (nodeInput0.component == node.component)
                      errors += s"Input $node can't be assigned from inside at\n${ScalaLocated.long(node.assignementThrowable)}"
                    else
                      errors += s"Input $node is not assigned by parent component but another at\n${ScalaLocated.long(node.assignementThrowable)}"
                  }
                } else if (node.isOutput) {
                  if (nodeInput0.component != node.component && !(nodeInput0IsIo && node.component == nodeInput0.component.parent))
                    errors += s"Output $node is not assigned by his component but an other at\n${ScalaLocated.long(node.assignementThrowable)}"
                } else
                  errors += s"No direction specified on IO \n${node.getScalaLocationLong}"
              } else {
                if (nodeInput0.component != node.component && !(nodeInput0IsIo && node.component == nodeInput0.component.parent))
                  errors += s"Node $node is assigned outside his component at\n${ScalaLocated.long(node.assignementThrowable)}"
              }
            }
          } else {
            if (!(node.isInput && node.component.isTopLevel) && !(node.isOutput && node.component.isInstanceOf[BlackBox]))
              errors += s"No driver on ${node.getScalaLocationLong}"
          }
        }
        case _ => {
          for ((in,idx) <- node.inputs.zipWithIndex) {
            if (in == null) {
              errors += s"No driver on ${node.getScalaLocationLong}"
            } else {
              if (in.component != node.component && !(in.isInstanceOf[BaseType] && in.asInstanceOf[BaseType].isIo && node.component == in.component.parent)) {
                val throwable = node match{
                  case node : AssignementTreePart => node.getAssignementContext(idx)
                  case _ => node.scalaTrace
                }
                errors += s"Node is driven outside his component \n${ScalaLocated.long(throwable)}"
              }
            }
          }
        }
      }
    })
    if (!errors.isEmpty)
      SpinalError(errors)
  }

  def allocateNames() = {
    for (enumDef <- enums.keys) {
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

  def normalizeNodeInputs(): Unit = {
    Node.walk(walkNodesDefautStack,(node,push) => {
      node.inputs.foreach(push(_))
      node.normalizeInputs
    })
  }

  def addInOutBinding(): Unit = {
    Node.walk(walkNodesDefautStack,(node,push) => {
      if (node.isInstanceOf[BaseType] && node.component.parent != null) {
        val baseType = node.asInstanceOf[BaseType]
        if (baseType.isInput) {
          val inBinding = baseType.clone //To be sure that there is no need of resize between it and node
          inBinding.scalaTrace = baseType.scalaTrace
          inBinding.inputs(0) = baseType.inputs(0)
          baseType.inputs(0) = inBinding
          inBinding.component = node.component.parent
        }
      }

      node.inputs.foreach(push(_))

      for (i <- 0 until node.inputs.size) {
        val nodeInput = node.inputs(i)
        nodeInput match {
          case nodeInput: BaseType => {
            if (nodeInput.isOutput && (nodeInput.component.parent == node.component || (nodeInput.component.parent == node.component.parent && nodeInput.component != node.component))) {
              val into = nodeInput.component.parent
              val bind = into.kindsOutputsToBindings.getOrElseUpdate(nodeInput, {
                val bind = nodeInput.clone
                bind.scalaTrace = nodeInput.scalaTrace
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

    })
  }

  def pullClockDomains(): Unit = {
    Node.walk(walkNodesDefautStack,(node, push) =>  {
      node match {
        case delay: SyncNode => {
          if(delay.isUsingReset && !delay.getClockDomain.hasReset)
              SpinalError(s"Clock domain without reset contain a register which needs one\n ${delay.getScalaLocationLong}")

          Component.push(delay.component)
          delay.inputs(SyncNode.getClockInputId) = delay.getClockDomain.readClockWire

          if (delay.isUsingReset)
            delay.inputs(SyncNode.getClockResetId) = delay.getClockDomain.readResetWire

          delay.inputs(SyncNode.getClockEnableId) = delay.getClockDomain.readClockEnableWire
          Component.pop(delay.component)
        }
        case _ =>
      }
      node.inputs.foreach(push(_))
    })
  }

  def dontSymplifyBasetypeWithComplexAssignement(): Unit = {
    Node.walk(walkNodesDefautStack,node => {
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

  def deleteUselessBaseTypes(): Unit = {
    Node.walk(walkNodesDefautStack,(node, push) => {
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
      node.inputs.foreach(push(_))
    })
  }

  def removeComponentThatNeedNoHdlEmit() = {

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

  def fillNodeConsumer(): Unit = {
    Node.walk(walkNodesDefautStack,(node)=>{
      for(input <- node.inputs){
        if (input != null) input.consumers += node
      }
    })
  }

  def nameComponentDeclaration(c: Component): Unit = {
    if(c.definitionName == null)
      c.definitionName = c.getClass.getSimpleName
  }


  def checkInferredWidth(): Unit = {
    val errors = mutable.ArrayBuffer[String]()
    Node.walk(walkNodesDefautStack,node => {
      val error = node.checkInferedWidth
      if (error != null)
        errors += error
    })

//    for(checker <- globalData.widthCheckers){
//      val error = checker.check()
//      if(error != null)
//        errors += error + s", ${checker.consumer.getWidth} bits assigned by ${checker.provider.getWidth} bits\n  consumer is ${checker.consumer.getScalaLocationString}\n  provider is ${checker.provider.getScalaLocationString}"
//    }
    if (errors.nonEmpty)
      SpinalError(errors)
  }


  def walkNodesDefautStack = {
    val nodeStack = mutable.Stack[Node]()

    topLevel.getAllIo.foreach(nodeStack.push(_))
    components.foreach(c => {
      c match {
        case blackBox: BlackBox => blackBox.getAllIo.filter(_.isInput).foreach(nodeStack.push(_))
        case _ =>
      }
      c.additionalNodesRoot.foreach(nodeStack.push(_))
    })
    nodeStack
  }

  def applyComponentIoDefaults() = {
    Node.walk(walkNodesDefautStack,node => {
      node match{
        case node : BaseType => {
          if(node.inputs(0) == null && node.defaultValue != null){
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
                  node.assignFrom(node.defaultValue, false)
                  Component.pop(c.parent)
                }
                case _ => {
                  Component.push(c)
                  node.assignFrom(node.defaultValue, false)
                  Component.pop(c)
                }
              }
            }
          }
        }
        case _ =>
      }

    })
  }

  def walkNodesBlackBoxGenerics() = {
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


  def postWidthInferationChecks() : Unit = {
    val errors = mutable.ArrayBuffer[String]()
    Node.walk(walkNodesDefautStack ++ walkNodesBlackBoxGenerics,_ match {
      case node : Reg =>{
        if(!node.isUsingReset && node.inputs(RegS.getDataInputId) == node){
          errors += s"$node has no assignement value and no reset value at\n ${node.getScalaLocationLong}"
        }
      }
      case _ =>
    })
    if(!errors.isEmpty)
      SpinalError(errors)
  }

  def inferWidth(): Unit = {
    globalData.nodeAreInferringWidth = true
    val nodes = ArrayBuffer[Node]()
    Node.walk(walkNodesDefautStack ++ walkNodesBlackBoxGenerics,nodes += _)


    def checkAll(): Unit = {
      val errors = mutable.ArrayBuffer[String]()
      for (node <- nodes) {
        if (node.inferWidth && !node.isInstanceOf[Reg]) {
          //Don't care about Reg width inference
          errors += s"Can't infer width on ${node.getScalaLocationLong}"
        }
        if (node.widthWhenNotInferred != -1 && node.widthWhenNotInferred != node.getWidth) {
          errors += s"getWidth call result during elaboration differ from inferred width on ${node.getScalaLocationLong}"
        }
      }
      if (errors.nonEmpty)
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
        checkAll()
        return
      }
    }
  }



  def trickDontCares(): Unit ={
    Node.walk(walkNodesDefautStack,node => {
      node match{
        case baseType : BaseType =>{

        }
        case _ =>
      }
    })
  }

  def allowNodesToReadOutputs(): Unit = {
    val outputsBuffers = mutable.Map[BaseType, BaseType]()
    Node.walk(walkNodesDefautStack,node => {
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

  def allowNodesToReadInputOfKindComponent() = {
    Node.walk(walkNodesDefautStack,node => {
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



  def propagateBaseTypeWidth(): Unit = {
    Node.walk(walkNodesDefautStack,node => {
      node match {
        case node: BaseType => {
          val width = node.getWidth

          node.inputs(0) match {
            case that: Reg => {
              that.inferredWidth = width
              walk(that,RegS.getInitialValueId)
              walk(that,RegS.getDataInputId)
            }
            case _ => walk(node,0)
          }
          walk(node,0)

          def walk(parent: Node,inputId : Int): Unit = {
            val that = parent.inputs(inputId)
            def walkChildren() : Unit = {
              var i = that.inputs.length
              while(i != 0){
                i -= 1
                walk(that,i)
              }
            }
            that match {
              case that: Multiplexer => { //TODO probably useless
                that.inferredWidth = width
                walk(that,1)
                walk(that,2)
              }
              case that: WhenNode => {
                that.inferredWidth = width
                walk(that,1)
                walk(that,2)
              }
              case that: MultipleAssignmentNode => {
                that.inferredWidth = width
                walkChildren()
              }
              case that : AssignementNode => that.inferredWidth = width
              case that: CaseNode => {
                that.inferredWidth = width
                walkChildren()
              }
              case that: SwitchNode => {
                that.inferredWidth = width
                walkChildren()
              }
              case dontCare : DontCareNode =>{
                dontCare.inferredWidth = width
              }
              // case lit : BitsAllToLiteral => lit.inferredWidth = width
              case bitVector : BitVector  => {
                if(bitVector.getWidth < width  && ! bitVector.isReg) {
                  val default = bitVector.spinalTags.find(_.isInstanceOf[TagDefault]).getOrElse(null).asInstanceOf[TagDefault]

                  if (default != null) {
                    val addedBitCount = width - bitVector.getWidth
                    Component.push(bitVector.component)
                    val newOne = bitVector.weakClone
                    newOne.inferredWidth = width
                    if(bitVector.getWidth > 0)
                      newOne(bitVector.getWidth-1,0) := bitVector
                    default.default match {
                      case (_,value : Boolean) =>  {
                        val lit = default.litFacto(if(value) (BigInt(1) << addedBitCount)-1 else 0,addedBitCount)
                        newOne(width-1,bitVector.getWidth).assignFrom(lit,false)
                      }
                      case (_,value : Bool) =>{
                        for(i <- bitVector.getWidth until width)
                          newOne(i) := value
                      }
                    }

                //    newOne.inputs(0).inferredWidth = width
                    parent.inputs(inputId) = newOne
                    Component.pop(bitVector.component)
                  }
                }

              }
              case _ =>
            }
          }

        }
        case _ =>
      }
    })
  }

  def checkCrossClockDomains(): Unit = {
    val errors = mutable.ArrayBuffer[String]()

    Node.walk(walkNodesDefautStack,node => {
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
                if(that == null){
                  println(":(")
                }
                if (!that.hasTag(crossClockDomain)) {
                  that match {
                    case syncDriver: SyncNode => {
                      val driverClockDomain = syncDriver.getClockDomain
                      if (//syncDriver.getClockDomain.clock != consumerCockDomain.clock &&
                          ! driverClockDomain.isSyncronousWith(consumerCockDomain)) {
                        errors += s"Synchronous element ${syncNode.getScalaLocationShort} is driven " +
                          s"by ${syncDriver.getScalaLocationShort} but they don't have the same clock domain. " +
                          s"Register declaration at \n${syncNode.getScalaLocationLong}"
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

  def checkCombinationalLoops2(): Unit = {
    val targetAlgoId = GlobalData.get.algoId
    GlobalData.get.algoId += 1

    val errors = mutable.ArrayBuffer[String]()
    val pendingNodes = mutable.Stack[Node]()
    pendingNodes.pushAll(walkNodesDefautStack)

    def nodeIsCompleted(node: Node) = node.algoId = targetAlgoId
    def isNodeCompleted(node : Node) = node.algoId == targetAlgoId

    while (!pendingNodes.isEmpty) {
      val pop = pendingNodes.pop()
      walk(scala.collection.immutable.HashMap[Node, AssignedBits](),Nil,pop,pop.getWidth-1,0)
    }

    if (!errors.isEmpty)
      SpinalError(errors)

    def walk(consumers :  scala.collection.immutable.HashMap[Node, AssignedBits],stack : List[(Node,Int,Int)],
             node: Node,
              outHi : Int, outLo : Int): Unit = {
      if (node == null || node.component == null || node.isInstanceOf[NoneNode]) {

      }else {
        val newStack = Tuple3(node,outHi,outLo) :: stack
        var bitsAlreadyUsed = consumers.getOrElse(node, new AssignedBits(node.getWidth))
        if (bitsAlreadyUsed.isIntersecting(AssignedRange(outHi, outLo))) {
          val ordred = newStack.reverseIterator
          val filtred = ordred.dropWhile((e) => (e._1 != node || e._2 < outLo || e._3 > outHi)).drop(1).toArray
         // val filtredNode = filtred.map(_._1)

          val wellNameLoop = filtred.reverseIterator.filter{case (n,hi,lo) => n.isInstanceOf[Nameable] && n.asInstanceOf[Nameable].isNamed}.map{case (n,hi,lo)  => n.component.getClass.getSimpleName + "." + n.asInstanceOf[Nameable].getName() + s"[$hi:$lo]"}.foldLeft("")(_ + _ + " -> ")
          val multiLineLoop = filtred.reverseIterator.map(n => "      " + n.toString).reduceLeft(_ + "\n" + _)
          errors += s"  Combinatorial loop ! ${wellNameLoop}\n${multiLineLoop}"
        }else if (!isNodeCompleted(node)) {
          node match {
            case syncNode: SyncNode => {
              nodeIsCompleted(node)
              val newConsumers = consumers + (node -> bitsAlreadyUsed.+(AssignedRange(outHi, outLo)))
              val syncNode = node.asInstanceOf[SyncNode]
              syncNode.getSynchronousInputs.foreach(addPendingNode(_))
              syncNode.getAsynchronousInputs.foreach(i => walk(newConsumers,newStack, i, i.getWidth - 1, 0)) //TODO, pessimistic
            }
            case baseType: BaseType => {
              val consumersPlusFull = consumers + (baseType -> bitsAlreadyUsed.+(AssignedRange(node.getWidth - 1, 0)))
              def walkBaseType(node: Node): Unit = {
                if (node != null) {
                  node match {
                    case node: MultipleAssignmentNode => for (input <- node.inputs) walkBaseType(input)
                    case node: WhenNode => {
                      walk(consumersPlusFull,newStack, node.cond, 0, 0) //Todo, to pessimistic !
                      walkBaseType(node.whenTrue)
                      walkBaseType(node.whenFalse)
                    }
                    case node: AssignementNode => {
                      val newConsumers = consumers + (baseType -> bitsAlreadyUsed.+(node.getScopeBits))
                      var idx = node.inputs.length
                      while (idx != 0) {
                        idx -= 1
                        val input = node.inputs(idx)
                        val (inHi, inLo) = node.getOutToInUsage(idx, outHi, outLo)
                        if (inHi >= inLo) walk(newConsumers,newStack, input, inHi, inLo)
                      }
                    }
                    case _ => {
                      walk(consumersPlusFull,newStack, node, outHi, outLo)
                    }
                  }
                }
              }

              walkBaseType(node.inputs(0))
            }
            case _ => {
              val newConsumers = consumers + (node -> bitsAlreadyUsed.+(AssignedRange(outHi, outLo)))
              var idx = node.inputs.length
              while (idx != 0) {
                idx -= 1
                val input = node.inputs(idx)
                if (input != null) {
                  val (inHi, inLo) = node.getOutToInUsage(idx, outHi, outLo)
                  if (inHi >= inLo) walk(newConsumers,newStack, input, inHi, inLo)
                }
              }
            }
          }
          if (outHi == node.getWidth - 1 && outLo == 0) nodeIsCompleted(node)
        }
      }
    }
    def addPendingNode(node: Node) = {
      if (node != null && ! isNodeCompleted(node)) pendingNodes.push(node)
    }
  }

  def checkCombinationalLoops(): Unit = {
//    val errors = mutable.ArrayBuffer[String]()
//    val pendingNodes: mutable.Stack[Node] = walkNodesDefautStack
//    val walkedNodes = mutable.Set[Node]()
//    val localNodes = mutable.Set[Node]()
//    val stack = mutable.Stack[Node]()
//    val partialAssignements = mutable.Map[Node, AssignedBits]() //Case where extract than assign different bits of the same signal
//    val extractAssignements = mutable.Map[Node, AssignedBits]()
//    while (!pendingNodes.isEmpty) {
//      val pop = pendingNodes.pop()
//      if (pop != null && !walkedNodes.contains(pop)) {
//        localNodes.clear()
//        partialAssignements.clear()
//        walk(pop)
//      }
//    }
//
//    if (!errors.isEmpty)
//      SpinalError(errors)
//
//    def addPendingNode(node: Node) = {
//      if (!walkedNodes.contains(node)) pendingNodes.push(node)
//    }
//
//
//    def walk(node: Node): Unit = {
//      if (node == null) return
//      if (node.isInstanceOf[SyncNode]) {
//        //End of comb path
//        val syncNode = node.asInstanceOf[SyncNode]
//        walkedNodes += node
//
//        syncNode.getSynchronousInputs.foreach(addPendingNode(_))
//        syncNode.getAsynchronousInputs.foreach(walk(_))
//      } else if (localNodes.contains(node)) {
//        //there is a loop !
//        val it = stack.iterator
//        val loopStack = mutable.Stack[Node](node)
//        var v: Node = null
//        do {
//          v = it.next
//          loopStack.push(v)
//        } while (v != node)
//        val wellNameLoop = loopStack.reverseIterator.filter(n => n.isInstanceOf[Nameable] && n.asInstanceOf[Nameable].isNamed).map(that => that.component.getClass.getSimpleName + "." + that.asInstanceOf[Nameable].getName()).foldLeft("")(_ + _ + " -> ")
//        val multiLineLoop = loopStack.reverseIterator.map(n => "      " + n.toString).reduceLeft(_ + "\n" + _)
//        errors += s"  Combinational loop ! ${wellNameLoop}\n${multiLineLoop}"
//      } else if (!walkedNodes.contains(node)) {
//        //Not already walked
//        walkedNodes += node
//        localNodes += node
//        node match {
//          case an: AssignementNode => {
//            val bv = an.getOutBaseType
//            val pa = partialAssignements.getOrElseUpdate(bv, new AssignedBits(bv.getWidth))
//            val ab = an.getScopeBits
//            pa.add(ab)
//
//
//            if (extractAssignements.contains(bv)) {
//              val notAllowedBits = extractAssignements.get(bv).get
//              if (!AssignedBits.intersect(notAllowedBits, ab).isEmpty) {
//                continueLocalWith(node.inputs) //Continue => errors come at next iteration (wanted)
//              } else {
//                //Nothing to do, extract node inputs already walked
//              }
//            } else {
//              continueLocalWith(node.inputs)
//            }
//
//            pa.remove(ab)
//            if (pa.isEmpty) partialAssignements.remove(bv)
//          }
//          case extract: Extract => {
//            val bv = extract.getBitVector
//            val ea = extractAssignements.getOrElseUpdate(bv, new AssignedBits(bv.getWidth))
//            val ab = extract.getScopeBits
//            ea.add(ab)
//
//
//            if (partialAssignements.contains(bv)) {
//              val notAllowedBits = partialAssignements.get(bv).get
//              if (!AssignedBits.intersect(notAllowedBits, ab).isEmpty) {
//                continueLocalWith(node.inputs) //Continue => errors come at next iteration (wanted)
//              } else {
//                continueLocalWith(extract.getParameterNodes)
//              }
//            } else {
//              continueLocalWith(node.inputs)
//            }
//
//            ea.remove(ab)
//            if (ea.isEmpty) extractAssignements.remove(bv)
//          }
//          case _ => continueLocalWith(node.inputs)
//        }
//
//        def continueLocalWith(inputs: Iterable[Node]): Unit = {
//          stack.push(node)
//          for (in <- inputs) {
//            walk(in)
//          }
//          stack.pop()
//        }
//
//        localNodes -= node
//      }
//    }
  }





  def simplifyNodes(): Unit = {
    fillNodeConsumer
    Node.walk(walkNodesDefautStack,_.simplifyNode)
    removeNodeConsumer
  }

  def removeNodeConsumer() : Unit = {
    Node.walk(walkNodesDefautStack,_.consumers.clear())
  }

  def simplifyBlacBoxGenerics(): Unit = {
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

  def addNodesIntoComponent(): Unit = {
    Node.walk({
      val stack = walkNodesDefautStack
      for (c <- components) {
        c.nodes = ArrayBuffer[Node]()
      }
      stack
    },node => {
      node.component.nodes += node
    })
  }

  def orderComponentsNodes(): Unit = {
    for (c <- components) {
      c.nodes = c.nodes.sortWith(_.instanceCounter < _.instanceCounter)
    }
  }

  def buildComponentsList(c: Component): Unit = {
    components += c
    c.kinds.foreach(buildComponentsList(_))
  }

}
