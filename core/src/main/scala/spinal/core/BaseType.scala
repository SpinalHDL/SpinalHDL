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

/**
  * Created by PIC18F on 21.08.2014.
  */

trait BaseTypeFactory extends BoolFactory with BitsFactory with UIntFactory with SIntFactory with VecFactory

trait BaseTypeCast extends BoolCast with UIntCast with SIntCast with BitsCast with SFixCast with UFixCast

object BaseType {
  def checkAssignability(dst : BaseType,src : Node) : Unit = {
    val globalData = dst.globalData
    dst.dir match{
      case null => if(globalData.componentStack.head() != dst.component)
        SpinalError(s"Signal $dst can't be assigned outside his component => \n${ScalaLocated.long}")
      case `in` => if(!(src.component == dst.component.parent || (dst.component.parent == src.component.parent && src.isInstanceOf[BaseType] && src.asInstanceOf[BaseType].isOutput)))
        SpinalError(s"Input signal $dst can't be assigned from there => \n${ScalaLocated.long}")
      case `out` => if(globalData.componentStack.head() != dst.component)
        SpinalError(s"Output signal $dst can't be assigned from there => \n${ScalaLocated.long}")
    }
  }

  def walkWhenNodes(baseType: BaseType, initialConsumer: Node, initialConsumerInputId: Int, conservative: Boolean = false) = {

    var consumer = initialConsumer
    var consumerInputId: Int = initialConsumerInputId
    val globalData = baseType.globalData
    var initialConditionalAssignHit = baseType.conditionalAssignScope == null

    def initMan(man: MultipleAssignmentNode, that: Node): Unit = {
      if(consumer.isInstanceOf[AssignementTreePart]){
        man.setAssignementContext(0,consumer.asInstanceOf[AssignementTreePart].getAssignementContext(consumerInputId))
      }
      //To be sure that there is basetype to bufferise (for future resize)
      if (that.isInstanceOf[WhenNode] || that.isInstanceOf[BaseType] || that.isInstanceOf[AssignementNode] ||
          that.isInstanceOf[MultipleAssignmentNode] || that.isInstanceOf[Reg]) {
        man.inputs += that
      } else {
        val bt = baseType.weakClone
        bt.setInputWrap(0) = that
        man.inputs += bt
      }
    }




    for (conditionalAssign <- globalData.conditionalAssignStack.stack.reverseIterator) {
      if (!initialConditionalAssignHit) {
        if (conditionalAssign == baseType.conditionalAssignScope) initialConditionalAssignHit = true
      } else {
        conditionalAssign match {
          case when: WhenContext => {
            consumer.getInput(consumerInputId) match {
              case nothing@(null | _: NoneNode) => {
                val whenNode = WhenNode(when)
                if(consumer.isInstanceOf[AssignementTreePart]){
                  consumer.asInstanceOf[AssignementTreePart].setAssignementContext(consumerInputId,globalData.getThrowable())
                }
                consumer.setInputWrap(consumerInputId) = whenNode
                consumer = whenNode
              }
              case man: MultipleAssignmentNode => {
                man.inputs.last match {
                  case whenNode: WhenNode if whenNode.w == when => consumer = whenNode
                  case _ => {
                    val whenNode = WhenNode(when)
                    man.inputs += whenNode
                    consumer = whenNode
                  }
                }
              }
              case whenNode: WhenNode if whenNode.w == when => consumer = whenNode
              case that => {
                val man = new MultipleAssignmentNode
                val whenNode = WhenNode(when)
                initMan(man, that)
                man.inputs += whenNode
                consumer.setInputWrap(consumerInputId) = man
                consumer = whenNode
              }
            }

            consumerInputId = if (when.isTrue) 1 else 2
          }

//          case context: SwitchContext => {
//            consumer.getInput(consumerInputId) match {
//              case nothing@(null | _: NoneNode) => {
//                val switchNode = new SwitchNode(context)
//                consumer.setInputWrap(consumerInputId) = switchNode
//                consumer = switchNode
//              }
//              case man: MultipleAssignmentNode => {
//                man.inputs.last match {
//                  case currentContext: SwitchNode if currentContext.context == context => consumer = currentContext
//                  case _ => {
//                    val switchNode = new SwitchNode(context)
//                    man.inputs += switchNode
//                    consumer = switchNode
//                  }
//                }
//              }
//              case currentContext: SwitchNode if currentContext.context == context => consumer = currentContext
//              case that => {
//                val man = new MultipleAssignmentNode
//                val switchNode = new SwitchNode(context)
//                initMan(man, that)
//                man.inputs += switchNode
//                consumer.setInputWrap(consumerInputId) = man
//                consumer = switchNode
//              }
//            }
//          }

//          case context: CaseContext => {
//            if (consumer.getInputsCount == 0) {
//              val caseNode = new CaseNode(context)
//              consumer.inputs += caseNode
//              consumer = caseNode
//            } else {
//              val last = consumer.inputs.last.asInstanceOf[CaseNode]
//              if (last.context != context) {
//                val caseNode = new CaseNode(context)
//                consumer.inputs += caseNode
//                consumer = caseNode
//              } else {
//                consumer = last
//              }
//            }
//            consumerInputId = 1
//          }
        }
      }
    }
    if (!initialConditionalAssignHit)
      throw new Exception("Basetype is affected outside his scope")

    if (conservative) {
      consumer.getInput(consumerInputId) match {
        case that: NoneNode =>
        case null =>
        case man: MultipleAssignmentNode => {
          consumer = man
          consumerInputId = man.getInputsCount;
          man.inputs += null
        }
        case that => {
          val man = new MultipleAssignmentNode
          initMan(man, that)
          man.inputs += null
          consumer.setInputWrap(consumerInputId) = man
          consumerInputId = 1
          consumer = man
        }
      }
    } else {
      val overrided = consumer.getInput(consumerInputId)
      if (overrided != null && !overrided.isInstanceOf[NoneNode] && !overrided.isInstanceOf[Reg])
        if (consumer.globalData.overridingAssignementWarnings) {
          val exept = new Throwable()
          val trace = ScalaLocated.short
          Component.current.prePopTasks += (() => {
            SpinalWarning(s"$baseType is overridden at ${trace}")
          })
        }
    }

    consumer match {
      case consumer : AssignementTreePart => consumer.setAssignementContext(consumerInputId,consumer.globalData.getThrowable())
      case _ =>
    }
    (consumer, consumerInputId)
  }
}


abstract class BaseType extends Node with Data with Nameable with AssignementTreePart{
  var input : Node = null
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}

  private[core] def canSymplifyIt = !dontSimplify && attributes.isEmpty

  private[core] var dontSimplify = false
  private[core] var dontCareAboutNameForSymplify = false

  override def dontSimplifyIt(): this.type = {
    dontSimplify = true;
    this
  }

  override def allowSimplifyIt(): this.type = {
    dontSimplify = false;
    this
  }

  private[core] def getLiteral[T <: Literal]: T = getInput(0) match {
    case lit: Literal => lit.asInstanceOf[T]
    case bt: BaseType => bt.getLiteral[T]
    case _ => null.asInstanceOf[T]
  }

  var defaultValue: BaseType = null

  override def getBitsWidth: Int = getWidth

  override def isReg = getInput(0).isInstanceOf[Reg]
  def getDrivingReg : this.type = getInput(0) match{
    case reg : Reg => this
    case bt : BaseType => bt.getDrivingReg.asInstanceOf[this.type]
    case _ => SpinalError("Driver is not a register")
  }

  def isDelay = getInput(0).isInstanceOf[SyncNode]

  override def asInput(): this.type = {
    component.ioSet += this
    super.asInput()
  }

  override def asOutput(): this.type = {
    component.ioSet += this
    super.asOutput()
  }


  override def asDirectionLess(): BaseType.this.type = {
    if(dir == null) return this
    component.ioSet -= this
    super.asDirectionLess()
  }

  private[core] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
    if (that.isInstanceOf[BaseType] || that.isInstanceOf[AssignementNode] || that.isInstanceOf[DontCareNode]) {
      BaseType.checkAssignability(this,that.asInstanceOf[Node])
      val (consumer, inputId) = BaseType.walkWhenNodes(this, this, 0, conservative)
      consumer.setInputWrap(inputId) = that.asInstanceOf[Node]
    } else {
      throw new Exception("Undefined assignment")
    }
  }

  // def castThatInSame(that: BaseType): this.type = throw new Exception("Not defined")

  override def assignDontCare(): this.type = {
    this.assignFrom(new DontCareNodeInfered(this), false)
    this
  }

  // = (this.flatten, that.flatten).zipped.map((a, b) => a.isNotEguals(b)).reduceLeft(_ || _)
  private[core] override def autoConnect(that: Data): Unit = autoConnectBaseImpl(that)

  override def flatten: Seq[BaseType] = Seq(this);

  override def flattenLocalName: Seq[String] = Seq("")

  override def addAttribute(attribute: Attribute): this.type = {
    attributes += attribute
    dontSimplifyIt()
    this
  }

  override private[core] def checkInferedWidth: String = {
    val input = this.getInput(0)
    if (input != null && input.component != null && this.getWidth != input.getWidth) {
      return s"Assignment bit count mismatch. ${this} := ${input}} at \n${ScalaLocated.long(getAssignementContext(0))}"
    }
    return null
  }

  override def clone: this.type = {
    val res = this.getClass.newInstance.asInstanceOf[this.type]
    //res.dir = this.dir
    res
  }

  private[core] def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer

  private[core] def newLogicalOperator(opName: String, right: Node, normalizeInputsImpl: (Node) => Unit, simplifyNodeImpl: (Node) => Unit): Bool = {
    val op = BinaryOperator(opName, this, right, WidthInfer.oneWidth, normalizeInputsImpl, simplifyNodeImpl)
    val typeNode = new Bool
    typeNode.setInputWrap(0) = op
    typeNode
  }

  private[core] def newBinaryOperator(opName: String, right: Node, getWidthImpl: (Node) => Int, normalizeInputsImpl: (Node) => Unit, simplifyNodeImpl: (Node) => Unit): this.type = {
    val op = BinaryOperator(opName, this, right, getWidthImpl, normalizeInputsImpl, simplifyNodeImpl)
    val typeNode = addTypeNodeFrom(op)
    typeNode
  }

  //def newUnaryOperator(opName: String, simplifyNodeImpl: (Node) => Unit = ZeroWidth.none): this.type = newUnaryOperator(opName, WidthInfer.inputMaxWidth, simplifyNodeImpl)

  private[core] def newUnaryOperator(opName: String, getWidthImpl: (Node) => Int = WidthInfer.inputMaxWidth, simplifyNodeImpl: (Node) => Unit): this.type = {
    val op = UnaryOperator(opName, this, getWidthImpl, InputNormalize.none, simplifyNodeImpl)
    val typeNode = addTypeNodeFrom(op)
    typeNode
  }


  private[core] def newFunction(opName: String, args: List[Node], getWidthImpl: (Node) => Int = WidthInfer.inputMaxWidth, simplifyNodeImpl: (Node) => Unit): this.type = {
    val op = Function(opName, args, getWidthImpl, simplifyNodeImpl)
    val typeNode = addTypeNodeFrom(op)
    typeNode
  }
  //Remove all of them


  private[core] def addTypeNodeFrom(node: Node): this.type = {
    val typeNode = weakClone
    typeNode.input = node
    typeNode
  }

  def newCast[T <: BaseType](result : T,node : Cast) : T = {
    node.input = this
    result.input = node
    result
  }

  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = (outHi, outLo)

  //Create a new instance of the same datatype without any configuration (width, direction)
  private[core] def weakClone: this.type = this.getClass.newInstance().asInstanceOf[this.type]

  override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getClassIdentifier}"



  var assignementThrowable : Throwable = null

  override def getAssignementContext(id: Int): Throwable = {
    assert(id == 0)
    assignementThrowable
  }
  override def setAssignementContext(id: Int,that : Throwable): Unit =  {
    assert(id == 0)
    assignementThrowable =that
  }
}