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



import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC18F on 21.08.2014.
 */

trait BaseTypeFactory extends BoolFactory with BitsFactory with UIntFactory with SIntFactory with VecFactory

trait BaseTypeCast extends BoolCast with UIntCast with SIntCast with BitsCast with SFixCast with UFixCast

object BaseType {
  def walkWhenNodes(baseType: BaseType, initialConsumer: Node, initialConsumerInputId: Int, conservative: Boolean = false) = {
    def initMan(man: MultipleAssignmentNode, that: Node): Unit = {
      //To be sure that there is basetype to bufferise (for future resize)
      if (that.isInstanceOf[WhenNode] || that.isInstanceOf[BaseType] || that.isInstanceOf[AssignementNode] || that.isInstanceOf[MultipleAssignmentNode] || that.isInstanceOf[Reg]) {
        man.inputs += that
      } else {
        val bt = baseType.weakClone
        bt.inputs(0) = that
        man.inputs += bt
      }
    }

    val globalData = baseType.globalData
    var consumer = initialConsumer
    var consumerInputId: Int = initialConsumerInputId
    var initialConditionalAssignHit = baseType.conditionalAssignScope == null

    for (conditionalAssign <- globalData.conditionalAssignStack.stack.reverseIterator) {
      if (!initialConditionalAssignHit) {
        if (conditionalAssign == baseType.conditionalAssignScope) initialConditionalAssignHit = true
      } else {
        conditionalAssign match {
          case when: WhenContext => {
            consumer.inputs(consumerInputId) match {
              case nothing @ (null | _:NoneNode) => {
                val whenNode = WhenNode(when)
                consumer.inputs(consumerInputId) = whenNode
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
                consumer.inputs(consumerInputId) = man
                consumer = whenNode
              }
            }

            consumerInputId = if (when.isTrue) 1 else 2
          }
          //TODO switch
          case context: SwitchContext => {
            consumer.inputs(consumerInputId) match {
              case nothing @ (null | _:NoneNode) => {
                val switchNode = new SwitchNode(context)
                consumer.inputs(consumerInputId) = switchNode
                consumer = switchNode
              }
              case man: MultipleAssignmentNode => {
                man.inputs.last match {
                  case currentContext: SwitchNode if currentContext.context == context => consumer = currentContext
                  case _ => {
                    val switchNode = new SwitchNode(context)
                    man.inputs += switchNode
                    consumer = switchNode
                  }
                }
              }
              case currentContext: SwitchNode if currentContext.context == context => consumer = currentContext
              case that => {
                val man = new MultipleAssignmentNode
                val switchNode = new SwitchNode(context)
                initMan(man, that)
                man.inputs += switchNode
                consumer.inputs(consumerInputId) = man
                consumer = switchNode
              }
            }
          }

          case context: CaseContext => {
            if(consumer.inputs.isEmpty){
              val caseNode = new CaseNode(context)
              consumer.inputs += caseNode
              consumer = caseNode
            }else{
              val last = consumer.inputs.last.asInstanceOf[CaseNode]
              if(last.context != context){
                val caseNode = new CaseNode(context)
                consumer.inputs += caseNode
                consumer = caseNode
              }else{
                consumer = last
              }
            }
            consumerInputId = 1
          }
        }
      }
    }
    if (!initialConditionalAssignHit)
      throw new Exception("Basetype is affected outside his scope")

    if (conservative) {
      consumer.inputs(consumerInputId) match {
        case that: NoneNode =>
        case null =>
        case man: MultipleAssignmentNode => {
          consumer = consumer.inputs(consumerInputId)
          consumerInputId = consumer.inputs.size;
          consumer.inputs += null
        }
        case that => {
          val man = new MultipleAssignmentNode
          initMan(man, that)
          man.inputs += null
          consumer.inputs(consumerInputId) = man
          consumerInputId = 1
          consumer = man
        }
      }
    } else {
      val overrided = consumer.inputs(consumerInputId)
      if (overrided != null && !overrided.isInstanceOf[NoneNode] && !overrided.isInstanceOf[Reg])
        if(consumer.globalData.overridingAssignementWarnings)
        SpinalWarning(s"Value of $baseType is overridden at ${ScalaLocated.getScalaTraceSmart}")
    }
    (consumer, consumerInputId)
  }
}


abstract class BaseType extends Node with Data with Nameable {
  inputs += null

  private[core] def canSymplifyIt = !dontSimplify && attributes.isEmpty

  private[core] var dontSimplify = false

  override def dontSimplifyIt(): this.type = {
    dontSimplify = true;
    this
  }

  override def allowSimplifyIt(): this.type = {
    dontSimplify = false;
    this
  }


  private[core] def getLiteral[T <: Literal]: T = inputs(0) match {
    case lit: Literal => lit.asInstanceOf[T]
    case bt: BaseType => bt.getLiteral[T]
    case _ => null.asInstanceOf[T]
  }

  var defaultValue: BaseType = null

  override def getBitsWidth: Int = getWidth

  override def isReg = inputs(0).isInstanceOf[Reg]

  def isDelay = inputs(0).isInstanceOf[SyncNode]

  override def asInput(): this.type = {
    component.ioSet += this
    super.asInput()
  }

  override def asOutput(): this.type = {
    component.ioSet += this
    super.asOutput()
  }

  private[core] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
    if (that.isInstanceOf[BaseType] || that.isInstanceOf[AssignementNode] || that.isInstanceOf[DontCareNode]) {
      val (consumer, inputId) = BaseType.walkWhenNodes(this, this, 0, conservative)
      consumer.inputs(inputId) = that.asInstanceOf[Node]
    } else {
      throw new Exception("Undefined assignement")
    }
  }


  // def castThatInSame(that: BaseType): this.type = throw new Exception("Not defined")


  def assignDontCare(): Unit = this.assignFrom(new DontCareNodeInfered(this), false)

  // = (this.flatten, that.flatten).zipped.map((a, b) => a.isNotEguals(b)).reduceLeft(_ || _)
  private[core] override def autoConnect(that: Data): Unit = autoConnectBaseImpl(that)

  override def flatten: Seq[BaseType] = Seq(this);

  override def flattenLocalName: Seq[String] = Seq("")

  override def add(attribute: Attribute): Unit = {
    attributes += attribute
    dontSimplifyIt()
  }

  override def clone: this.type = {
    val res = this.getClass.newInstance.asInstanceOf[this.type];
    //res.dir = this.dir
    res
  }


  private[core] def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer


  private[core] def newLogicalOperator(opName: String, right: Node, normalizeInputsImpl: (Node) => Unit, simplifyNodeImpl: (Node) => Unit): Bool = {
    val op = BinaryOperator(opName, this, right, WidthInfer.oneWidth, normalizeInputsImpl, simplifyNodeImpl)
    val typeNode = new Bool
    typeNode.inputs(0) = op
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

  private[core] def castFrom(opName: String, that: Node, getWidthImpl: (Node) => Int = WidthInfer.inputMaxWidth): this.type = {
    val op = Cast(opName, that, getWidthImpl)
    this.setInput(op)
    this
  }

  private[core] def enumCastFrom(opName: String, that: Node, getWidthImpl: (Node) => Int = WidthInfer.inputMaxWidth): this.type = {
    val op = EnumCast(this.asInstanceOf[SpinalEnumCraft[_]], opName, that, getWidthImpl)
    this.setInput(op)
    this
  }

  private[core] def newFunction(opName: String, args: List[Node], getWidthImpl: (Node) => Int = WidthInfer.inputMaxWidth, simplifyNodeImpl: (Node) => Unit): this.type = {
    val op = Function(opName, args, getWidthImpl, simplifyNodeImpl)
    val typeNode = addTypeNodeFrom(op)
    typeNode
  }

  private[core] def newResize(opName: String, args: List[Node], getWidthImpl: (Node) => Int = WidthInfer.inputMaxWidth, simplifyNodeImpl: (Node) => Unit): this.type = {
    val op = Resize(opName, args, getWidthImpl, simplifyNodeImpl)
    val typeNode = addTypeNodeFrom(op)
    typeNode
  }


  private[core] def addTypeNodeFrom(node: Node): this.type = {
    val typeNode = weakClone
    typeNode.setInput(node)
    typeNode
  }

  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = (outHi,outLo)

  //Create a new instance of the same datatype without any configuration (width, direction)
  private[core] def weakClone: this.type = this.getClass.newInstance().asInstanceOf[this.type]


  override def toString(): String = s"${getClassIdentifier}(named ${"\"" + getName() + "\""},into ${if (component == null) "null" else component.getClass.getSimpleName})"
}

