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

trait TypeFactory{
  def postTypeFactory[T <: Data] (that : T) = that
}

trait BaseTypeFactory extends BoolFactory with BitsFactory with UIntFactory with SIntFactory with VecFactory with SFixFactory with UFixFactory

trait BaseTypeCast extends UIntCast with SIntCast with BitsCast with SFixCast with UFixCast

object BaseType {
  def checkAssignability(dst : BaseType,src : Node) : Unit = {
    val globalData = dst.globalData
    dst.dir match{
      case null => if(globalData.componentStack.head() != dst.component) {
        val trace = ScalaLocated.long
        globalData.pendingErrors += (() => (s"Signal $dst can't be assigned by $src\n$trace"))
      }
      case `in` => if(!(src.component == dst.component.parent || (dst.component.parent == src.component.parent && src.isInstanceOf[BaseType] && src.asInstanceOf[BaseType].isOutput))){
        val trace = ScalaLocated.long
        globalData.pendingErrors += (() => (s"Input signal $dst can't be assigned by $src\n$trace"))
      }
      case `out` => if(globalData.componentStack.head() != dst.component){
        val trace = ScalaLocated.long
        globalData.pendingErrors += (() => (s"Output signal $dst can't be assigned by $src\n$trace"))
      }
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
        man.inputs += that.asInstanceOf[man.T]
      } else {
        val bt = baseType.weakClone
        bt.input = that
        man.inputs += bt.asInstanceOf[man.T]
      }
    }




    for (conditionalAssign <- globalData.conditionalAssignStack.stack.reverseIterator) {
      if (!initialConditionalAssignHit) {
        if (conditionalAssign == baseType.conditionalAssignScope) initialConditionalAssignHit = true
      } else {
        conditionalAssign match {
          case when: WhenContext => {
            consumer.getInput(consumerInputId) match {
              case null => {
                val whenNode = WhenNode(baseType,when)
                if(consumer.isInstanceOf[AssignementTreePart]){
                  consumer.asInstanceOf[AssignementTreePart].setAssignementContext(consumerInputId,globalData.getThrowable())
                }
                consumer.setInput(consumerInputId,whenNode)
                consumer = whenNode
              }
              case man: MultipleAssignmentNode => {
                man.inputs.last match {
                  case whenNode: WhenNode if whenNode.w == when => consumer = whenNode
                  case _ => {
                    val whenNode = WhenNode(baseType,when)
                    man.inputs += whenNode.asInstanceOf[man.T]
                    consumer = whenNode
                  }
                }
              }
              case whenNode: WhenNode if whenNode.w == when => consumer = whenNode
              case that => {
                val man = MultipleAssignmentNode.newFor(baseType)
                val whenNode = WhenNode(baseType,when)
                initMan(man, that)
                man.inputs += whenNode.asInstanceOf[man.T]
                consumer.setInput(consumerInputId, man)
                consumer = whenNode
              }
            }

            consumerInputId = if (when.isTrue) 1 else 2
          }
        }
      }
    }
    if (!initialConditionalAssignHit)
      throw new Exception("Basetype is affected outside his scope")

    if (conservative) {
      consumer.getInput(consumerInputId) match {
        case null =>
        case man: MultipleAssignmentNode => {
          consumer = man
          consumerInputId = man.inputs.length;
          man.inputs += null.asInstanceOf[man.T]
        }
        case that => {
          val man = MultipleAssignmentNode.newFor(baseType)
          initMan(man, that)
          man.inputs += null.asInstanceOf[man.T]
          consumer.setInput(consumerInputId,man)
          consumerInputId = 1
          consumer = man
        }
      }
    } else {
      val overrided = consumer.getInput(consumerInputId)
      if (overrided != null && !overrided.isInstanceOf[Reg])
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

  private[core] def canSymplifyIt = !dontSimplify && attributes.isEmpty && isEmptyOfTag

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

  private[core] def getLiteral[T <: Literal]: T = input match {
    case lit: Literal => lit.asInstanceOf[T]
    case bt: BaseType => bt.getLiteral[T]
    case _ => null.asInstanceOf[T]
  }

  var defaultValue: BaseType = null

  override def isReg = input.isInstanceOf[Reg]
  def getDrivingReg : this.type = input match{
    case reg : Reg => this
    case bt : BaseType => bt.getDrivingReg.asInstanceOf[this.type]
    case _ => SpinalError("Driver is not a register")
  }

  def isDelay = input.isInstanceOf[SyncNode]

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
      consumer.setInput(inputId,that.asInstanceOf[Node])
    } else {
      throw new Exception("Undefined assignment")
    }
  }

  // def castThatInSame(that: BaseType): this.type = throw new Exception("Not defined")

  override def assignDontCare(): this.type = {
    this.assignFrom(new DontCareNode(this), false)
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



  override def clone: this.type = {
    val res = this.getClass.newInstance.asInstanceOf[this.type]
    //res.dir = this.dir
    res
  }

  private[core] def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer
  private[core] def newMultiplexer(cond : Node,whenTrue : Node,whenFalse : Node,mux : Multiplexer): Multiplexer = {
    mux.cond      = cond
    mux.whenTrue  = whenTrue.asInstanceOf[mux.T]
    mux.whenFalse = whenFalse.asInstanceOf[mux.T]
    mux
  }


  private[core] def wrapWithWeakClone(node: Node): this.type = {
    val typeNode = weakClone
    typeNode.input = node
    typeNode
  }

  def wrapCast[T <: BaseType](result : T,node : Cast) : T = {
    node.input = this.asInstanceOf[node.T]
    result.input = node
    result
  }

  private[core] def wrapConstantOperator(op : ConstantOperator): this.type = {
    op.input = this.asInstanceOf[op.T]
    wrapWithWeakClone(op)
  }
  private[core] def wrapUnaryOperator(op : UnaryOperator): this.type = {
    op.input = this.asInstanceOf[op.T]
    wrapWithWeakClone(op)
  }
  private[core] def wrapBinaryOperator(right : Node,op : BinaryOperator): this.type = {
    op.left = this.asInstanceOf[op.T]
    op.right = right.asInstanceOf[op.T]
    wrapWithWeakClone(op)
  }
  private[core] def wrapLogicalOperator(right : Node,op : BinaryOperator):  Bool = {
    op.left = this.asInstanceOf[op.T]
    op.right = right.asInstanceOf[op.T]
    val ret = new Bool
    ret.input = op
    ret
  }
  private[core] def wrapMultiplexer(cond : Node,whenTrue : Node,whenFalse : Node,mux : Multiplexer): this.type = {
    mux.cond      = cond
    mux.whenTrue  = whenTrue.asInstanceOf[mux.T]
    mux.whenFalse = whenFalse.asInstanceOf[mux.T]
    wrapWithWeakClone(mux)
  }


  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = (outHi, outLo)

  //Create a new instance of the same datatype without any configuration (width, direction)
  private[core] def weakClone: this.type

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