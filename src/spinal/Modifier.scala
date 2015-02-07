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

/**
 * Created by PIC18F on 21.08.2014.
 */

object EnumCast {
  def apply(enum: SpinalEnumCraft[_], opName: String, that: Node, widthImpl: (Node) => Int = WidthInfer.inputMaxWidthl): Modifier = {
    val op = new EnumCast(enum, opName, widthImpl)
    op.inputs += that
    op
  }


}


object Cast {
  def apply(opName: String, that: Node, widthImpl: (Node) => Int = WidthInfer.inputMaxWidthl): Modifier = {
    val op = new Cast(opName, widthImpl)
    op.inputs += that
    op
  }
}

object Resize {
  def apply(opName: String, args: List[Node], widthImpl: (Node) => Int = WidthInfer.inputMaxWidthl): Modifier = {
    val op = new Function(opName, widthImpl)
    op.inputs ++= args
    op.inferredWidth = widthImpl(op)
    op
  }

}

object Function {
  def apply(opName: String, args: List[Node], widthImpl: (Node) => Int = WidthInfer.inputMaxWidthl): Modifier = {
    val op = new Function(opName, widthImpl)
    op.inputs ++= args
    op
  }

}

object UnaryOperator {
  def apply(opName: String, right: Node, widthImpl: (Node) => Int, normalizeInputsImpl: (Node) => Unit): Modifier = {
    val op = new Operator(opName, widthImpl, normalizeInputsImpl)
    op.inputs += right
    op
  }
}

object BinaryOperator {
  def apply(opName: String, left: Node, right: Node, widthImpl: (Node) => Int, normalizeInputsImpl: (Node) => Unit): Modifier = {
    val op = new Operator(opName, widthImpl, normalizeInputsImpl)
    op.inputs += left
    op.inputs += right
    op
  }
}

class Operator(opName: String, widthImpl: (Node) => Int, val normalizeInputsImpl: (Node) => Unit) extends Modifier(opName, widthImpl) {
  override def normalizeInputs: Unit = {
    normalizeInputsImpl(this)
  }
}

class Modifier(val opName: String, widthImpl: (Node) => Int) extends Node {
  override def calcWidth(): Int = {
    widthImpl(this)
  }


  override def toString(): String = {
    s"($opName ${this.inputs.map(in => if(in == null) "null" else in.nonRecursiveToString()).reduceLeft(_ + " " + _)})"
  }
  override def nonRecursiveToString(): String = opName
}

class Function(opName: String, widthImpl: (Node) => Int) extends Modifier(opName, widthImpl) {

}

class Cast(opName: String, widthImpl: (Node) => Int = WidthInfer.inputMaxWidthl) extends Modifier(opName, widthImpl) {

}

class EnumCast(val enum: SpinalEnumCraft[_], opName: String, widthImpl: (Node) => Int = WidthInfer.inputMaxWidthl) extends Modifier(opName, widthImpl) {
  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, 0, this.getWidth)
  }
}

object ExtractBool {
  def apply(bitVector: BitVector, bitId: Int): ExtractBool = {
    val op = new ExtractBool
    op.inputs += bitVector
    op.inputs += new IntLiteral(bitId)
    op
  }

  def apply(bitVector: BitVector, bitId: UInt): ExtractBool = {
    val op = new ExtractBool
    op.inputs += bitVector
    op.inputs += bitId
    op
  }
}

class ExtractBool extends Node {

  def calcWidth: Int = {
    1
  }

  def bitVector = inputs(0)

  def bitId = inputs(1)
}

object ExtractBitsVector {
  def apply(bitVector: BitVector, bitIdHi: Int, bitIdLow: Int): ExtractBitsVector = {
    if (bitIdHi - bitIdLow < -1) SpinalError(s"Static bits extraction with a negative size ($bitIdHi downto $bitIdLow)")
    val op = new ExtractBitsVector
    op.inputs += bitVector.toBits
    op.inputs += new IntLiteral(bitIdHi)
    op.inputs += new IntLiteral(bitIdLow)
    op
  }


  def apply(bitVector: BitVector, bitIdHi: UInt, bitIdLow: UInt): Bits = {
    val sr = bitVector.toBits >> bitIdLow
    val mask = (UInt(1) << bitIdLow) - UInt(1)
    val ret = sr & mask.toBits;
    ret
  }
}

class ExtractBitsVector extends Node {
  def calcWidth: Int = (bitIdHi, bitIdLo) match {
    case range: (IntLiteral, IntLiteral) => {
      (range._1.value + 1 - range._2.value).toInt
    }

  }

  def bitVector = inputs(0)
  def bitIdHi = inputs(1).asInstanceOf[IntLiteral]
  def bitIdLo = inputs(2).asInstanceOf[IntLiteral]
}

class Multiplexer(opName: String) extends Modifier(opName, WidthInfer.inputMaxWidthl) {
  var whenMux: Boolean = false;

  def cond = inputs(0)
  def whenTrue = inputs(1)
  def whenFalse = inputs(2)

  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, 1, this.getWidth)
    Misc.normalizeResize(this, 2, this.getWidth)
  }
}

object Mux {
  /*
    def apply(sel: Bool, whenTrue: Bool, whenFalse: Bool): Bool = {
      whenTrue.clone.addTypeNodeFrom(Multiplex.baseType(sel,whenTrue,whenFalse))
    }*/

  def apply[T <: Data](sel: Bool, whenTrue: T, whenFalse: T): T = {
    Multiplex.complexData(sel, whenTrue, whenFalse)
    /* Tuple2(whenTrue,whenFalse) match {
       case data : Tuple2[BaseType,BaseType] => data._1.clone().addTypeNodeFrom(Multiplex.baseType(sel,data._1,data._2))
       case data : Tuple2[Bundle,Bundle] => Multiplex.complexData(sel,data._1,data._2)
     }*/
  }
}

/*
object Mux {
  def apply(sel: Bool, one: UInt, zero: UInt): UInt = {
    val op = new Multiplexer(false)

    op.inputs += sel
    op.inputs += one
    op.inputs += zero

    val typeNode = one.addTypeNodeFrom(op)
    typeNode

  }


  def widthImpl(node: Node): Int = {
    val oneWidth = if (node.inputs(1) != null) node.inputs(1).getWidth else -1
    val zeroWidth = if (node.inputs(2) != null) node.inputs(2).getWidth else -1
    Math.max(oneWidth, zeroWidth)
  }


}
*/

private[spinal] object Multiplex {
  def apply(opName: String, sel: Bool, one: Node, zero: Node): Multiplexer = {

    val op = new Multiplexer(opName)
    op.inputs += sel
    op.inputs += one
    op.inputs += zero
    op
  }

  def baseType[T <: BaseType](sel: Bool, whenTrue: T, whenFalse: T): Multiplexer = {
    whenTrue.newMultiplexor(sel, whenTrue, whenFalse)
  }


  def complexData[T <: Data](sel: Bool, whenTrue: T, whenFalse: T): T = {
    val outType = if (whenTrue.getClass.isAssignableFrom(whenFalse.getClass)) whenTrue
    else if (whenFalse.getClass.isAssignableFrom(whenTrue.getClass)) whenFalse
    else throw new Exception("can't mux that")

    val muxOut = outType.clone()
    val muxInTrue = outType.clone()
    val muxInFalse = outType.clone()

    //TODO need strong asignement (bundle case) ? what happend if bundle are not same type
    muxInTrue := whenTrue
    muxInFalse := whenFalse

    for (((x, out), (y, t), (z, f)) <- (muxOut.flatten, muxInTrue.flatten, muxInFalse.flatten).zipped) {
      out.setInput(Multiplex.baseType(sel, t, f))
    }

    muxOut
  }

}

