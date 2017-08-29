package spinal.core

/**
 * Created by PIC32F_USER on 12/02/2016.
 */



//class VecBaseType[T <: BaseType](val baseType : T,val dims : IndexedSeq[Int]) extends BaseType{
//
//  //override private[core] def calcWidth: Int = WidthInfer.inputMaxWidth(this)*getInputsCount
//  override private[core] def calcWidth: Int = elementWidth*dims.reduce(_*_)
//  def elementWidth = baseType.calcWidth
//
//
//  override private[core] def newMultiplexer(sel: Bool, whenTrue: Node, whenFalse: Node): Multiplexer = ???
//
//  override def assignFromBits(bits: Bits): Unit = ???
//
//  override private[core] def isEguals(that: Any): Bool = ???
//
//  override def assignFromBits(bits: Bits, hi: Int, low: Int): Unit = ???
//
//  override def asBits: Bits = ???
//
//  override def getZero: VecBaseType.this.type = ???
//
//  // = (this.flatten, that.flatten).zipped.map((a, b) => a.isEguals(b)).reduceLeft(_ && _)
//  override private[core] def isNotEguals(that: Any): Bool = ???
//
//  override private[core] def normalizeInputs: Unit = {
//
//  }
//
//  def apply(sels : Seq[Int]) : T = {
//    val ret = this.read(sels)
//    ret.compositeAssign = new Assignable {
//      override private[core] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = write(that.asInstanceOf[T],sels)
//    }
//    ret
//  }
//
//  def write(data : T,sels : Seq[Int]) : Unit = {
//    val assign = new VecBaseTypeAssignFixed(this,sels)
//    assign.setIn3putWrap(0) = data
//    this.assignFrom(assign)
//  }
//
//  def read(sels : Seq[Int]) : T = {
//    val ret = baseType.clone
//    val extract = new VecBaseTypeExtractFixed(sels)
//    extract.setInp3utWrap(0) = this
//    ret.setInpu3tWrap(0) = extract
//    ret
//  }
//
//
//
//  def widthAfter(dim : Int) = dims.drop(dim).foldLeft(elementWidth)(_*_)
//
//}


//TOD outToIn for all
//class VecBaseTypeExtractFixed(val sels : Seq[Int]) extends NodeWithVariableInputsCount{
//  inputs += null
//  def getVec = getInput(0).asInstanceOf[VecBaseType[_]]
//
//
//  override private[core] def calcWidth: Int = getVec.widthAfter(sels.length)
//
//}


//class VecBaseTypeAssignFixed(val vec : VecBaseType[_],val sels : Seq[Int]) extends AssignementNode{
//  inputs += null
//  def getData = getInput(0)
//
//
//  //override private[core] def calcWidth: Int = vec.getWidth
//
//  override def getAssignedBits: AssignedRange = {//TOD
//    AssignedRange(vec.getWidth-1,0)
//  }
//
//  override def getOutBaseType: BaseType = vec
//
//  override def getScopeBits: AssignedRange = getAssignedBits //TOD
//  override def clone(out: Node): VecBaseTypeAssignFixed.this.type = ???
//
//  override private[core] def calcWidth: Int = ???
//}