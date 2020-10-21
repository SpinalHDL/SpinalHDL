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

package spinal.lib

import spinal.core.internals._
import java.io.UTFDataFormatException
import java.nio.charset.Charset

import spinal.core._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer



object UIntToOh {
  def apply(value: UInt, width : Int): Bits = {
    val ret = Bits(width bits)
    for(i <- 0 until width){
      ret(i) := value === i
    }
    ret
  }

  def apply(value: UInt, mapping : Seq[Int]): Bits = {
    val ret = Bits(mapping.size bits)
    for((m, i) <- mapping.zipWithIndex){
      ret(i) := value === m
    }
    ret
  }
}


object OHToUInt {
  def apply(bitVector: BitVector): UInt = apply(bitVector.asBools)
  def apply(bools: Seq[Bool]): UInt = signalCache(bools, "OHToUInt") {
    val boolsSize = bools.size
    if (boolsSize < 2) return U(0,0 bits)

    val retBitCount = log2Up(bools.size)
    val ret = Vec(Bool,retBitCount)

    for (retBitId <- 0 until retBitCount) {
      var bit: Bool = null
      for (boolsBitId <- 0 until boolsSize if ((boolsBitId >> retBitId) & 1) != 0) {
        if (bit != null)
          bit = bit | bools(boolsBitId)
        else
          bit = bools(boolsBitId)
      }
      ret(retBitId) := bit.dontSimplifyIt()
    }

    ret.asBits.asUInt
  }

  def apply(bitVector: BitVector, mapping : Seq[Int]): UInt = apply(bitVector.asBools, mapping)
  def apply(oh: Seq[Bool], mapping : Seq[Int]): UInt = {
    assert(oh.size == mapping.size)
    val ret = UInt(log2Up(mapping.max + 1) bits)

    if (mapping.size == 1) {
      ret := mapping.head
    } else {
      for (bitId <- ret.range) {
        val triggersId = mapping.zipWithIndex.filter(e => ((e._1 >> bitId) & 1) != 0).map(_._2)
        val triggers = triggersId.map(oh(_))
        ret(bitId) := triggers.orR
      }
    }

    ret
  }
}

//Will be target dependent
object MuxOH {
  def apply[T <: Data](oneHot : BitVector,inputs : Seq[T]): T = apply(oneHot.asBools,inputs)
  def apply[T <: Data](oneHot : collection.IndexedSeq[Bool],inputs : Iterable[T]): T =  apply(oneHot,Vec(inputs))

  def apply[T <: Data](oneHot : BitVector,inputs : Vec[T]): T = apply(oneHot.asBools,inputs)
  def apply[T <: Data](oneHot : collection.IndexedSeq[Bool],inputs : Vec[T]): T = {
    oneHot.size match {
      case 2 => oneHot(0) ? inputs(0) | inputs(1)
      case _ => inputs(OHToUInt(oneHot))
    }
  }
}

object Min {
    def apply[T <: Data with Num[T]](nums: T*) = list(nums)
    def list[T <: Data with Num[T]](nums: Seq[T]) = {
        nums.reduceBalancedTree(_ min _)
    }
}

object Max {
    def apply[T <: Data with Num[T]](nums: T*) = list(nums)
    def list[T <: Data with Num[T]](nums: Seq[T]) = {
        nums.reduceBalancedTree(_ max _)
    }
}

object OHMasking{

  /** returns an one hot encoded vector with only LSB of the word present */
  def first[T <: Data](that : T) : T = {
      val input = that.asBits.asUInt
      val masked = input & ~(input - 1)
      val ret = cloneOf(that)
      ret.assignFromBits(masked.asBits)
      ret
  }

  //Avoid combinatorial loop on the first
  def first(that : Vec[Bool]) : Vec[Bool] = {
    val bitsFirst = first(that.asBits)
    Vec(that.head +: bitsFirst.asBools.tail)
  }

  //Avoid combinatorial loop on the first
  def first(that : Seq[Bool]) : Vec[Bool] = first(Vec(that))

  /** returns an one hot encoded vector with only MSB of the word present */
  def last[T <: Data](that : T) : T = {
    val input = Reverse(that.asBits.asUInt)
    val masked = input & ~(input - 1)
    val ret = cloneOf(that)
    ret.assignFromBits(Reverse(masked.asBits))
    ret
  }


  def roundRobin[T <: Data](requests : T,ohPriority : T) : T = {
    val width = requests.getBitsWidth
    val uRequests = requests.asBits.asUInt
    val uGranted = ohPriority.asBits.asUInt

    val doubleRequests = uRequests @@ uRequests
    val doubleGrant = doubleRequests & ~(doubleRequests-uGranted)
    val masked = doubleGrant(width,width bits) | doubleGrant(0,width bits)

    val ret = cloneOf(requests)
    ret.assignFromBits(masked.asBits)
    ret
  }
}

object CountOne{
  def args(thats : Bool*) : UInt = apply(thats)
  def apply(thats : BitVector) : UInt = apply(thats.asBools)
  def apply(thats : Seq[Bool]) : UInt = {
    if(thats.isEmpty) return U(0, 0 bits)
    val lut = Vec((0 until 1 << Math.min(thats.size, 3)).map(v => U(BigInt(v).bitCount, log2Up(thats.size + 1) bits)))
    val groups = thats.grouped(3)
    val seeds = groups.map(l => lut.read(U(l.asBits()).resized)).toSeq
    seeds.reduceBalancedTree(_+_)
  }
}

object CountOneOnEach{
  def args(thats : Bool*) : Seq[UInt] = apply(thats)
  def apply(thats : BitVector) : Seq[UInt] = apply(thats.asBools)
  def apply(thats : Seq[Bool]) : Seq[UInt] = {
    for(bitCount <- 1 to thats.size) yield CountOne(thats.take(bitCount))

    //TODO
    /*val lut = Vec((0 until 1 << Math.min(thats.size, 3)).map(v => U(BigInt(v).bitCount, log2Up(thats.size + 1) bits)))
    val groups = thats.grouped(3)
    val seeds = groups.map(l => lut.read(U(l.asBits()))).toSeq
    var offset = U(0)
    for(bitId <- 0 until thats.size) yield {
      val ret = offset + U(resize(log2Up(bitId + 1) bits)
      if(bitId % 3 == 2) offset = offset + seeds(bitId/3)
      ret
    }*/
  }
}

object LeastSignificantBitSet{
  def apply(thats : Bool*) : UInt = list(thats)
  def apply(thats : Bits) : UInt = list(thats.asBools)
  def list(thats : Seq[Bool]) : UInt = {
    var ret = UInt(log2Up(thats.length+1) bit)
    ret.assignDontCare()
    for((e,id) <- thats.zipWithIndex.reverse){
      when(e){
        ret := id
      }
    }
    ret
  }
}


object toGray {
  def apply(uint: UInt): Bits = {
    B((uint >> U(1)) ^ uint)
  }
}

object fromGray {
  def apply(gray: Bits): UInt = {
    val ret = List.fill(widthOf(gray)) (Bool)
    for (i <- 0 until widthOf(gray) - 1) {
      ret(i) := gray(i) ^ ret(i + 1)
    }
    ret.last := gray.msb
    ret.asBits().asUInt
  }
}

object GrayCounter {
  def apply(width: Int, enable: Bool): UInt = {
    val gray = RegInit(U(0, width bit))
    val even = RegInit(True)
    val word = Cat(True, gray(width - 3 downto  0), even)
    when(enable) {
      var found = False
      for (i <- 0 until width) {
        when(word(i) && !found) {
          gray(i) := !gray(i)
          found \= True
        }
      }
      even := !even
    }
    return gray
  }
}


/******************************************************************************
  * Big-Endian <-> Little-Endian
  */
object EndiannessSwap{
  def apply[T <: BitVector](that : T, base:BitCount = 8 bits) : T = {

    val nbrBase = that.getWidth / base.value
    val ret = cloneOf(that)

    assert(nbrBase * base.value == that.getWidth, "Endianness Error : Width's input is not a multiple of " + base.value)

    for(i <- (0 until nbrBase)){
      val rangeIn  = (i * base.value + base.value - 1)    downto (i * base.value)
      val rangeOut = (that.getWidth - 1 - i * base.value) downto (that.getWidth - i * base.value - base.value)

      ret(rangeOut) := that(rangeIn)
    }
    ret
  }
}


object Reverse{
  def apply[T <: BitVector](that : T) : T = {
    val ret = cloneOf(that)
    for(i <- that.range){
      ret(i) := that(that.getWidth-1-i)
    }
    ret
  }
}
object AddWithCarry {
  def apply(left: UInt, right: UInt): (UInt, Bool) = {
    val temp = left.resize(left.getWidth + 1) + right.resize(right.getWidth + 1)
    return (temp.resize(temp.getWidth - 1), temp.msb)
  }
}

//This is a pure software, It can be used by a software driver to pack data
class BitAggregator {
  val elements = ArrayBuffer[(BigInt, Int)]()
  def clear = elements.clear()
  def add(valueParam: BigInt, bitCount: Int): Unit = elements += (valueParam -> bitCount)
  def add(valueParam: Boolean): Unit = if (valueParam) add(1, 1) else add(0, 1)

  def getWidth = elements.foldLeft(0)(_ + _._2)

  def toBytes: Seq[Byte] = {
    val elementsWidth = getWidth
    val bytes = new Array[Byte]((elementsWidth + 7) / 8)
    var byteId = 0
    var byteBitId = 0
    for (element <- elements) {
      var bitCount = element._2
      var value = (element._1 & (BigInt(1) << element._2) - 1) << byteBitId;
      while (bitCount != 0) {
        val bitToInsert = Math.min(bitCount, 8 - byteBitId);

        bytes(byteId) = (bytes(byteId) | value.toByte).toByte

        byteBitId += bitToInsert;
        if (byteBitId == 8) {
          byteBitId = 0;
          byteId += 1
        }
        value >>= 8;
        bitCount -= bitToInsert;
      }
    }

    bytes
  }

  override def toString: String = toBytes.map("%02X" format _).mkString(" ")
}

//object Flag{
//  def apply() = new Flag
//
//  implicit def implicitValue(f: Flag) = f.value
//}
//class Flag extends Area{
//  val value = False
//  def set = value := True
//}

object CounterFreeRun {
  def apply(stateCount: BigInt): Counter = {
    val c = Counter(stateCount)
    c.willIncrement.removeAssignments()
    c.increment()
    c
  }
}

object Counter {
  def apply(start: BigInt,end: BigInt) : Counter  = new Counter(start = start, end = end)
  def apply(range : Range) : Counter = {
    require(range.step == 1)
    Counter(start = range.low, end = range.high)
  }
  def apply(stateCount: BigInt): Counter = new Counter(start = 0, end = stateCount-1)
  def apply(bitCount: BitCount): Counter = new Counter(start = 0, end = (BigInt(1)<<bitCount.value)-1)

  def apply(start: BigInt,end: BigInt, inc: Bool) : Counter  = {
    val counter = Counter(start,end)
    when(inc) {
      counter.increment()
    }
    counter
  }
  def apply(range : Range, inc: Bool) : Counter  = {
    require(range.step == 1)
    Counter(start = range.low, end = range.high,inc = inc)
  }
  def apply(stateCount: BigInt, inc: Bool): Counter = Counter(start = 0, end = stateCount-1,inc = inc)
  def apply(bitCount: BitCount, inc: Bool): Counter = Counter(start = 0, end = (BigInt(1)<<bitCount.value)-1,inc = inc)
}

// start and end inclusive, up counter
class Counter(val start: BigInt,val end: BigInt) extends ImplicitArea[UInt] {
  require(start <= end)
  val willIncrement = False.allowOverride
  val willClear = False.allowOverride

  def clear(): Unit = willClear := True
  def increment(): Unit = willIncrement := True

  val valueNext = UInt(log2Up(end + 1) bit)
  val value = RegNext(valueNext) init(start)
  val willOverflowIfInc = value === end
  val willOverflow = willOverflowIfInc && willIncrement

  if (isPow2(end + 1) && start == 0) {   //Check if using overflow follow the spec
    valueNext := (value + U(willIncrement)).resized
  }
  else {
    when(willOverflow){
      valueNext := U(start)
    } otherwise {
      valueNext := (value + U(willIncrement)).resized
    }
  }
  when(willClear) {
    valueNext := start
  }

  willOverflowIfInc.allowPruning
  willOverflow.allowPruning

  override def implicitValue: UInt = this.value

  /**
   * Convert this stream to a flow. It will send each value only once. It is "start inclusive, end exclusive". 
   * This means that the current value will only be sent if the counter increments.
   */
  def toFlow(): Flow[UInt] = {
    val flow = Flow(value)
    flow.payload := value
    flow.valid := willIncrement
    flow
  }
}

object Timeout {
  def apply(cycles: BigInt) : Timeout = new Timeout(cycles)
  def apply(time: TimeNumber) : Timeout = new Timeout((time*ClockDomain.current.frequency.getValue).toBigInt)
  def apply(frequency: HertzNumber) : Timeout = Timeout(frequency.toTime)
}

class Timeout(val limit: BigInt) extends ImplicitArea[Bool] {
  assert(limit > 1)

  val state = RegInit(False)
  val stateRise = False

  val counter = CounterFreeRun(limit)
  when(counter.willOverflow) {
    state := True
    stateRise := !state
  }

  def clear() : Unit = {
    counter.clear()
    state := False
    stateRise := False
  }

  override def implicitValue: Bool = state
}

object MajorityVote {
  def apply(that: BitVector): Bool = apply(that.asBools)
  def apply(that: collection.IndexedSeq[Bool]): Bool = {
    val size = that.size
    val trigger = that.size / 2 + 1
    var globalOr = False
    for (i <- BigInt(0) until (BigInt(1) << size)) {
      if (i.bitCount == trigger) {
        var localAnd = True
        for (bitId <- 0 until i.bitLength) {
          if (i.testBit(bitId)) localAnd &= that(bitId)
        }
        globalOr = globalOr | localAnd
      }
    }
    globalOr
  }
}


object CounterUpDown {
  def apply(stateCount: BigInt): CounterUpDown = new CounterUpDown(stateCount)
  def apply(stateCount: BigInt, incWhen: Bool,decWhen : Bool): CounterUpDown = {
    val counter = CounterUpDown(stateCount)
    when(incWhen) {
      counter.increment()
    }
    when(decWhen) {
      counter.decrement()
    }
    counter
  }
  //  implicit def implicitValue(c: Counter) = c.value
}

class CounterUpDown(val stateCount: BigInt) extends ImplicitArea[UInt] {
  val incrementIt = False
  val decrementIt = False

  def increment(): Unit = incrementIt := True
  def decrement(): Unit = decrementIt := True

  def ===(that: UInt): Bool = this.value === that
  def !==(that: UInt): Bool = this.value =/= that
  def =/=(that: UInt): Bool = this.value =/= that

  val valueNext = UInt(log2Up(stateCount) bit)
  val value = RegNext(valueNext) init(0)
  val willOverflowIfInc = value === stateCount - 1 && !decrementIt
  val willOverflow = willOverflowIfInc && incrementIt

  val finalIncrement = UInt(log2Up(stateCount) bit)
  when(incrementIt && !decrementIt){
    finalIncrement := 1
  }elsewhen(!incrementIt && decrementIt){
    finalIncrement := finalIncrement.maxValue
  }otherwise{
    finalIncrement := 0
  }

  if (isPow2(stateCount)) {
    valueNext := (value + finalIncrement).resized
  }
  else {
    assert(false,"TODO")
  }


  override def implicitValue: UInt = this.value
}


object CounterMultiRequest {
  def apply(width: Int, requests : (Bool,(UInt) => UInt)*): UInt = {
    val counter = Reg(UInt(width bit)) init(0)
    var counterNext = cloneOf(counter)
    counterNext := counter
    for((cond,func) <- requests){
      when(cond){
        counterNext \= func(counterNext)
      }
    }
    counter := counterNext
    counter
  }
}



object LatencyAnalysis {
  //Don't care about clock domain
  def apply(paths: Expression*): Integer = list(paths)

  def list(paths: Seq[Expression]): Integer = {
    var stack = 0;
    for (i <- (0 to paths.size - 2)) {
      stack = stack + impl(paths(i), paths(i + 1))
    }
    stack
  }

  //TODO mather about clock and reset wire
  def impl(from: Expression, to: Expression): Integer = {
    val walkedId = GlobalData.get.allocateAlgoIncrementale()
    val pendingQueues = new Array[mutable.ArrayBuffer[BaseNode]](3)
    for(i <- 0 until pendingQueues.length) pendingQueues(i) = new ArrayBuffer[BaseNode]
    def walk(that: BaseNode): Boolean = {
      if(that.algoIncrementale == walkedId)
        return false
      that.algoIncrementale = walkedId
      if(that == from)
        return true

      that match{
        case that : Mem[_] => {
          that.foreachStatements{
            case port : MemWrite =>
              port.foreachDrivingExpression(input => {
                pendingQueues(1) += input
              })
            case port : MemReadWrite =>
              port.foreachDrivingExpression(input => {
                pendingQueues(1) += input
              })
            case port : MemReadSync =>
            case port : MemReadAsync =>
              //TODO other ports
          }
          return false
        }
        case that : BaseType => { //TODO IR when conds
          def walkInputs(func : (BaseNode) => Unit) = {
            that.foreachStatements(s => {
              s.foreachDrivingExpression(input => {
                func(input)
              })
              s.walkParentTreeStatementsUntilRootScope(tree => tree.walkDrivingExpressions(input => {
                func(input)
              }))
            })
          }
          if(that.isReg){
            walkInputs(input => pendingQueues(1) += input)
            return false
          } else {
            walkInputs(input => {
              if(walk(input))
                return true
            })
          }
          return false
        }
        case that : MemReadSync =>
          that.foreachDrivingExpression(input => {
            if(walk(input))
              return true
          })
          pendingQueues(1) += that.mem
          return false
        case that : MemReadWrite =>
          that.foreachDrivingExpression(input => {
            if(walk(input))
              return true
          })
          pendingQueues(1) += that.mem
          that.foreachDrivingExpression(input => {
            pendingQueues(2) += input
          })
          return false
        case that : MemReadAsync =>
          that.foreachDrivingExpression(input => {
            if(walk(input))
              return true
          })
          if(walk(that.mem))
            return true
          return false
        case that : Expression => {
          that.foreachDrivingExpression(input => {
            if(walk(input))
              return true
          })
          return false
        }
      }
    }

    var depth = 0
    pendingQueues(0) += to
    while(pendingQueues.exists(_.nonEmpty)){
      pendingQueues(0).foreach(node => {
        if(walk(node))
          return depth
      })


      pendingQueues(0).clear()
      pendingQueues(pendingQueues.length - 1) = pendingQueues(0)
      for(i <- 0 until pendingQueues.length - 1){
        pendingQueues(i) = pendingQueues(i + 1)
      }

      depth += 1
    }



    SpinalError("latencyAnalysis don't find any path")
    -1
//    val walked = mutable.Set[Expression]()
//    var pendingStack = mutable.ArrayBuffer[Expression](to)
//    var depth = 0;
//
//    while (pendingStack.size != 0) {
//      val iterOn = pendingStack
//      pendingStack = new mutable.ArrayBuffer[Expression](10000)
//      for (start <- iterOn) {
//        if (walk(start)) return depth;
//      }
//      depth = depth + 1
//    }
//
//    def walk(that: Expression, depth: Integer = 0): Boolean = {
//      if (that == null) return false
//      if (walked.contains(that)) return false
//      walked += that
//      if (that == from)
//        return true
//      that match {
//        case delay: SyncNode => {
//          for (input <- delay.getAsynchronousInputs) {
//            if (walk(input)) return true
//          }
//          pendingStack ++= delay.getSynchronousInputs
//        }
//        case _ => {
//          that.onEachInput(input =>  {
//            if (walk(input)) return true
//          })
//        }
//      }
//      false
//    }
//
//    SpinalError("latencyAnalysis don't find any path")
//    -1
  }
}

object DataCarrier{
  implicit def toImplicit[T <: Bundle](dataCarrier: DataCarrier[T]): T = dataCarrier.payload
  implicit def toImplicit2[T <: Bundle](dataCarrier: DataCarrier[Fragment[T]]): T = dataCarrier.fragment
}

trait DataCarrier[T <: Data] {
  def fire: Bool
  def valid: Bool
  def payload: T

  def freeRun(): this.type
}


object DelayEvent {
  def apply(event: Bool, t: Double, hz: Double): Bool = {
    DelayEvent(event, ((t - 100e-12) * hz).ceil.toInt)
  }

  def apply(event: Bool, cycle: BigInt): Bool = {
    if (cycle == 0) return event
    val run = RegInit(False)
    val counter = Counter(cycle)

    counter.increment()

    when(counter.willOverflow) {
      run := False
    }

    when(event) {
      run := True
      counter.clear()
    }

    return run && counter.willOverflow
  }

  def apply(event: Bool, cycle: UInt): Bool = {
    val ret = False
    val isDelaying = RegInit(False)
    val counterNext = cloneOf(cycle)
    val counter = RegNext(counterNext)
    val counterMatch = counterNext === cycle

    counterNext := counter + 1

    when(event) {
      counterNext := 0
      when(counterMatch) {
        isDelaying := False
        ret := True
      } otherwise {
        isDelaying := True
      }
    }elsewhen(isDelaying) {
      when(counterMatch) {
        isDelaying := False
        ret := True
      }
    }

    ret
  }

}


class NoData extends Bundle {

}


class TraversableOnceAnyPimped[T <: Any](pimped: Seq[T]) {
  def apply(id : UInt)(gen : (T) => Unit): Unit ={
    assert(widthOf(id) == log2Up(pimped.size))
    for((e,i) <- pimped.zipWithIndex) {
      when(id === i){
        gen(e)
      }
    }
  }

  def reduceBalancedTree(op: (T, T) => T): T = {
    reduceBalancedTree(op, (s,l) => s)
  }
  def reduceBalancedTree(op: (T, T) => T, levelBridge: (T, Int) => T): T = {
    def stage(elements: ArrayBuffer[T], level: Int): T = {
      if (elements.length == 1) return elements.head
      val stageLogic = new ArrayBuffer[T]()
      val logicCount = (elements.length + 1) / 2

      for (i <- 0 until logicCount) {
        if (i * 2 + 1 < elements.length)
          stageLogic += levelBridge(op(elements(i * 2), elements(i * 2 + 1)), level)
        else
          stageLogic += levelBridge(elements(i * 2), level)
      }
      stage(stageLogic, level + 1)

    }
    val array = ArrayBuffer[T]() ++ pimped
    assert(array.length >= 1)
    stage(array, 0)
  }

}

class TraversableOnceBoolPimped(pimped: Seq[Bool]) {
  def orR: Bool = pimped.asBits =/= 0
  def andR: Bool = pimped.reduce(_ && _)
  def xorR: Bool = pimped.reduce(_ ^ _)
}

class TraversableOncePimped[T <: Data](pimped: Seq[T]) {
  def reduceBalancedTree(op: (T, T) => T): T =  new TraversableOnceAnyPimped[T](pimped).reduceBalancedTree(op)
  def reduceBalancedTree(op: (T, T) => T, levelBridge: (T, Int) => T): T =  new TraversableOnceAnyPimped[T](pimped).reduceBalancedTree(op, levelBridge)
  def asBits() : Bits = Cat(pimped)


  def read(idx: UInt): T = {
    Vec(pimped).read(idx)
  }
  def write(index: UInt, data: T): Unit = {
    apply(index) := data
  }
  def write(index: Int, data: T): Unit = {
    apply(index) := data
  }
  def apply(index: UInt): T = Vec(pimped)(index)
  def apply(index: Int): T = Vec(pimped)(index)

  def sExist(condition: T => Bool): Bool = (pimped map condition).fold(False)(_ || _)
  def sContains(value: T) : Bool = sExist(_ === value)

  def sCount(condition: T => Bool): UInt = SetCount((pimped.map(condition)))
  def sCount(value: T): UInt = sCount(_ === value)

  def sFindFirst(condition: T => Bool) : (Bool,UInt) = {
    val size = pimped.size
    val hits = pimped.map(condition(_))
    val hitValid = hits.reduceLeft(_ || _)
    val hitValue = PriorityMux(hits,(0 until size).map(U(_,log2Up(size) bit)))
    (hitValid,hitValue)
  }
}


object Delay {
  def apply[T <: Data](that: T, cycleCount: Int,when : Bool = null,init : T = null.asInstanceOf[T],onEachReg : T => Unit = null.asInstanceOf[T => Unit]): T = {
    require(cycleCount >= 0,"Negative cycleCount is not allowed in Delay")
    var ptr = that
    for(i <- 0 until cycleCount) {
      if (when == null)
        ptr = RegNext(ptr, init)
      else
        ptr = RegNextWhen(ptr, when, init)

      ptr.unsetName().setCompositeName(that, "delay_" + (i + 1), true)
      if(onEachReg != null) {
        onEachReg(ptr)
      }
    }
    ptr
  }
}

object DelayWithInit {
  def apply[T <: Data](that: T, cycleCount: Int)(onEachReg: (T) => Unit = null): T = {
    Delay[T](that, cycleCount, onEachReg = onEachReg)
  }
}

object History {
  def apply[T <: Data](that: T, length: Int, when: Bool = null, init: T = null): Vec[T] = {
    def builder(that: T, left: Int): List[T] = {
      left match {
        case 0 => Nil
        case 1 => that :: Nil
        case _ => that :: builder({
          if (when != null)
            RegNextWhen(that, when, init = init)
          else
            RegNext(that, init = init)
        }, left - 1)
      }
    }
    val inputBuffer = cloneOf(that); inputBuffer := that
    Vec(builder(inputBuffer, length))
  }

  def apply[T <: Data](that: T, range: Range, when: Bool, init: T): Vec[T] = {
    val ret = Vec(cloneOf(that), range.length)
    (ret, History(that, range.high + 1, when, init).drop(range.low)).zipped.foreach(_ := _)
    ret
  }

  def apply[T <: Data](that: T, range: Range, init: T): Vec[T] =
    apply(that, range, null, init = init)

  def apply[T <: Data](that: T, range: Range, when: Bool): Vec[T] =
    apply(that, range, when, null.asInstanceOf[T])

  def apply[T <: Data](that: T, range: Range): Vec[T] =
    apply(that, range, null, null.asInstanceOf[T])

}

object SetCount
{
  def apply(in: Iterable[Bool]): UInt = {
    if (in.size == 0) {
      U(0)
    } else if (in.size == 1) {
      in.head.asUInt
    } else {
      (U(0,1 bit) ## apply(in.slice(0, in.size/2))).asUInt + apply(in.slice(in.size/2, in.size))
    }
  }
  def apply(in: Bits): UInt = apply((0 until in.getWidth).map(in(_)))
}

object ClearCount{
  def apply(in: Iterable[Bool]): UInt = SetCount(in.map(!_))
  def apply(in: Bits) : UInt = SetCount(~in)
}

class StringPimped(pimped : String){
  def toVecOfByte(encoding : String = "UTF-8") : Vec[Bits] = {
    Vec(pimped.getBytes(Charset.forName(encoding)).map(b => B(b.toInt, 8 bit)))
  }
}


object PriorityMux{
  def apply[T <: Data](in: Seq[(Bool, T)]): T = {
    if (in.size == 1) {
      in.head._2
    } else {
      Mux(in.head._1, in.head._2, apply(in.tail)) //Inttelij right code marked red
    }
  }
  def apply[T <: Data](sel: Seq[Bool], in: Seq[T]): T = apply(sel zip in)
  def apply[T <: Data](sel: Bits, in: Seq[T]): T = apply(sel.asBools.zip(in))
}



object WrapWithReg{
  def on(c : Component): Unit = {
    for(e <- c.getOrdredNodeIo){
      if(e.isInput){
        e := RegNext(RegNext(in(cloneOf(e).setName(e.getName))))
      }else{
        out(cloneOf(e).setName(e.getName)) := RegNext(RegNext(e))
      }
    }
  }

  class Wrapper(c :  => Component) extends Component{
    val comp = c
    on(comp)
  }
}



object Callable{
  def apply(doIt : => Unit) = new Area{
    val isCalled = False
    when(isCalled){doIt}

    def call() = isCalled := True
  }
}

case class DataOr[T <: Data](dataType : HardType[T]) extends Area{
  val value = dataType()
  val values = ArrayBuffer[T]()
  Component.current.afterElaboration{
    values.size match {
      case 0 => value := value.getZero
      case _ => value.assignFromBits(values.map(_.asBits).reduceBalancedTree(_ | _))
    }
  }
  def newPort(): T ={
    val port = dataType()
    values += port
    port
  }
}