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

import java.io.UTFDataFormatException
import java.nio.charset.Charset

import spinal.core._
import sun.text.normalizer.UTF16

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer



object OHToUInt {
  def apply(bitVector: BitVector): UInt = apply(bitVector.asBools)
  def apply(bools: Seq[Bool]): UInt = {
    val boolsSize = bools.size
    if (boolsSize < 2) return U(0)

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
}

//Will be target dependent
object MuxOH {
  def apply[T <: Data](oneHot : BitVector,inputs : Seq[T]): T = apply(oneHot.asBools,inputs)
  def apply[T <: Data](oneHot : collection.IndexedSeq[Bool],inputs : Iterable[T]): T =  apply(oneHot,Vec(inputs))

  def apply[T <: Data](oneHot : BitVector,inputs : Vec[T]): T = apply(oneHot.asBools,inputs)
  def apply[T <: Data](oneHot : collection.IndexedSeq[Bool],inputs : Vec[T]): T = inputs(OHToUInt(oneHot))
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
    var ret = UInt(log2Up(thats.length+1) bit)
    ret := 0
    for(e <- thats){
      when(e){
        ret \= ret + 1
      }
    }
    ret
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
    val ret = UInt(widthOf(gray) bit)
    for (i <- 0 until widthOf(gray) - 1) {
      ret(i) := gray(i) ^ ret(i + 1)
    }
    ret.msb := gray.msb
    ret
  }
}



/******************************************************************************
  * Linear feedback shift register (LFSR)
  *   There are 2 types of LFSR : Fibonacci and Galois
  */
object LFSR{

  /**
   * Shift register direction
   */
  trait LFSR_SHIFT_DIR
  case object SHIFT_LEFT  extends LFSR_SHIFT_DIR
  case object SHIFT_RIGHT extends LFSR_SHIFT_DIR


  /****************************************************************************
    * LFSR Fibonacci
    *
    * Right :
    *        ____ ____ ____     _____ _____ _____
    *   /-->|_31_|_30_|_29_|...|__2__|__1__|__0__|
    *   |          |              |           |
    *   \<--------XOR<-----------XOR<---------/
    *
    *   e.g : val result = LSFR(myBits, Seq(30,2,0))
    *
    * Left :
    *     ____ ____ ____     _____ _____ _____
    *    |_31_|_30_|_29_|...|__2__|__1__|__0__|<-\
    *      |                   |     |           |
    *      \----------------->XOR-->XOR----------/
    *
    *   e.g : val result = LSFR(myBits, Seq(31,2,1), LFSR_SHIFT_DIR.SHIFT_LEFT)
    *
    * @param that       : Signal to shift
    * @param xorBits    : List of index that must be xor
    * @param rightLeft  : Shift direction (SHIFT_RIGHT, SHIFT_LEFT)
    */
  def fibonacci(that : Bits, xorBits : Seq[Int], rightLeft : LFSR_SHIFT_DIR = SHIFT_RIGHT) : Bits = {

    assert(that.getWidth >= xorBits.size,  "xorBits length is bigger than the bit vector length")
    assert(xorBits.max <= that.getWidth-1, "number in xorBits is bigger than the index of the MSB of the bit vector")
    assert(xorBits.size >= 2, "At least 2 indexes must be specified in xorBits")

    val ret      = cloneOf(that)
    val feedback = (xorBits.map(that(_)).reduce(_ ^ _)).dontSimplifyIt()

    if(rightLeft == SHIFT_RIGHT){
      ret := feedback ## (that >> 1)
    }else{
      ret := (that << 1)(that.high downto 1) ## feedback
    }

    ret
  }


  /****************************************************************************
    * LFSR Galois
    *
    * Right :
    *        _____ _____        _____         _____ _____
    *    /->|__4__|__3__|-XOR->|__2__|--XOR->|__1__|__0__|
    *    |_________________|_____________|____________|
    *
    *    e.g: val result = LFSR_Galois(myBits, Seq(1,2))
    *
    * Left :
    *       _____ _____        _____         _____ _____
    *      |__4__|__3__|<-XOR-|__2__|<--XOR-|__1__|__0__|<-\
    *         |____________|_____________|_________________|
    *
    *    e.g: val result = LFSR_Galois(myBits, Seq(2,3), LFSR_SHIFT_DIR.SHIFT_LEFT)
    *
    * @param that       : Signal to shift
    * @param xorBits    : List of index that must be xor
    * @param rightLeft  : Shift direction (SHIFT_RIGHT, SHIFT_LEFT)
    */
  def galois(that : Bits, xorBits : Seq[Int],  rightLeft : LFSR_SHIFT_DIR = SHIFT_RIGHT): Bits ={

    assert(that.getWidth >= xorBits.size,  "xorBits length is bigger than the bit vector length")
    assert(xorBits.max <= that.getWidth-1, "number in xorBits is bigger than the index of the MSB of the bit vector")

    val ret = cloneOf(that)

    val bitsList = new ListBuffer[Bool]()

    if (rightLeft == SHIFT_RIGHT){

      for (index <- that.high to 0 by -1){
        if (index == that.high) {
          bitsList += that.lsb
        }else if(xorBits.contains(index)) {
          bitsList += that(index + 1) ^ that(0)
        }else{
          bitsList += that(index+1)
        }
      }
      ret := Cat(bitsList.reverse)
    }else{

      for (index <- 0 to that.high){
        if(index == 0){
          bitsList += that.msb
        }else if(xorBits.contains(index)) {
          bitsList += that(index - 1) ^ that.msb
        }else{
          bitsList += that(index - 1)
        }
      }
      ret := Cat(bitsList)
    }
    ret
  }
}


/******************************************************************************
  * Big-Endian <-> Little-Endian
  */
object Endianness{
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
    c.willIncrement.removeAssignements()
    c.increment()
    c
  }
}

object Counter {
  def apply(low: BigInt,high: BigInt) : Counter  = new Counter(start = low, end = high)
  def apply(stateCount: BigInt): Counter = new Counter(start = 0, end = stateCount-1)
  def apply(range : Range) : Counter = Counter(low = range.start, high = range.end)
  def apply(stateCount: BigInt, inc: Bool): Counter = {
    val counter = Counter(stateCount)
    when(inc) {
      counter.increment()
    }
    counter
  }
//  implicit def implicitValue(c: Counter) = c.value
}

// start and end inclusive, up counter
class Counter(val start: BigInt,val end: BigInt) extends ImplicitArea[UInt] {
  require(start <= end)
  val willIncrement = False
  val willClear = False

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
    when(willIncrement && willOverflowIfInc){
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
}




object Timeout {
  def apply(limit: BigInt) = new Timeout(limit)
  def apply(limit: TimeNumber) = new Timeout((limit*ClockDomain.current.frequency.getValue).toBigInt())
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
  }.elsewhen(!incrementIt && decrementIt){
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
  def apply(paths: Node*): Integer = list(paths)

  def list(paths: Seq[Node]): Integer = {
    var stack = 0;
    for (i <- (0 to paths.size - 2)) {
      stack = stack + impl(paths(i), paths(i + 1))
    }
    stack
  }

  def impl(from: Node, to: Node): Integer = {
    val walked = mutable.Set[Node]()
    var pendingStack = mutable.ArrayBuffer[Node](to)
    var depth = 0;

    while (pendingStack.size != 0) {
      val iterOn = pendingStack
      pendingStack = new mutable.ArrayBuffer[Node](10000)
      for (start <- iterOn) {
        if (walk(start)) return depth;
      }
      depth = depth + 1
    }

    def walk(that: Node, depth: Integer = 0): Boolean = {
      if (that == null) return false
      if (walked.contains(that)) return false
      walked += that
      if (that == from)
        return true
      that match {
        case delay: SyncNode => {
          for (input <- delay.getAsynchronousInputs) {
            if (walk(input)) return true
          }
          pendingStack ++= delay.getSynchronousInputs
        }
        case _ => {
          that.onEachInput(input =>  {
            if (walk(input)) return true
          })
        }
      }
      false
    }

    SpinalError("latencyAnalysis don't find any path")
    -1
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
  @deprecated("Shoud use payload instead of data. Or directly myStream.myBundleElement in place of myStream.data.myBundleElement")
  //def data : T = payload
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
    }.elsewhen(isDelaying) {
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
  def toto = 2
  def apply(id : UInt)(gen : (T) => Unit): Unit ={
    assert(widthOf(id) == log2Up(pimped.size))
    for((e,i) <- pimped.zipWithIndex) {
      when(i === id){
        gen(e)
      }
    }
  }
}

class TraversableOnceBoolPimped(pimped: Seq[Bool]) {
  def orR: Bool = pimped.reduce(_ || _)
  def andR: Bool = pimped.reduce(_ && _)
  def xorR: Bool = pimped.reduce(_ ^ _)
}

class TraversableOncePimped[T <: Data](pimped: Seq[T]) {
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

  def asBits() : Bits = Cat(pimped)


  def read(idx: UInt): T = {
    Vec(pimped).read(idx)
  }
  def write(index: UInt, data: T): Unit = {
    read(index) := data
  }
  def apply(index: UInt): T = Vec(pimped)(index)

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
  def apply[T <: Data](that: T, cycleCount: Int,when : Bool = null,init : T = null.asInstanceOf[T]): T = {
    require(cycleCount >= 0,"Negative cycleCount is not allowed in Delay")
    cycleCount match {
      case 0 => that
      case _ => {
        if(when == null)
          Delay(RegNext(that,init), cycleCount - 1,when,init)
        else
          Delay(RegNextWhen(that,when,init), cycleCount - 1,when,init)
      }
    }
  }
}

object DelayWithInit {
  def apply[T <: Data](that: T, cycleCount: Int)(onEachReg : (T) => Unit = null): T = {
    cycleCount match {
      case 0 => that
      case _ => {
        val reg = RegNext(that)
        if(onEachReg != null) onEachReg(reg)
        DelayWithInit(reg, cycleCount - 1)(onEachReg)
      }
    }
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
    Vec(builder(that, length))
  }

  def apply[T <: Data](that: T, range: Range, when: Bool, init: T): Vec[T] =
    Vec(History(that, range.high + 1, when, init).drop(range.low))

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
    c.nameElements()
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
