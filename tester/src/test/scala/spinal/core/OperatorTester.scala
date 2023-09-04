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

import spinal.tester.SpinalTesterCocotbBase

object OperatorTester {
  object State extends SpinalEnum{
    val a,b,c = newElement
  }


  class OperatorTester extends Component {
    val uint4 = in UInt(4 bits)
    val uint8 = in UInt(8 bits)
    val uint32 = in UInt(32 bits)
    val sint4 = in SInt(4 bits)
    val sint8 = in SInt(8 bits)
    val sint32 = in SInt(32 bits)
    val bits4 = in Bits(4 bits)
    val bits8 = in Bits(8 bits)
    val bits32 = in Bits(32 bits)
    val boolA,boolB,boolC = in Bool()
    val uint4NotZero = in UInt(4 bits)
    val uint8NotZero = in UInt(8 bits)
    val sint4NotZero = in SInt(4 bits)
    val sint8NotZero = in SInt(8 bits)


//    def notZero(x : UInt) = ((x === 0) ? (x + 1) | x)
//    def notZero(x : SInt) = ((x === 0) ? (x + 1) | x)


    val boolMux = out(Mux(boolC,boolA,boolB))

    val bitsBsMux = out(Mux(boolC,bits8,bits4.resized))
    val uintBsMux = out(Mux(boolC,uint8,uint4))
    val sintBsMux = out(Mux(boolC,sint8,sint4))
    val bitsSbMux = out(Mux(boolC,bits4.resized,bits8))
    val uintSbMux = out(Mux(boolC,uint4,uint8))
    val sintSbMux = out(Mux(boolC,sint4,sint8))


    val uintNot = out(~uint4)
    val uintShiftLeftInt = out(uint8 << 4)
    val uintShiftLeftUint = out(uint8 << uint4)
    val uintShiftRightInt = out(uint8 >> 4)
    val uintShiftRightUint = out(uint8 >> uint4)
    val uintShiftLeftIntFixedWidth   = out(uint8 |<< 4)
    val uintShiftLeftUintFixedWidth  = out(uint8 |<< uint4)
    val uintShiftRightIntFixedWidth  = out(uint8 |>> 4)
    val uintShiftRightUintFixedWidth = out(uint8 |>> uint4)

    val sintNot = out(~sint4)
    val sintShiftLeftInt = out(sint8 << 4)
    val sintShiftLeftUint = out(sint8 << uint4)
    val sintShiftRightInt = out(sint8 >> 4)
    val sintShiftRightUint = out(sint8 >> uint4)
    val sintShiftLeftIntFixedWidth   = out(sint8 |<< 4)
    val sintShiftLeftUintFixedWidth  = out(sint8 |<< uint4)
    val sintShiftRightIntFixedWidth  = out(sint8 |>> 4)
    val sintShiftRightUintFixedWidth = out(sint8 |>> uint4)
    val sintMinus = out(-sint4)

    val bitsNot = out(~bits4)
    val bitsShiftLeftInt = out(bits8 << 4)
    val bitsShiftLeftUint = out(bits8 << uint4)
    val bitsShiftRightInt = out(bits8 >> 4)
    val bitsShiftRightUint = out(bits8 >> uint4)
    val bitsShiftLeftIntFixedWidth = out(bits8 |<< 4)
    val bitsShiftLeftUintFixedWidth = out(bits8 |<< uint4)
    val bitsShiftRightIntFixedWidth = out(bits8 |>> 4)
    val bitsShiftRightUintFixedWidth = out(bits8 |>> uint4)

    val uintSbEquals= out(uint4 === uint8)
    val uintSbNotEquals = out(uint4 =/= uint8)

    val uintSbAdd = out(uint4 + uint8)
    val uintSbSub = out(uint4 - uint8)
    val uintSbMul = out(uint4 * uint8)
    val uintSbDiv = out(uint4 / uint8NotZero)
    val uintSbRem = out(uint4 % uint8NotZero)

    val uintSbAnd = out(uint4.resized & uint8)
    val uintSbOr = out(uint4.resized  | uint8)
    val uintSbXor = out(uint4.resized  ^ uint8)

    val uintSbSmaller = out(uint4 < uint8)
    val uintSbSmallerEquals = out(uint4 <= uint8)
    val uintSbBigger = out(uint4 > uint8)
    val uintSbBiggerEquals = out(uint4 >= uint8)

    val sintSbEquals= out(sint4 === sint8)
    val sintSbNotEquals = out(sint4 =/= sint8)

    val sintSbAdd = out(sint4 + sint8)
    val sintSbSub = out(sint4 - sint8)
    val sintSbMul = out(sint4 * sint8)
    val sintSbDiv = out(sint4 / sint8NotZero)
    val sintSbRem = out(sint4 % sint8NotZero)

    val sintSbAnd = out(sint4.resized & sint8)
    val sintSbOr = out(sint4.resized | sint8)
    val sintSbXor = out(sint4.resized ^ sint8)

    val sintSbSmaller = out(sint4 < sint8)
    val sintSbSmallerEquals = out(sint4 <= sint8)
    val sintSbBigger = out(sint4 > sint8)
    val sintSbBiggerEquals = out(sint4 >= sint8)



    val bitsSbEquals= out(bits4.resized === bits8)
    val bitsSbNotEquals = out(bits4.resized =/= bits8)

    val bitsSbAnd = out(bits4.resized & bits8)
    val bitsSbOr = out(bits4.resized | bits8)
    val bitsSbXor = out(bits4.resized ^ bits8)



    val uintBsEquals= out(uint8 === uint4)
    val uintBsNotEquals = out(uint8 =/= uint4)

    val uintBsAdd = out(uint8 + uint4)
    val uintBsSub = out(uint8 - uint4)
    val uintBsMul = out(uint8 * uint4)
    val uintBsDiv = out(uint8 / uint4NotZero)
    val uintBsRem = out(uint8 % uint4NotZero)

    val uintBsAnd = out(uint8 & uint4.resized)
    val uintBsOr = out(uint8 | uint4.resized)
    val uintBsXor = out(uint8 ^ uint4.resized)
    val uintBsNot = out(~uint8)

    val uintBsSmaller = out(uint8 < uint4)
    val uintBsSmallerEquals = out(uint8 <= uint4)
    val uintBsBigger = out(uint8 > uint4)
    val uintBsBiggerEquals = out(uint8 >= uint4)

    val sintBsEquals= out(sint8 === sint4)
    val sintBsNotEquals = out(sint8 =/= sint4)

    val sintBsAdd = out(sint8 + sint4)
    val sintBsSub = out(sint8 - sint4)
    val sintBsMul = out(sint8 * sint4)
    val sintBsDiv = out(sint8 / sint4NotZero)
    val sintBsRem = out(sint8 % sint4NotZero)

    val sintBsAnd = out(sint8 & sint4.resized)
    val sintBsOr = out(sint8 | sint4.resized)
    val sintBsXor = out(sint8 ^ sint4.resized)
    val sintBsNot = out(~sint8)

    val sintBsSmaller = out(sint8 < sint4)
    val sintBsSmallerEquals = out(sint8 <= sint4)
    val sintBsBigger = out(sint8 > sint4)
    val sintBsBiggerEquals = out(sint8 >= sint4)

    val bitsBsEquals= out(bits8 === bits4.resized)
    val bitsBsNotEquals = out(bits8 =/= bits4.resized)

    val bitsBsAnd = out(bits8 & bits4.resized)
    val bitsBsOr = out(bits8 | bits4.resized)
    val bitsBsXor = out(bits8 ^ bits4.resized)
    val bitsBsNot = out(~bits8)

    val bitsCat = out(bits8 ## bits4)





    val boolEquals    = out(boolA === boolB)
    val boolNotEquals = out(boolA =/= boolB)
    
    val boolAnd = out(boolA & boolB)
    val boolOr = out(boolA | boolB)
    val boolXor = out(boolA ^ boolB)
    val boolNot = out(!boolA)



    val uintAsBits = out(uint8.asBits)
    val uintAsSint = out(uint8.asSInt)

    val sintAsBits = out(sint8.asBits)
    val sintAsUint = out(sint8.asUInt)

    val bitsAsUint = out(bits8.asUInt)
    val bitsAsSint = out(bits8.asSInt)

    val boolAsBits = out(boolA.asBits)
    val boolAsUInt = out(boolA.asUInt)
    val boolAsSInt = out(boolA.asSInt)



    val bitsResizeBigger  = out(bits8.resize(16))
    val bitsResizeSmaller = out(bits8.resize(4))

    val uintResizeBigger  = out(uint8.resize(16))
    val uintResizeSmaller = out(uint8.resize(4))

    val sintResizeBigger  = out(sint8.resize(16))
    val sintResizeSmaller = out(sint8.resize(4))




    val stateNative,stateNativeMuxInternal = State(native)
    stateNative.assignFromBits(bits8(1 downto 0))
    val stateNativeBits   = out(stateNative.asBits)
    val stateNativeIsA    = out(stateNative === State.a)
    val stateNativeIsB    = out(stateNative === State.b)
    val stateNativeIsC    = out(stateNative === State.c)
    val stateNativeIsNotA = out(stateNative =/= State.a)
    val stateNativeIsNotB = out(stateNative =/= State.b)
    val stateNativeIsNotC = out(stateNative =/= State.c)
    stateNativeMuxInternal := Mux(boolC,State.a,State.b)
    val stateNativeMux = out(stateNativeMuxInternal.asBits)


    val stateBinarySequancial,stateBinarySequancialMux = State(binarySequential)
    stateBinarySequancial.assignFromBits(bits8(1 downto 0))
    val stateBinarySequancialBits   = out(stateBinarySequancial.asBits)
    val stateBinarySequancialIsA    = out(stateBinarySequancial === State.a)
    val stateBinarySequancialIsB    = out(stateBinarySequancial === State.b)
    val stateBinarySequancialIsC    = out(stateBinarySequancial === State.c)
    val stateBinarySequancialIsNotA = out(stateBinarySequancial =/= State.a)
    val stateBinarySequancialIsNotB = out(stateBinarySequancial =/= State.b)
    val stateBinarySequancialIsNotC = out(stateBinarySequancial =/= State.c)
    stateBinarySequancialMux    := Mux(boolC,State.a,State.b)
    stateBinarySequancialMux.asOutput()

    val stateBinaryOneHot,stateBinaryOneHotMux = State(binaryOneHot)
    stateBinaryOneHot.assignFromBits(bits8(2 downto 0))
    val stateBinaryOneHotBits = out(stateBinaryOneHot.asBits)
    val stateBinaryOneHotIsA    = out(stateBinaryOneHot === State.a)
    val stateBinaryOneHotIsB    = out(stateBinaryOneHot === State.b)
    val stateBinaryOneHotIsC    = out(stateBinaryOneHot === State.c)
    val stateBinaryOneHotIsNotA = out(stateBinaryOneHot =/= State.a)
    val stateBinaryOneHotIsNotB = out(stateBinaryOneHot =/= State.b)
    val stateBinaryOneHotIsNotC = out(stateBinaryOneHot =/= State.c)
    stateBinaryOneHotMux := (Mux(boolC,State.a,State.b))
    stateBinaryOneHotMux.asOutput()



    val bitsAggregateFixed = out(B(7 -> false,(6 downto 5) -> true,(4 downto 3) -> bits8(1 downto 0),(2 downto 1) -> boolA,0 -> True))
    val uintAggregateFixed = out(U(7 -> false,(6 downto 5) -> true,(4 downto 3) -> uint8(1 downto 0),(2 downto 1) -> boolA,0 -> True))
    val sintAggregateFixed = out(S(7 -> false,(6 downto 5) -> true,(4 downto 3) -> sint8(1 downto 0),(2 downto 1) -> boolA,0 -> True))

    val bitsAggregateUnfixedWidthFixedDefault = out(B(7 -> false,(6 downto 5) -> true,(4 downto 3) -> bits8(1 downto 0),0 -> True,(2 downto 1) -> true))
    val uintAggregateUnfixedWidthFixedDefault = out(U(7 -> false,(6 downto 5) -> true,(4 downto 3) -> uint8(1 downto 0),0 -> True,(2 downto 1) -> true))
    val sintAggregateUnfixedWidthFixedDefault = out(S(7 -> false,(6 downto 5) -> true,(4 downto 3) -> sint8(1 downto 0),0 -> True,(2 downto 1) -> true))

    val bitsAggregateUnfixedWidthUnfixedDefault = out Bits(8 bits)
    val uintAggregateUnfixedWidthUnfixedDefault = out UInt(8 bits)
    val sintAggregateUnfixedWidthUnfixedDefault = out SInt(8 bits)

    bitsAggregateUnfixedWidthUnfixedDefault := (7 -> false,(6 downto 5) -> true,(4 downto 3) -> bits8(1 downto 0),0 -> True,default -> boolA)
    uintAggregateUnfixedWidthUnfixedDefault := (7 -> false,(6 downto 5) -> true,(4 downto 3) -> uint8(1 downto 0),0 -> True,default -> boolA)
    sintAggregateUnfixedWidthUnfixedDefault := (7 -> false,(6 downto 5) -> true,(4 downto 3) -> sint8(1 downto 0),0 -> True,default -> boolA)




    val bitsRotateLeftUInt = out(bits32(26 downto 0).rotateLeft(uint8(4 downto 0)))
    val uintRotateLeftUInt = out(bits32(26 downto 0).asUInt.rotateLeft(uint8(4 downto 0)))
    val sintRotateLeftUInt = out(bits32(26 downto 0).asSInt.rotateLeft(uint8(4 downto 0)))
    val bitsRotateRightUInt = out(bits32(26 downto 0).rotateRight(uint8(4 downto 0)))
    val uintRotateRightUInt = out(bits32(26 downto 0).asUInt.rotateRight(uint8(4 downto 0)))
    val sintRotateRightUInt = out(bits32(26 downto 0).asSInt.rotateRight(uint8(4 downto 0)))
  }
}

class OperatorTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "OperatorTester"
  override def createToplevel: Component =  new OperatorTester.OperatorTester
  override def pythonTestLocation: String = "tester/src/test/python/spinal/OperatorTester"
}

//object OperatorTesterGenVhdl{
//  def main(args: Array[String]) {
//    SpinalVhdl(new OperatorTester)
//  }
//}