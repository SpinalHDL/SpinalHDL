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

import scala.io.Source

object ZeroWidthTester {


  class ZeroWidthTester extends Component {
    val uint8 = in UInt(8 bits)
    val sint8 = in SInt(8 bits)
    val bits8 = in Bits(8 bits)

    val uint0Src = U(0)
    val sint0Src = S(0)
    val bits0Src = B(0)

    val uint0 = UInt(0 bits)
    val sint0 = SInt(0 bits)
    val bits0 = Bits(0 bits)

    uint0 := uint0Src
    sint0 := sint0Src
    bits0 := bits0Src

    val bitsShiftLeftInt = out(bits0 << 4)
    val uintShiftLeftInt = out(uint0 << 4)
    val sintShiftLeftInt = out(sint0 << 4)

    val uint08ShiftLeftUint  = out(uint0 << uint8)
    val sint08ShiftLeftUint  = out(sint0 << uint8)
    val bits08ShiftLeftUint  = out(bits0 << uint8)



    val uint08Equals    = out(uint0 === uint8)
    val uint08NotEquals = out(uint0 =/= uint8)

    val uint08Add = out(uint0 + uint8)
    val uint08Sub = out(uint0 - uint8)
    val uint08Mul = out(uint0 * uint8)

    val uint08And = out(uint0.resized & uint8)
    val uint08Or  = out(uint0.resized | uint8)
    val uint08Xor = out(uint0.resized ^ uint8)

    val uint08Smaller       = out(uint0 < uint8)
    val uint08SmallerEquals = out(uint0 <= uint8)
    val uint08Bigger        = out(uint0 > uint8)
    val uint08BiggerEquals  = out(uint0 >= uint8)


    val sint08Equals= out(sint0 === sint8)
    val sint08NotEquals = out(sint0 =/= sint8)

    val sint08Add = out(sint0 + sint8)
    val sint08Sub = out(sint0 - sint8)
    val sint08Mul = out(sint0 * sint8)

    val sint08And = out(sint0.resized & sint8)
    val sint08Or = out(sint0.resized | sint8)
    val sint08Xor = out(sint0.resized ^ sint8)

    val sint08Smaller = out(sint0 < sint8)
    val sint08SmallerEquals = out(sint0 <= sint8)
    val sint08Bigger = out(sint0 > sint8)
    val sint08BiggerEquals = out(sint0 >= sint8)



    val bits08Equals= out(0 === bits8)
    val bits08NotEquals = out(0 =/= bits8)

    val bits08And = out(bits0.resized & bits8)
    val bits08Or = out(bits0.resized | bits8)
    val bits08Xor = out(bits0.resized ^ bits8)


    val uint80ShiftLeftUint  = out(uint8 >> uint0)
    val sint80ShiftLeftUint  = out(sint8 >> uint0)
    val bits80ShiftLeftUint  = out(bits8 >> uint0)

    val uint80Equals= out(uint8 === uint0)
    val uint80NotEquals = out(uint8 =/= uint0)

    val uint80Add = out(uint8 + uint0)
    val uint80Sub = out(uint8 - uint0)
    val uint80Mul = out(uint8 * uint0)

    val uint80And = out(uint8 & uint0.resized)
    val uint80Or = out(uint8 | uint0.resized)
    val uint80Xor = out(uint8 ^ uint0.resized)

    val uint80Smaller = out(uint8 < uint0)
    val uint80SmallerEquals = out(uint8 <= uint0)
    val uint80Bigger = out(uint8 > uint0)
    val uint80BiggerEquals = out(uint8 >= uint0)

    val sint80Equals= out(sint8 === sint0)
    val sint80NotEquals = out(sint8 =/= sint0)

    val sint80Add = out(sint8 + sint0)
    val sint80Sub = out(sint8 - sint0)
    val sint80Mul = out(sint8 * sint0)

    val sint80And = out(sint8 & sint0.resized)
    val sint80Or = out(sint8 | sint0.resized)
    val sint80Xor = out(sint8 ^ sint0.resized)

    val sint80Smaller = out(sint8 < sint0)
    val sint80SmallerEquals = out(sint8 <= sint0)
    val sint80Bigger = out(sint8 > sint0)
    val sint80BiggerEquals = out(sint8 >= sint0)

    val bits80Equals= out(bits8 === bits0.resized)
    val bits80NotEquals = out(bits8 =/= bits0.resized)

    val bits80And = out(bits8 & bits0.resized)
    val bits80Or = out(bits8 | bits0.resized)
    val bits80Xor = out(bits8 ^ bits0.resized)


    val bitsResizeBigger  = out(bits0.resize(16))
    val uintResizeBigger  = out(uint0.resize(16))
    val sintResizeBigger  = out(sint0.resize(16))

    val bits08Cat = out(bits0 ## bits8)
    val bits80Cat = out(bits8 ## bits0)

  }
}

class ZeroWidthTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "ZeroWidthTester"
  override def createToplevel: Component =  new ZeroWidthTester.ZeroWidthTester
  override def pythonTestLocation: String = "tester/src/test/python/spinal/ZeroWidthTester"

  override def postTest: () => Unit = () => {
    if(!noVerilog){
      val iterator = Source.fromFile(s"$workspaceRoot/ZeroWidthTester.v").getLines()
      for (line <- iterator) {
        assert(!line.contains("-1"))
      }
    }
    if(!noVhdl) {
      val iterator = Source.fromFile(s"$workspaceRoot/ZeroWidthTester.vhd").getLines()
      var wait = true
      for (line <- iterator) {
        if(wait){
          if(line == "entity ZeroWidthTester is") wait = false
        }else {
          assert(!line.contains("-1"))
        }
      }
    }
  }
}