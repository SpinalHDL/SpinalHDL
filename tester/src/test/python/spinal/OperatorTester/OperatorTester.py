import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer

from cocotblib.misc import randSignal, truncUInt, sint, truncSInt
import random

def check(signal,bitCount,value):
    if len(signal) != bitCount or int(signal) != truncUInt(value, bitCount):
        raise TestFailure("FAIL %s    %d != %d" % (signal._path, int(signal), truncUInt(value, bitCount)))

def checkSigned(signal,bitCount,value):
    if len(signal) != bitCount or sint(signal) != truncSInt(value,bitCount):
        raise TestFailure("FAIL %s    %d != %d" % (signal._path, sint(signal), truncSInt(value,bitCount)))

def notZero(x):
    if x == 0:
        return 1
    else:
        return x

def randSignalNotZero(that):
    value = random.getrandbits(len(that))
    if(value == 0):
        value += 1
    that <= value

@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")

    # itemDir = dut.uint4.__dict__
    #
    # for i in itemDir:
    #     print '{0}  :  {1}'.format(i, itemDir[i])

    for i in range(0,2000):
        randSignal(dut.uint4)
        randSignal(dut.uint8)
        randSignalNotZero(dut.uint4NotZero)
        randSignalNotZero(dut.uint8NotZero)
        randSignal(dut.uint32)
        randSignal(dut.sint4)
        randSignal(dut.sint8)
        randSignalNotZero(dut.sint4NotZero)
        randSignalNotZero(dut.sint8NotZero)
        randSignal(dut.sint32)
        randSignal(dut.bits4)
        randSignal(dut.bits8)
        randSignal(dut.bits32)
        randSignal(dut.boolA)
        randSignal(dut.boolB)
        randSignal(dut.boolC)
        yield Timer(1000)
        uint4 = int(dut.uint4)
        uint8 = int(dut.uint8)
        uint32 = int(dut.uint32)
        sint4 = sint(dut.sint4)
        sint8 = sint(dut.sint8)
        sint32 = sint(dut.sint32)
        bits4 = int(dut.bits4)
        bits8 = int(dut.bits8)
        bits32 = int(dut.bits32)
        boolA = int(dut.boolA)
        boolB = int(dut.boolB)
        boolC = int(dut.boolC)
        uint4NotZero =  int(dut.uint4NotZero)
        uint8NotZero =  int(dut.uint8NotZero)
        sint4NotZero = sint(dut.sint4NotZero)
        sint8NotZero = sint(dut.sint8NotZero)



        check(dut.boolMux,1, boolA if boolC else boolB)
        check(dut.bitsBsMux,8, bits8 if boolC else bits4)
        check(dut.uintBsMux,8, uint8 if boolC else uint4)
        check(dut.sintBsMux,8, sint8 if boolC else sint4)
        check(dut.bitsSbMux,8, bits4 if boolC else bits8)
        check(dut.uintSbMux,8, uint4 if boolC else uint8)
        check(dut.sintSbMux,8, sint4 if boolC else sint8)
        check(dut.stateNativeMux            ,2, 0 if boolC else 1)
        check(dut.stateBinarySequancialMux  ,2, 0 if boolC else 1)
        check(dut.stateBinaryOneHotMux      ,3, 1 if boolC else 2)

        check(dut.uintNot, 4, ~uint4)
        check(dut.uintShiftLeftInt  , 12, uint8 << 4)
        check(dut.uintShiftLeftUint , 23, uint8 << uint4)
        check(dut.uintShiftRightInt , 4, uint8 >> 4)
        check(dut.uintShiftRightUint, 8, uint8 >> uint4)
        check(dut.uintShiftLeftIntFixedWidth  , 8, uint8 << 4)
        check(dut.uintShiftLeftUintFixedWidth , 8, uint8 << uint4)
        check(dut.uintShiftRightIntFixedWidth , 8, uint8 >> 4)
        check(dut.uintShiftRightUintFixedWidth, 8, uint8 >> uint4)

        check(dut.sintNot, 4, ~sint4)
        checkSigned(dut.sintMinus, 4, -sint4)
        check(dut.sintShiftLeftInt  , 12, sint8 << 4)
        check(dut.sintShiftLeftUint , 23, sint8 << uint4)
        check(dut.sintShiftRightInt , 4, sint8 >> 4)
        checkSigned(dut.sintShiftRightUint, 8, sint8 >> uint4)
        check(dut.sintShiftLeftIntFixedWidth  , 8, sint8 << 4)
        check(dut.sintShiftLeftUintFixedWidth , 8, sint8 << uint4)
        checkSigned(dut.sintShiftRightIntFixedWidth , 8, sint8 >> 4)
        checkSigned(dut.sintShiftRightUintFixedWidth, 8, sint8 >> uint4)

        check(dut.bitsNot, 4, ~bits4)
        check(dut.bitsShiftLeftInt  , 12, bits8 << 4)
        check(dut.bitsShiftLeftUint , 23, bits8 << uint4)
        check(dut.bitsShiftRightInt , 4, bits8 >> 4)
        check(dut.bitsShiftRightUint, 8, bits8 >> uint4)
        check(dut.bitsShiftLeftIntFixedWidth  , 8, bits8 << 4)
        check(dut.bitsShiftLeftUintFixedWidth , 8, bits8 << uint4)
        check(dut.bitsShiftRightIntFixedWidth , 8, bits8 >> 4)
        check(dut.bitsShiftRightUintFixedWidth, 8, bits8 >> uint4)



        check(dut.uintSbEquals,         1, uint4 == uint8)
        check(dut.uintSbNotEquals,      1, uint4 != uint8)

        check(dut.uintSbAdd, 8, uint4 + uint8)
        check(dut.uintSbSub, 8, uint4 - uint8)
        check(dut.uintSbMul, 12, uint4 * uint8)
        if uint8NotZero != 0:
            check(dut.uintSbDiv, 4, uint4 // uint8NotZero)
            check(dut.uintSbRem, 4, uint4 % uint8NotZero)
        check(dut.uintSbAnd, 8, uint4 & uint8)
        check(dut.uintSbOr , 8, uint4 | uint8)
        check(dut.uintSbXor, 8, uint4 ^ uint8)

        check(dut.uintSbSmaller      , 1, uint4 < uint8)
        check(dut.uintSbSmallerEquals, 1, uint4 <= uint8)
        check(dut.uintSbBigger       , 1, uint4 > uint8)
        check(dut.uintSbBiggerEquals , 1, uint4 >= uint8)




        check(dut.sintSbEquals, 1, sint4 == sint8)
        check(dut.sintSbNotEquals, 1, sint4 != sint8)

        checkSigned(dut.sintSbAdd, 8, sint4 + sint8)
        checkSigned(dut.sintSbSub, 8, sint4 - sint8)
        checkSigned(dut.sintSbMul, 12, sint4 * sint8)
        if sint8NotZero != 0 and sint4 > 0 and sint8NotZero > 0:
            checkSigned(dut.sintSbDiv, 4,  int(sint4//sint8NotZero))
            checkSigned(dut.sintSbRem, 4,  int(sint4% sint8NotZero))

        checkSigned(dut.sintSbAnd, 8, sint4 & sint8)
        checkSigned(dut.sintSbOr, 8, sint4 | sint8)
        checkSigned(dut.sintSbXor, 8, sint4 ^ sint8)

        check(dut.sintSbSmaller, 1, sint4 < sint8)
        check(dut.sintSbSmallerEquals, 1, sint4 <= sint8)
        check(dut.sintSbBigger, 1, sint4 > sint8)
        check(dut.sintSbBiggerEquals, 1, sint4 >= sint8)




        check(dut.bitsSbEquals, 1, bits4 == bits8)
        check(dut.bitsSbNotEquals, 1, bits4 != bits8)

        check(dut.bitsSbAnd, 8, bits4 & bits8)
        check(dut.bitsSbOr, 8, bits4 | bits8)
        check(dut.bitsSbXor, 8, bits4 ^ bits8)








        check(dut.uintBsEquals,         1, uint8 == uint4)
        check(dut.uintBsNotEquals,      1, uint8 != uint4)

        check(dut.uintBsAdd, 8, uint8 + uint4)
        check(dut.uintBsSub, 8, uint8 - uint4)
        check(dut.uintBsMul, 12, uint8 * uint4)
        if uint4NotZero != 0:
            check(dut.uintBsDiv, 8, uint8 // uint4NotZero)
            check(dut.uintBsRem, 8, uint8 % uint4NotZero)
        check(dut.uintBsAnd, 8, uint8 & uint4)
        check(dut.uintBsOr , 8, uint8 | uint4)
        check(dut.uintBsXor, 8, uint8 ^ uint4)

        check(dut.uintBsSmaller      , 1, uint8 < uint4)
        check(dut.uintBsSmallerEquals, 1, uint8 <= uint4)
        check(dut.uintBsBigger       , 1, uint8 > uint4)
        check(dut.uintBsBiggerEquals , 1, uint8 >= uint4)




        check(dut.sintBsEquals, 1, sint8 == sint4)
        check(dut.sintBsNotEquals, 1, sint8 != sint4)

        checkSigned(dut.sintBsAdd, 8, sint8 + sint4)
        checkSigned(dut.sintBsSub, 8, sint8 - sint4)
        checkSigned(dut.sintBsMul, 12, sint8 * sint4)
        if sint4NotZero != 0 and sint8 > 0 and sint4NotZero > 0:
            checkSigned(dut.sintBsDiv, 8,  int(sint8// sint4NotZero))
            checkSigned(dut.sintBsRem, 8,  int(sint8%  sint4NotZero))

        checkSigned(dut.sintBsAnd, 8, sint8 & sint4)
        checkSigned(dut.sintBsOr, 8, sint8 | sint4)
        checkSigned(dut.sintBsXor, 8, sint8 ^ sint4)

        check(dut.sintBsSmaller, 1, sint8 < sint4)
        check(dut.sintBsSmallerEquals, 1, sint8 <= sint4)
        check(dut.sintBsBigger, 1, sint8 > sint4)
        check(dut.sintBsBiggerEquals, 1, sint8 >= sint4)




        check(dut.bitsBsEquals, 1, bits8 == bits4)
        check(dut.bitsBsNotEquals, 1, bits8 != bits4)

        check(dut.bitsBsAnd, 8, bits8 & bits4)
        check(dut.bitsBsOr, 8, bits8 | bits4)
        check(dut.bitsBsXor, 8, bits8 ^ bits4)

        check(dut.bitsCat, 12, bits8 * 16 +  bits4)


        check(dut.boolEquals, 1, boolA == boolB)
        check(dut.boolNotEquals, 1, boolA != boolB)

        check(dut.boolAnd, 1, boolA & boolB)
        check(dut.boolOr, 1, boolA | boolB)
        check(dut.boolXor, 1, boolA ^ boolB)

        check(dut.uintAsBits,8,int(dut.uint8))
        check(dut.uintAsSint,8,int(dut.uint8))
        check(dut.sintAsBits,8,int(dut.sint8))
        check(dut.sintAsUint,8,int(dut.sint8))
        check(dut.bitsAsUint,8,int(dut.bits8))
        check(dut.bitsAsSint,8,int(dut.bits8))
        check(dut.boolAsBits,1,int(dut.boolA))
        check(dut.boolAsUInt,1,int(dut.boolA))
        check(dut.boolAsSInt,1,int(dut.boolA))

        check(dut.bitsResizeBigger  ,16,bits8)
        check(dut.bitsResizeSmaller ,4 ,bits8)
        check(dut.uintResizeBigger  ,16,uint8)
        check(dut.uintResizeSmaller ,4 ,uint8)
        check(dut.sintResizeBigger  ,16,sint8)
        check(dut.sintResizeSmaller ,4 ,sint8)

        bits8StateSeq = (bits8 & 3)
        bits8StateHO = (bits8 & 7)

        if bits8StateSeq <= 2:
            check(dut.stateNativeBits,2,bits8)
            check(dut.stateBinarySequancialBits, 2, bits8)
            check(dut.stateNativeIsA,1,bits8StateSeq == 0)
            check(dut.stateNativeIsB, 1, bits8StateSeq == 1)
            check(dut.stateNativeIsC, 1, bits8StateSeq == 2)
            check(dut.stateNativeIsNotA,1,bits8StateSeq != 0)
            check(dut.stateNativeIsNotB, 1, bits8StateSeq != 1)
            check(dut.stateNativeIsNotC, 1, bits8StateSeq != 2)
            check(dut.stateBinarySequancialIsA,1,bits8StateSeq == 0)
            check(dut.stateBinarySequancialIsB, 1, bits8StateSeq == 1)
            check(dut.stateBinarySequancialIsC, 1, bits8StateSeq == 2)
            check(dut.stateBinarySequancialIsNotA,1,bits8StateSeq != 0)
            check(dut.stateBinarySequancialIsNotB, 1, bits8StateSeq != 1)
            check(dut.stateBinarySequancialIsNotC, 1, bits8StateSeq != 2)

        if bits8StateHO == 1 or bits8StateHO == 2 or bits8StateHO == 4:
            check(dut.stateBinaryOneHotBits, 3, bits8)
            check(dut.stateBinaryOneHotIsA,1,bits8StateHO == 1)
            check(dut.stateBinaryOneHotIsB, 1, bits8StateHO == 2)
            check(dut.stateBinaryOneHotIsC, 1, bits8StateHO == 4)
            check(dut.stateBinaryOneHotIsNotA,1,bits8StateHO != 1)
            check(dut.stateBinaryOneHotIsNotB, 1, bits8StateHO != 2)
            check(dut.stateBinaryOneHotIsNotC, 1, bits8StateHO != 4)

        check(dut.bitsAggregateFixed, 8, (3 << 5) | ((bits8 & 3) << 3) |(boolA << 2) | (boolA << 1) | (1 << 0))
        check(dut.uintAggregateFixed, 8, (3 << 5) | ((uint8 & 3) << 3) |(boolA << 2) | (boolA << 1) | (1 << 0))
        check(dut.sintAggregateFixed, 8, (3 << 5) | ((sint8 & 3) << 3) |(boolA << 2) | (boolA << 1) | (1 << 0))


        check(dut.bitsAggregateUnfixedWidthFixedDefault, 8, (3 << 5) | ((bits8 & 3) << 3) |(boolA << 2) | (3 << 1) | (1 << 0))
        check(dut.uintAggregateUnfixedWidthFixedDefault, 8, (3 << 5) | ((uint8 & 3) << 3) |(boolA << 2) | (3 << 1) | (1 << 0))
        check(dut.sintAggregateUnfixedWidthFixedDefault, 8, (3 << 5) | ((sint8 & 3) << 3) |(boolA << 2) | (3 << 1) | (1 << 0))


        check(dut.bitsAggregateUnfixedWidthUnfixedDefault, 8, (3 << 5) | ((bits8 & 3) << 3) |(boolA << 2) | (boolA << 1) | (1 << 0))
        check(dut.uintAggregateUnfixedWidthUnfixedDefault, 8, (3 << 5) | ((uint8 & 3) << 3) |(boolA << 2) | (boolA << 1) | (1 << 0))
        check(dut.sintAggregateUnfixedWidthUnfixedDefault, 8, (3 << 5) | ((sint8 & 3) << 3) |(boolA << 2) | (boolA << 1) | (1 << 0))

        bits27 = bits32 & ((1 << 27) - 1)
        uint5  = uint8 & 0x1F
        uint5mod27 = uint5 % 27
        rotateLeftValue = (bits27 << uint5mod27) | (bits27 >> (27-uint5mod27))
        rotateRightValue = (bits27 >> uint5mod27) | (bits27 << (27-uint5mod27))
        check(dut.bitsRotateLeftUInt,27,rotateLeftValue)
        check(dut.uintRotateLeftUInt, 27, rotateLeftValue)
        check(dut.sintRotateLeftUInt, 27, rotateLeftValue)
        check(dut.bitsRotateRightUInt,27,rotateRightValue)
        check(dut.uintRotateRightUInt, 27, rotateRightValue)
        check(dut.sintRotateRightUInt, 27, rotateRightValue)

    dut.log.info("Cocotb test done")
