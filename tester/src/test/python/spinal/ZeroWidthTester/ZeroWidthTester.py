import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer

from cocotblib.misc import randSignal, truncUInt, sint, truncSInt


def check(signal,bitCount,value):
    if len(signal) != bitCount or int(signal) != truncUInt(value, bitCount):
        raise TestFailure("FAIL %s    %d != %d" % (signal._path, int(signal), truncUInt(value, bitCount)))

def checkSigned(signal,bitCount,value):
    if len(signal) != bitCount or sint(signal) != truncSInt(value,bitCount):
        raise TestFailure("FAIL %s    %d != %d" % (signal._path, sint(signal), truncSInt(value,bitCount)))

@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")

    # itemDir = dut.uint4.__dict__
    #
    # for i in itemDir:
    #     print '{0}  :  {1}'.format(i, itemDir[i])

    for i in range(0,200):
        randSignal(dut.uint8)
        randSignal(dut.sint8)
        randSignal(dut.bits8)
        yield Timer(1000)
        uint8 = int(dut.uint8)
        sint8 = sint(dut.sint8)
        bits8 = int(dut.bits8)



        check(dut.bitsShiftLeftInt, 4, 0)
        check(dut.uintShiftLeftInt, 4, 0)
        check(dut.sintShiftLeftInt, 4, 0)




        check(dut.uint08ShiftLeftUint  , 255, 0 << uint8)
        check(dut.sint08ShiftLeftUint  , 255, 0 << uint8)
        check(dut.bits08ShiftLeftUint  , 255, 0 << uint8)


        check(dut.uint08Equals   , 1, 0 == uint8)
        check(dut.uint08NotEquals, 1, 0 != uint8)
        check(dut.uint08Add, 8, 0 + uint8)
        check(dut.uint08Sub, 8, 0 - uint8)
        check(dut.uint08Mul, 8, 0 * uint8)
        check(dut.uint08And, 8, 0 & uint8)
        check(dut.uint08Or , 8, 0 | uint8)
        check(dut.uint08Xor, 8, 0 ^ uint8)
        check(dut.uint08Smaller      , 1, 0 < uint8)
        check(dut.uint08SmallerEquals, 1, 0 <= uint8)
        check(dut.uint08Bigger       , 1, 0 > uint8)
        check(dut.uint08BiggerEquals , 1, 0 >= uint8)


        check(dut.sint08Equals   , 1, 0 == sint8)
        check(dut.sint08NotEquals, 1, 0 != sint8)
        check(dut.sint08Add, 8, 0 + sint8)
        check(dut.sint08Sub, 8, 0 - sint8)
        check(dut.sint08Mul, 8, 0 * sint8)
        check(dut.sint08And, 8, 0 & sint8)
        check(dut.sint08Or , 8, 0 | sint8)
        check(dut.sint08Xor, 8, 0 ^ sint8)
        check(dut.sint08Smaller      , 1, 0 < sint8)
        check(dut.sint08SmallerEquals, 1, 0 <= sint8)
        check(dut.sint08Bigger       , 1, 0 > sint8)
        check(dut.sint08BiggerEquals , 1, 0 >= sint8)

        check(dut.bits08Equals, 1, 0 == bits8)
        check(dut.bits08NotEquals, 1, 0 != bits8)
        check(dut.bits08And, 8, 0 & bits8)
        check(dut.bits08Or, 8, 0 | bits8)
        check(dut.bits08Xor, 8, 0 ^ bits8)
        
        
        
        
        

        check(dut.uint80ShiftLeftUint  , 8, uint8 << 0)
        check(dut.sint80ShiftLeftUint  , 8, sint8 << 0)
        check(dut.bits80ShiftLeftUint  , 8, bits8 << 0)


        check(dut.uint80Equals   , 1, uint8 == 0)
        check(dut.uint80NotEquals, 1, uint8 != 0)
        check(dut.uint80Add, 8, uint8 + 0)
        check(dut.uint80Sub, 8, uint8 - 0)
        check(dut.uint80Mul, 8, uint8 * 0)
        check(dut.uint80And, 8, uint8 & 0)
        check(dut.uint80Or , 8, uint8 | 0)
        check(dut.uint80Xor, 8, uint8 ^ 0)
        check(dut.uint80Smaller      , 1, uint8 < 0)
        check(dut.uint80SmallerEquals, 1, uint8 <= 0)
        check(dut.uint80Bigger       , 1, uint8 > 0)
        check(dut.uint80BiggerEquals , 1, uint8 >= 0)


        check(dut.sint80Equals   , 1, sint8 == 0)
        check(dut.sint80NotEquals, 1, sint8 != 0)
        check(dut.sint80Add, 8, sint8 + 0)
        check(dut.sint80Sub, 8, sint8 - 0)
        check(dut.sint80Mul, 8, sint8 * 0)
        check(dut.sint80And, 8, sint8 & 0)
        check(dut.sint80Or , 8, sint8 | 0)
        check(dut.sint80Xor, 8, sint8 ^ 0)
        check(dut.sint80Smaller      , 1, sint8 < 0)
        check(dut.sint80SmallerEquals, 1, sint8 <= 0)
        check(dut.sint80Bigger       , 1, sint8 > 0)
        check(dut.sint80BiggerEquals , 1, sint8 >= 0)

        check(dut.bits80Equals, 1, bits8 == 0)
        check(dut.bits80NotEquals, 1, bits8 != 0)
        check(dut.bits80And, 8, bits8 & 0)
        check(dut.bits80Or, 8, bits8 | 0)
        check(dut.bits80Xor, 8, bits8 ^ 0)

        check(dut.bitsResizeBigger,16,0)
        check(dut.uintResizeBigger,16,0)
        check(dut.sintResizeBigger,16,0)

        check(dut.bits08Cat, 8, bits8)
        check(dut.bits80Cat, 8, bits8)

    dut.log.info("Cocotb test done")
