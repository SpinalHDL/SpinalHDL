import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, FallingEdge
from cocotb.result import TestFailure
from cocotb import binary

from random import *
import struct

def floatToBits(f):
    s = struct.pack('>f', f)
    return struct.unpack('>L',s)[0]

def bitsToFloat(f):
    s = struct.pack('>L', f)
    return struct.unpack('>f',s)[0]

def checkRecodeDecode(dut):
    if int(dut.io_inp_sign) != int(dut.io_dec_sign):
        raise TestFailure ("Sign badly recoded or decoded")
    if int(dut.io_inp_exponent) != int(dut.io_dec_exponent):
        raise TestFailure ("Exponent badly recoded or decoded")
    if int(dut.io_inp_mantissa) != int(dut.io_dec_mantissa):
        raise TestFailure ("Exponent badly recoded or decoded")


@cocotb.coroutine
def checkZero(dut):
    dut.io_inp_exponent <= 0
    dut.io_inp_mantissa <= 0
    yield Timer(1)
    if (int(dut.io_recoded_exp) & 0x1C0) != 0:
        raise TestFailure ("Zero badly recoded")
    checkRecodeDecode(dut)

@cocotb.coroutine
def checkQNaN(dut):
    dut.io_inp_exponent <= 0xFF
    dut.io_inp_mantissa <= 0x7FFFFF
    yield Timer(1)
    if (int(dut.io_recoded_exp) & 0x1C0) != 0x1C0:
        raise TestFailure ("QNaN badly recoded")
    if int(dut.io_recoded_mantissa) != int(dut.io_inp_mantissa):
        raise TestFailure ("QNaN mantissa badly recoded")
    if int(dut.io_qnan) != 1:
        raise TestFailure ("QNaN flag not raised")
    checkRecodeDecode(dut)

@cocotb.coroutine
def checkSNaN(dut):
    dut.io_inp_exponent <= 0xFF
    dut.io_inp_mantissa <= 0x3FFFFF
    yield Timer(1)
    if (int(dut.io_recoded_exp) & 0x1C0) != 0x1C0:
        raise TestFailure ("SNaN badly recoded")
    if int(dut.io_recoded_mantissa) != int(dut.io_inp_mantissa):
        raise TestFailure ("SNaN mantissa badly recoded")
    if int(dut.io_snan) != 1:
        raise TestFailure ("SNaN flag not raised")
    checkRecodeDecode(dut)

@cocotb.coroutine
def checkInf(dut):
    dut.io_inp_exponent <= 0xFF
    dut.io_inp_mantissa <= 0x0
    yield Timer(1)
    if (int(dut.io_recoded_exp) & 0x1C0) != 0x180:
        raise TestFailure ("Infinity badly recoded")
    if int(dut.io_recoded_mantissa) != 0:
        raise TestFailure ("Infinity mantissa badly recoded")
    if int(dut.io_infinite) != 1:
        raise TestFailure ("Infinity flag not raised")
    checkRecodeDecode(dut)

@cocotb.coroutine
def checkNormalized(dut):
    pass

@cocotb.coroutine
def checkDenormalized(dut):
    pass

@cocotb.coroutine
def checkOther(dut):
    dut.io_inp_exponent <= 0
    dut.io_inp_mantissa <= 0x3
    yield Timer(1)


@cocotb.coroutine
def checkUIntToFloat(dut):
    for i in range(100):
        dut.io_in_uint <= randint(0, 1 << 24 - 1)
        yield Timer(1)
        if int(dut.io_in_uint) != bitsToFloat(int(dut.io_out_uint_bits)):
            raise TestFailure ("UInt to float conversion failed with values "+str(dut.io_in_uint)+" "+str(bitsToFloat(dut.io_out_uint_bits)))
        if bitsToFloat(int(dut.io_out_uint_bits)) != int(dut.io_out_to_UInt):
            raise TestFailure ("float to UInt conversion failed with values "+str(dut.io_out_to_UInt)+" "+str(bitsToFloat(dut.io_out_uint_bits)))


@cocotb.coroutine
def checkSIntToFloat(dut):
    for i in range(100):
        inputValue = randint(0, 1 << 24 - 1)
        dut.io_in_sint <= inputValue
        yield Timer(1)
        if inputValue != bitsToFloat(int(dut.io_out_sint_bits)):
            raise TestFailure ("SInt to float conversion failed with positive values "+str(dut.io_in_sint)+" "+str(bitsToFloat(dut.io_out_sint_bits)))
        if inputValue != int(dut.io_out_to_SInt):
            raise TestFailure ("float to SInt conversion failed with positive values "+str(dut.io_out_to_SInt)+" "+str(bitsToFloat(dut.io_out_uint_bits)))

    for i in range(100):
        inputValue = -randint(0, 1 << 24 - 1)
        dut.io_in_sint <= inputValue
        yield Timer(1)
        if inputValue != bitsToFloat(int(dut.io_out_sint_bits)):
            raise TestFailure ("SInt to float conversion failed with negative values "+str(dut.io_in_sint)+" "+str(bitsToFloat(dut.io_out_sint_bits)))
        #if inputValue != dut.io_out_to_SInt:
        #    readSInt = dut.io_out_to_SInt
        #    raise TestFailure ("float to SInt conversion failed with negative values "+str(readSInt)+" "+str(inputValue))

@cocotb.coroutine
def checkUIntToFloatZero(dut):
    dut.io_in_uint <= 0
    yield Timer(1)
    if 0 != bitsToFloat(int(dut.io_out_uint_bits)):
        raise TestFailure ("UInt to float conversion failed for Zero")
    if 0 != bitsToFloat(int(dut.io_out_to_UInt)):
        raise TestFailure ("float to UInt conversion failed for Zero")


@cocotb.coroutine
def checkSIntToFloatZero(dut):
    dut.io_in_sint <= 0
    yield Timer(1)
    if 0 != bitsToFloat(int(dut.io_out_sint_bits)):
        raise TestFailure ("SInt to float conversion failed for Zero")
    if 0 != bitsToFloat(int(dut.io_out_to_SInt)):
        raise TestFailure ("SInt to float conversion failed for Zero")


@cocotb.test()
def FloatTest(dut):
    for sign in range(2):
        dut.io_inp_sign <= sign
        yield checkZero(dut)
        yield checkQNaN(dut)
        yield checkSNaN(dut)
        yield checkInf(dut)
        yield checkOther(dut)
    yield checkUIntToFloatZero(dut)
    yield checkUIntToFloat(dut)
    yield checkSIntToFloatZero(dut)
    yield checkSIntToFloat(dut)
