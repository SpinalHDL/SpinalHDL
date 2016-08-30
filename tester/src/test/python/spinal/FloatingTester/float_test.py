import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, FallingEdge
from cocotb.result import TestFailure

from random import *

def checkRecodeDecode(dut):
    if dut.io_inp_sign != dut.io_dec_sign:
        raise TestFailure ("Sign badly recoded or decoded")
    if dut.io_inp_exponent != dut.io_dec_exponent:
        raise TestFailure ("Exponent badly recoded or decoded")
    if dut.io_inp_mantissa != dut.io_dec_mantissa:
        raise TestFailure ("Exponent badly recoded or decoded")


@cocotb.coroutine
def checkZero(dut):
    dut.io_inp_exponent = 0
    dut.io_inp_mantissa = 0
    yield Timer(1)
    print("%X" % dut.io_recoded_exp)
    if (int(dut.io_recoded_exp) & 0x1C0) != 0:
        raise TestFailure ("Zero badly recoded")
    checkRecodeDecode(dut)

@cocotb.coroutine
def checkQNaN(dut):
    dut.io_inp_exponent = 0xFF
    dut.io_inp_mantissa = 0x7FFFFF
    yield Timer(1)
    if (int(dut.io_recoded_exp) & 0x1C0) != 0x1C0:
        raise TestFailure ("QNaN badly recoded")
    if dut.io_recoded_mantissa != dut.io_inp_mantissa:
        raise TestFailure ("QNaN mantissa badly recoded")
    if dut.io_qnan != 1:
        raise TestFailure ("QNaN flag not raised")
    checkRecodeDecode(dut)

@cocotb.coroutine
def checkSNaN(dut):
    dut.io_inp_exponent = 0xFF
    dut.io_inp_mantissa = 0x3FFFFF
    yield Timer(1)
    if (int(dut.io_recoded_exp) & 0x1C0) != 0x1C0:
        raise TestFailure ("SNaN badly recoded")
    if dut.io_recoded_mantissa != dut.io_inp_mantissa:
        raise TestFailure ("SNaN mantissa badly recoded")
    if dut.io_snan != 1:
        raise TestFailure ("SNaN flag not raised")
    checkRecodeDecode(dut)

@cocotb.coroutine
def checkInf(dut):
    dut.io_inp_exponent = 0xFF
    dut.io_inp_mantissa = 0x0
    yield Timer(1)
    if (int(dut.io_recoded_exp) & 0x1C0) != 0x180:
        raise TestFailure ("Infinity badly recoded")
    if dut.io_recoded_mantissa != 0:
        raise TestFailure ("Infinity mantissa badly recoded")
    if dut.io_infinite != 1:
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
    dut.io_inp_exponent = 0
    dut.io_inp_mantissa = 0x3
    yield Timer(1)

@cocotb.test()
def FloatTest(dut):
    for sign in range(2):
        dut.io_inp_sign = sign
        yield checkZero(dut)
        yield checkQNaN(dut)
        yield checkSNaN(dut)
        yield checkInf(dut)
        yield checkOther(dut)
