import random

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer, Edge, RisingEdge

def randBool():
    return bool(random.getrandbits(1))

def randBits(width):
    return random.getrandbits(width)

@cocotb.coroutine
def ClockDomainAsyncReset(clk,reset):
    if reset:
        reset <= 1
    clk <= 0
    yield Timer(1000)
    if reset:
        reset <= 0
    while True:
        clk <= 0
        yield Timer(500)
        clk <= 1
        yield Timer(500)


def getReference(dut):
    if int(dut.io_conds_0) == 1:
        io_outDefault_ref = dut.io_data_1
    else:
        io_outDefault_ref = dut.io_data_0


    if int(dut.io_conds_0) == 1:
        io_outComplex_ref = dut.io_data_0
    elif int(dut.io_conds_1) == 1:
        io_outComplex_ref = dut.io_data_1
        if int(dut.io_data_3) == int(dut.io_data_4):
            io_outComplex_ref = dut.io_data_5
        elif int(dut.io_data_3) == int(dut.io_data_6):
            io_outComplex_ref = dut.io_data_7
        elif int(dut.io_data_3) == 0x55:
            if int(dut.io_conds_2) == 1:
                io_outComplex_ref = 0xAA
            elif int(dut.io_conds_3) == 1:
                io_outComplex_ref = dut.io_data_8
        else:
            io_outComplex_ref = dut.io_data_11
    else:
        if int(dut.io_conds_4) == 1:
            io_outComplex_ref = dut.io_data_9
        else:
            io_outComplex_ref = dut.io_data_10
    return (int(io_outDefault_ref), int(io_outComplex_ref))


@cocotb.test()
def test1(dut):
    """
    Try accessing the design
    """
    dut.log.info("Cocotb test boot")
    random.seed(0)
    clockThread = cocotb.fork(ClockDomainAsyncReset(dut.clk, None))

    for i in range(0,1000):
        dut.io_conds_0 <= randBool()
        dut.io_conds_1 <= randBool()
        dut.io_conds_2 <= randBool()
        dut.io_conds_3 <= randBool()
        dut.io_conds_4 <= randBool()
        dut.io_conds_5 <= randBool()
        dut.io_conds_6 <= randBool()
        dut.io_conds_7 <= randBool()
        dut.io_data_0  <= randBits(8)
        dut.io_data_1  <= randBits(8)
        dut.io_data_2  <= randBits(8)
        dut.io_data_3  <= randBits(8)
        dut.io_data_4  <= randBits(8)
        dut.io_data_5  <= randBits(8)
        dut.io_data_6  <= randBits(8)
        dut.io_data_7  <= randBits(8)
        dut.io_data_8  <= randBits(8)
        dut.io_data_9  <= randBits(8)
        dut.io_data_10 <= randBits(8)
        dut.io_data_11 <= randBits(8)
        yield RisingEdge(dut.clk)
        (io_outDefault_ref, io_outComplex_ref) = getReference(dut)
        if io_outDefault_ref != int(dut.io_outDefault):
            raise TestFailure(
                "io_outDefault %d % d" % (io_outDefault_ref,int(dut.io_outDefault)))
        if io_outComplex_ref != int(dut.io_outComplex):
            raise TestFailure(
                "io_outComplex")

        yield Timer(1)
        if io_outComplex_ref != int(dut.io_outRegComplex):
            raise TestFailure(
                "io_outRegComplex")

    dut.log.info("Cocotb test done")
