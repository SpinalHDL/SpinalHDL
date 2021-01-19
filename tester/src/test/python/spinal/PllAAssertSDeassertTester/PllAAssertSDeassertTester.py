import cocotb
from cocotb.triggers import RisingEdge, Timer

from cocotblib.misc import randSignal, assertEquals, truncUInt, ClockDomainAsyncReset


@cocotb.coroutine
def genClkReset(dut):
    dut.clk_100Mhz <= 0
    dut.aReset <= 1
    yield Timer(1000)
    dut.aReset <= 0
    yield Timer(1000)

    while True:
        dut.clk_100Mhz <= 1
        yield Timer(1000)
        dut.clk_100Mhz <= 0
        yield Timer(1000)

@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")

    cocotb.fork(genClkReset(dut))
    yield Timer(20000)
    print("yolo" + str(dut.counter))
    assertEquals(dut.counter,3,"Mismatch")
    dut.aReset <= 1
    yield Timer(5)
    dut.aReset <= 0
    yield Timer(40000)
    print("yolo" + str(dut.counter))
    assertEquals(dut.counter,3+5,"Mismatch")
    dut.log.info("Cocotb test done")
