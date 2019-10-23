import random

import cocotb
from cocotb.triggers import Timer

from cocotblib.Apb3 import Apb3
from cocotblib.Phase import PhaseManager, Infrastructure, PHASE_WAIT_TASKS_END
from cocotblib.Scorboard import ScorboardInOrder
from cocotblib.misc import simulationSpeedPrinter, randBits, BoolRandomizer

from cocotblib.Stream import StreamDriverSlave, StreamDriverMaster, Transaction, StreamMonitor, Stream
from spinal.SdramXdr.common.Tester import Bmb, BmbMemoryTester


@cocotb.coroutine
def ClockDomainAsyncResetCustom(clk,reset):
    if reset:
        reset <= 1
    clk <= 0
    yield Timer(100000000)
    if reset:
        reset <= 0
    while True:
        clk <= 0
        yield Timer(5000)
        clk <= 1
        yield Timer(5000)

@cocotb.test()
def test1(dut):
    random.seed(0)
    from cocotblib.misc import cocotbXHack
    cocotbXHack()

    cocotb.fork(ClockDomainAsyncResetCustom(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))



    bmbs = [Bmb(dut, "io_ports_" + str(x)) for x in range(3)]
    tester = BmbMemoryTester(bmbs, 16*1024, 2, 32,dut.clk,dut.reset)

    @cocotb.coroutine
    def delay():
        yield RisingEdge(dut.clk)

    apb = Apb3(dut, "io_apb", dut.clk)
    yield apb.write(0x10, 0x30d)
    yield apb.write(0x20, 0x1060103)
    yield apb.write(0x24, 0x1)
    yield apb.write(0x28, 0x1000004)
    yield apb.write(0x10c, 0x0)
    yield apb.write(0x108, 0x400)
    yield apb.write(0x104, 0x8)
    yield apb.write(0x100, 0x0)
    yield apb.write(0x10c, 0x0)
    yield apb.write(0x108, 0x0)
    yield apb.write(0x104, 0x10)
    yield apb.write(0x100, 0x0)
    yield apb.write(0x10c, 0x0)
    yield apb.write(0x108, 0x0)
    yield apb.write(0x104, 0x10)
    yield apb.write(0x100, 0x0)
    yield apb.write(0x10c, 0x0)
    yield apb.write(0x108, 0x20)
    yield apb.write(0x104, 0x0)
    yield apb.write(0x100, 0x0)

    delay()
    tester.run = True
    while True:
        yield Timer(0x1000000)

