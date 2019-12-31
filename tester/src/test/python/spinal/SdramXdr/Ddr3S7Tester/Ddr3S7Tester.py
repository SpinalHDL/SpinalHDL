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
def ClockDomainAsyncResetCustom(period, clk, serdesClk0, serdesClk90, reset):
    forth = period//4
    if reset:
        reset <= 1
    clk <= 0
    serdesClk0 <= 0
    serdesClk90 <= 0
    yield Timer(100000000)
    if reset:
        reset <= 0
    yield Timer(period*100)
    while True:
        clk <= 1
        serdesClk0 <= 1
        yield Timer(forth)
        serdesClk90 <= 1
        yield Timer(forth)
        serdesClk0 <= 0
        yield Timer(forth)
        serdesClk90 <= 0
        yield Timer(forth)
        serdesClk0 <= 1
        yield Timer(forth)
        serdesClk90 <= 1
        yield Timer(forth)
        serdesClk0 <= 0
        yield Timer(forth)
        serdesClk90 <= 0
        yield Timer(forth)

        clk <= 0
        serdesClk0 <= 1
        yield Timer(forth)
        serdesClk90 <= 1
        yield Timer(forth)
        serdesClk0 <= 0
        yield Timer(forth)
        serdesClk90 <= 0
        yield Timer(forth)
        serdesClk0 <= 1
        yield Timer(forth)
        serdesClk90 <= 1
        yield Timer(forth)
        serdesClk0 <= 0
        yield Timer(forth)
        serdesClk90 <= 0
        yield Timer(forth)

@cocotb.test()
def test1(dut):
    random.seed(0)
    from cocotblib.misc import cocotbXHack
    cocotbXHack()

    cocotb.fork(ClockDomainAsyncResetCustom(3300, dut.clk0, dut.serdesClk0, dut.serdesClk90, dut.rst0))
    cocotb.fork(simulationSpeedPrinter(dut.clk0))



    bmbs = [Bmb(dut, "system_io_ports_" + str(x)) for x in range(1)]
    tester = BmbMemoryTester(bmbs, 64*1024, 8, 32,dut.clk0 ,dut.rst0,True)

    @cocotb.coroutine
    def delay():
        yield RisingEdge(dut.clk0)

    ctrlApb = Apb3(dut, "system_io_ctrlApb", dut.clk0)
    phyApb = Apb3(dut, "system_io_phyApb", dut.clk0)


    @cocotb.coroutine
    def apbWrite(address, data):
        yield ctrlApb.write(address, data)

    yield apbWrite(0x0, 0x20001)
    yield apbWrite(0x4, 0x2)
    yield apbWrite(0x10, 0x491)
    yield apbWrite(0x20, 0x1170205)
    yield apbWrite(0x24, 0x2)
    yield apbWrite(0x28, 0x2030102)
    yield apbWrite(0x30, 0x5)
    yield apbWrite(0x34, 0x103)
    yield apbWrite(0x110, 0x0)
    yield apbWrite(0x110, 0x1)
    yield apbWrite(0x110, 0x3)
    yield apbWrite(0x10c, 0x2)
    yield apbWrite(0x108, 0x200)
    yield apbWrite(0x104, 0x0)
    yield apbWrite(0x100, 0x0)
    yield apbWrite(0x10c, 0x3)
    yield apbWrite(0x108, 0x0)
    yield apbWrite(0x104, 0x0)
    yield apbWrite(0x100, 0x0)
    yield apbWrite(0x10c, 0x1)
    yield apbWrite(0x108, 0x44)
    yield apbWrite(0x104, 0x0)
    yield apbWrite(0x100, 0x0)
    yield apbWrite(0x10c, 0x0)
    yield apbWrite(0x108, 0x310)
    yield apbWrite(0x104, 0x0)
    yield apbWrite(0x100, 0x0)
    yield apbWrite(0x10c, 0x0)
    yield apbWrite(0x108, 0x400)
    yield apbWrite(0x104, 0xc)
    yield apbWrite(0x100, 0x0)

    # yield ctrlApb.write(0x000, 0x00) #phase command = 0
    # yield ctrlApb.write(0x110, 0x00) #reset
    # yield ctrlApb.delay(10)
    # yield ctrlApb.write(0x110, 0x01) #!reset
    # yield ctrlApb.delay(10)
    # yield ctrlApb.write(0x110, 0x03) #cke
    # yield ctrlApb.delay(10)
    #
    #
    # @cocotb.coroutine
    # def command(cmd, bank, address):
    #     yield ctrlApb.write(0x10C, bank)
    #     yield ctrlApb.write(0x108, address)
    #     yield ctrlApb.write(0x104, cmd)
    #     yield ctrlApb.write(0x100, 0)
    #     yield ctrlApb.delay(10)
    #
    # CKE = 1 << 0
    # CSn = 1 << 1
    # RASn = 1 << 2
    # CASn = 1 << 3
    # WEn = 1 << 4
    #
    # PRE = CKE | CASn
    # REF = CKE | WEn
    # MOD = CKE
    # ZQCL = CKE | RASn | CASn
    #
    # CL = 2 # 5
    #
    #
    # yield command(MOD, 2, 0)
    # yield command(MOD, 3, 0)
    # yield command(MOD, 1, 0)
    # yield command(MOD, 0, (1 << 9) | 0x100 | ((CL & 1) << 2) | ((CL & 0xE) << 3)) #DDL reset
    # yield command(ZQCL, 0, 0x400)
    # yield ctrlApb.delay(1000)

    delay()
    tester.run = True
    while True:
        yield Timer(0x1000000)

