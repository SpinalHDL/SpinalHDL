import random

import cocotb
from cocotblib.Phase import PhaseManager
from cocotblib.misc import ClockDomainAsyncReset, simulationSpeedPrinter

from cocotblib.Axi4 import Axi4Shared, Axi4SharedMemoryChecker


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    from cocotblib.misc import cocotbXHack
    cocotbXHack()
    random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))

    phaseManager = PhaseManager()
    phaseManager.setWaitTasksEndTime(1000*2000)

    checker = Axi4SharedMemoryChecker("checker",phaseManager,Axi4Shared(dut, "io_axi"),12,dut.clk,dut.reset)
    checker.idWidth = 2
    checker.nonZeroReadRspCounterTarget = 2000

    yield phaseManager.run()

    dut.log.info("Cocotb test done")
