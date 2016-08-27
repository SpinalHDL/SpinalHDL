import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer, RisingEdge

from spinal.Axi4CrossbarTester2.MasterDriver import WriteOnlyMasterDriver, ReadOnlyMasterDriver, SharedMasterDriver
from spinal.Axi4CrossbarTester2.MasterMonitor import ReadOnlyMasterMonitor, WriteOnlyMasterMonitor, SharedMasterMonitor
from spinal.Axi4CrossbarTester2.SlaveMonitor import WriteDataMonitor, SharedDataMonitor
from spinal.Axi4CrossbarTester2.SlavesDriver import ReadOnlySlaveDriver, WriteOnlySlaveDriver, SharedSlaveDriver
from spinal.common.Axi4 import Axi4, Axi4ReadOnly, Axi4WriteOnly, Axi4Shared
from spinal.common.Phase import PhaseManager, Infrastructure, PHASE_CHECK_SCORBOARDS
from spinal.common.Stream import StreamDriverSlave, StreamDriverMaster, Transaction, StreamMonitor, Stream, StreamFifoTester
from spinal.common.misc import ClockDomainAsyncReset, simulationSpeedPrinter, randBits, BoolRandomizer, assertEquals



@cocotb.test()
def test1(dut):
    random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))


    phaseManager = PhaseManager()
    phaseManager.setWaitTasksEndTime(1000*200)

    yield phaseManager.run()

