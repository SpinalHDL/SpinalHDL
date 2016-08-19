import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer, RisingEdge

from spinal.Axi4InterconnectTester2.MasterDriver import WriteOnlyMasterDriver, ReadOnlyMasterDriver, SharedMasterDriver
from spinal.Axi4InterconnectTester2.SlavesDriver import ReadOnlySlaveDriver, WriteOnlySlaveDriver, SharedSlaveDriver
from spinal.common.Axi4 import Axi4, Axi4ReadOnly, Axi4WriteOnly, Axi4Shared
from spinal.common.Phase import PhaseManager, Infrastructure, PHASE_CHECK_SCORBOARDS
from spinal.common.Stream import StreamDriverSlave, StreamDriverMaster, Transaction, StreamMonitor
from spinal.common.misc import ClockDomainAsyncReset, simulationSpeedPrinter, randBits, BoolRandomizer, assertEquals





@cocotb.coroutine
def aaa():
    raise TestFailure("AAAA")


@cocotb.coroutine
def bbb():
    raise TestFailure("BBBB")

class ErrorInfra(Infrastructure):
    def __init__(self,name,parent,message):
        Infrastructure.__init__(self,name,parent)
        self.message = message

    def startPhase(self, phase):
        if phase == PHASE_CHECK_SCORBOARDS:
            # raise TestFailure(self.message)
            cocotb.log.error(self.message)


    def endPhase(self, phase):
        if phase == PHASE_CHECK_SCORBOARDS:
            raise TestFailure("hohhoho")
            # cocotb.log.error(self.message)


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))


    phaseManager = PhaseManager()
    axiMasters = [Axi4(dut, "axiMasters_" + str(i)) for i in range(2)]
    axiSlaves = [Axi4(dut, "axiSlaves_" + str(i)) for i in range(2)]

    axiReadOnlyMasters = [Axi4ReadOnly(dut, "axiReadOnlyMasters_" + str(i)) for i in range(2)]
    axiReadOnlySlaves = [Axi4ReadOnly(dut,  "axiReadOnlySlaves_" + str(i)) for i in range(2)]

    axiWriteOnlyMasters = [Axi4WriteOnly(dut, "axiWriteOnlyMasters_" + str(i)) for i in range(2)]
    axiWriteOnlySlaves = [Axi4WriteOnly(dut,  "axiWriteOnlySlaves_" + str(i)) for i in range(2)]

    axiSharedMasters = [Axi4Shared(dut, "axiSharedMasters_" + str(i)) for i in range(2)]
    axiSharedSlaves = [Axi4Shared(dut,  "axiSharedSlaves_" + str(i)) for i in range(2)]



    # Instanciate master sides
    for idx,axiMaster in enumerate(axiMasters):
        writeMaster = WriteOnlyMasterDriver(0 + idx * 4, axiMaster, dut).createInfrastructure()
        readMaster = ReadOnlyMasterDriver(0 + idx * 4, axiMaster, dut).createInfrastructure()

    for idx,axiMaster in enumerate(axiReadOnlyMasters):
        readMaster = ReadOnlyMasterDriver(4 + idx * 4, axiMaster, dut).createInfrastructure()

    for idx,axiMaster in enumerate(axiWriteOnlyMasters):
        writeMaster = WriteOnlyMasterDriver(8 + idx * 4, axiMaster, dut).createInfrastructure()

    for idx,axiMaster in enumerate(axiSharedMasters):
        master = SharedMasterDriver(12 + idx * 4, axiMaster, dut).createInfrastructure()




    for idx,axiSlave in enumerate(axiSlaves):
        writeSlave = WriteOnlySlaveDriver(axiSlave, dut).createInfrastructure()
        readSlave = ReadOnlySlaveDriver(axiSlave, dut).createInfrastructure()

    for idx,axiSlave in enumerate(axiReadOnlySlaves):
        readSlave = ReadOnlySlaveDriver(axiSlave, dut).createInfrastructure()

    for idx,axiSlave in enumerate(axiWriteOnlySlaves):
        writeSlave = WriteOnlySlaveDriver(axiSlave, dut).createInfrastructure()

    for idx,axiSlave in enumerate(axiSharedSlaves):
        sharedSlave = SharedSlaveDriver(axiSlave, dut).createInfrastructure()


    # cocotb.log.error("miaou")
    # Run until completion
    yield phaseManager.run()
    yield Timer(1000*6000)

    dut.log.info("Cocotb test done")
