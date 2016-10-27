import random

import cocotb
from cocotb.result import TestFailure
from cocotblib.Axi4 import Axi4, Axi4ReadOnly, Axi4WriteOnly, Axi4Shared
from cocotblib.Phase import PhaseManager, Infrastructure, PHASE_CHECK_SCORBOARDS
from cocotblib.misc import ClockDomainAsyncReset, simulationSpeedPrinter

from spinal.Axi4CrossbarTester2.MasterDriver import WriteOnlyMasterDriver, ReadOnlyMasterDriver, SharedMasterDriver
from spinal.Axi4CrossbarTester2.MasterMonitor import ReadOnlyMasterMonitor, WriteOnlyMasterMonitor, SharedMasterMonitor
from spinal.Axi4CrossbarTester2.SlaveMonitor import WriteDataMonitor, SharedDataMonitor
from spinal.Axi4CrossbarTester2.SlavesDriver import ReadOnlySlaveDriver, WriteOnlySlaveDriver, SharedSlaveDriver


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
    from cocotblib.misc import cocotbXHack
    cocotbXHack()
    random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))


    phaseManager = PhaseManager()
    phaseManager.setWaitTasksEndTime(1000*2000)

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
        WriteOnlyMasterDriver("Axi4WriteMasterDriver" + str(idx),phaseManager,0 + idx * 4, axiMaster, dut).createInfrastructure()
        ReadOnlyMasterDriver("Axi4ReadMasterDriver" + str(idx),phaseManager,0 + idx * 4, axiMaster, dut).createInfrastructure()
        ReadOnlyMasterMonitor("Axi4ReadMasterMonitor" + str(idx), phaseManager, axiMaster, dut).createInfrastructure()
        WriteOnlyMasterMonitor("Axi4WriteMasterMonitor" + str(idx), phaseManager, axiMaster, dut).createInfrastructure()

    for idx,axiMaster in enumerate(axiReadOnlyMasters):
        ReadOnlyMasterDriver("ReadOnlyMasterDriver" + str(idx),phaseManager,4 + idx * 4, axiMaster, dut).createInfrastructure()
        ReadOnlyMasterMonitor("ReadOnlyMasterMonitor" + str(idx),phaseManager,axiMaster,dut).createInfrastructure()

    for idx,axiMaster in enumerate(axiWriteOnlyMasters):
        WriteOnlyMasterDriver("WriteOnlyMasterDriver" + str(idx),phaseManager,8 + idx * 4, axiMaster, dut).createInfrastructure()
        WriteOnlyMasterMonitor("WriteOnlyMasterMonitor" + str(idx), phaseManager, axiMaster, dut).createInfrastructure()

    for idx,axiMaster in enumerate(axiSharedMasters):
        SharedMasterDriver("SharedMasterDriver" + str(idx),phaseManager,12 + idx * 4, axiMaster, dut).createInfrastructure()
        SharedMasterMonitor("SharedMasterMonitor" + str(idx), phaseManager, axiMaster, dut).createInfrastructure()



    for idx,axiSlave in enumerate(axiSlaves):
        WriteOnlySlaveDriver(axiSlave,0x0000 + idx*0x0800,0x0800, dut).createInfrastructure()
        ReadOnlySlaveDriver(axiSlave,0x0000 + idx*0x0800,0x0800, dut).createInfrastructure()
        WriteDataMonitor("Axi4DataSlaveMonitor" + str(idx), phaseManager, axiSlave, dut).createInfrastructure()

    for idx,axiSlave in enumerate(axiReadOnlySlaves):
        ReadOnlySlaveDriver(axiSlave,0x2000 + idx*0x0800,0x0800, dut).createInfrastructure()

    for idx,axiSlave in enumerate(axiWriteOnlySlaves):
        WriteOnlySlaveDriver(axiSlave,0x3000 + idx*0x0800,0x0800, dut).createInfrastructure()
        WriteDataMonitor("WriteOnlySlaveMonitor" + str(idx),phaseManager,axiSlave,dut).createInfrastructure()

    for idx,axiSlave in enumerate(axiSharedSlaves):
        SharedSlaveDriver(axiSlave,0x1000 + idx*0x0800,0x0800, dut).createInfrastructure()
        SharedDataMonitor("SharedSlaveMonitor" + str(idx), phaseManager, axiSlave, dut).createInfrastructure()

    # cocotb.log.error("miaou")
    # Run until completion
    yield phaseManager.run()
    # yield Timer(1000*6000)

    dut.log.info("Cocotb test done")
