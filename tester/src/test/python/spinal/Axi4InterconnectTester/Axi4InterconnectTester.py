import random

import cocotb
from cocotb.triggers import Timer

from spinal.common.Axi4 import Axi4
from spinal.common.Stream import StreamDriverSlave, StreamDriverMaster, StreamTransaction, StreamMonitor
from spinal.common.misc import ClockDomainAsyncReset, simulationSpeedPrinter, randBits, BoolRandomizer


class MasterHandle:
    def __init__(self,id):
        self.id = id

    def genRandomAddress(self):
        while True:
            value = randBits(12)
            if (value >> 10) != self.id  and ((value >> 8) & 0x3) == self.id:
                return value

    def genTransaction(self):
        trans = StreamTransaction()
        trans.addr = self.genRandomAddress()
        trans.id = self.id
        trans.region = randBits(4)
        trans.len = randBits(8)
        trans.size = randBits(3)
        trans.burst = randBits(2)
        trans.lock = randBits(1)
        trans.cache = randBits(4)
        trans.qos = randBits(4)
        trans.prot = randBits(3)
        return trans

class SlaveHandle:
    def __init__(self,id):
        self.id = id
        self.tasks = []
        self.genRandomizer = BoolRandomizer()

    def genTransaction(self):
        if len(self.tasks) != 0:
            if self.genRandomizer.get():
                task = self.tasks[random.randint(0, len(self.tasks) - 1)]
                trans = StreamTransaction()
                trans.data = task.addr + task.progress
                trans.resp = 0
                trans.id = task.id
                task.progress += 1
                if task.progress == task.len + 1:
                    trans.last = 1
                    self.tasks.remove(task)
                else:
                    trans.last = 0
                return trans


    def onTransaction(self, trans):
        trans.progress = 0
        self.tasks.append(trans)




@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))

    axiMasters = [Axi4(dut, "axiMasters_" + str(i)) for i in range(3)]
    axiSlaves = [Axi4(dut, "axiSlaves_" + str(i)) for i in range(4)]
    for axiMaster in axiMasters:
        idx = axiMasters.index(axiMaster)
        masterHandle = MasterHandle(idx)
        StreamDriverMaster(axiMaster.ar,masterHandle.genTransaction,dut.clk,dut.reset)
        StreamDriverSlave(axiMaster.r, dut.clk, dut.reset)

    for axiSlave in axiSlaves:
        axiSlave.r.payload.id <= 0

        slaveHandle = SlaveHandle(idx)
        StreamDriverSlave(axiSlave.ar,dut.clk,dut.reset)
        StreamDriverMaster(axiSlave.r,slaveHandle.genTransaction,dut.clk,dut.reset)
        StreamMonitor(axiSlave.ar,slaveHandle.onTransaction,dut.clk,dut.reset)

    # while True:
    #     yield RisingEdge(dut.clk)
    #     done = True
    #     for checker in checkers:
    #         if checker.counter < 1000:
    #             done = False
    #     if done:
    #         break

    yield Timer(1000*100)

    dut.log.info("Cocotb test done")
