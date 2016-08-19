import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer, RisingEdge

from spinal.common.Axi4 import Axi4, Axi4ReadOnly, Axi4WriteOnly, Axi4Shared
from spinal.common.Phase import PhaseManager, Infrastructure, PHASE_CHECK_SCORBOARDS
from spinal.common.Stream import StreamDriverSlave, StreamDriverMaster, Transaction, StreamMonitor
from spinal.common.misc import ClockDomainAsyncReset, simulationSpeedPrinter, randBits, BoolRandomizer, assertEquals


class WriteOnlyMasterDriver:
    def __init__(self,idBase,axi,dut):
        self.dut = dut
        self.axi = axi
        self.idBase = idBase
        self.writeCmdQueue = Queue()
        self.writeDataQueue = Queue()
        self.writeCmdIdleRand = BoolRandomizer()
        self.writeDataIdleRand = BoolRandomizer()
        self.writeCounter = 0
        self.doFinish = False

    def isCompleted(self):
        if not self.doFinish:
            return False
        if not self.writeDataQueue.empty():
            return False
        if not self.writeCmdQueue.empty():
            return False
        return True

    def createInfrastructure(self):
        StreamDriverMaster(self.axi.aw, self.genWriteCmd, self.dut.clk, self.dut.reset)
        StreamDriverMaster(self.axi.w, self.genWriteData, self.dut.clk, self.dut.reset)
        StreamDriverSlave(self.axi.b, self.dut.clk, self.dut.reset)
        return self

    def genRandomWriteAddress(self):
        if random.random() < 0.1: # Random assertion of decoding error
            return 1 << 14
        return randBits(12) + random.choice([0,1,3])*0x1000

    def genWrite(self):
        idOffset = randBits(2)
        writeCmd = Transaction()
        writeCmd.addr = self.genRandomWriteAddress()
        writeCmd.hid = self.idBase + idOffset #Each master can use 4 id
        writeCmd.region = randBits(4)
        writeCmd.len = randBits(4)
        writeCmd.size = randBits(3)
        writeCmd.burst = randBits(2)
        writeCmd.lock = randBits(1)
        writeCmd.cache = randBits(4)
        writeCmd.qos = randBits(4)
        writeCmd.prot = randBits(3)
        self.writeCmdQueue.put(writeCmd)

        for i in xrange(writeCmd.len + 1):
            writeData = Transaction()
            writeData.data = writeCmd.addr + i
            writeData.strb = (writeCmd.addr + i) & 0xF
            writeData.last = 1 if i == writeCmd.len else 0
            self.writeDataQueue.put(writeData)

        self.writeCounter += 1
        self.updateDoFinish()

    def getNextWriteCmdTrans(self):
        if(self.writeCmdQueue.empty()):
            if self.doFinish:
                return None
            self.genWrite()
        return self.writeCmdQueue.get()

    def getNextWriteDataTrans(self):
        if(self.writeDataQueue.empty()):
            if self.doFinish:
                return None
            self.genWrite()
        return self.writeDataQueue.get()

    def genWriteCmd(self):
        if not self.writeCmdIdleRand.get():
            return None
        return self.getNextWriteCmdTrans()

    def genWriteData(self):
        if not self.writeDataIdleRand.get():
            return None
        return self.getNextWriteDataTrans()


    def updateDoFinish(self):
        if self.writeCounter > 100:
            self.doFinish = False


class ReadOnlyMasterDriver:
    def __init__(self,idBase,axi,dut):
        self.idBase = idBase
        self.axi = axi
        self.dut = dut
        self.readCounter = 0
        self.doFinish = False
        self.readCmdIdleRand = BoolRandomizer()

    def createInfrastructure(self):
        StreamDriverMaster(self.axi.ar, self.genReadCmd, self.dut.clk, self.dut.reset)
        StreamDriverSlave(self.axi.r, self.dut.clk, self.dut.reset)
        return self

    def isCompleted(self):
        if not self.doFinish:
            return False
        for q in self.readMonitorQueues:
            if not q.empty():
                return False
        return True

    def genRandomReadAddress(self):
        if random.random() < 0.1: # Random assertion of decoding error
            return 1 << 14
        return randBits(12) + random.choice([0,1,2])*0x1000

    def genReadCmd(self):
        if self.doFinish:
            return None
        if not self.readCmdIdleRand.get():
            return None

        idOffset = randBits(2)
        trans = Transaction()
        trans.addr = self.genRandomReadAddress()
        trans.hid = self.idBase*4 + idOffset #Each master can use 4 id
        trans.region = randBits(4)
        trans.len = randBits(4)
        trans.size = randBits(3)
        trans.burst = randBits(2)
        trans.lock = randBits(1)
        trans.cache = randBits(4)
        trans.qos = randBits(4)
        trans.prot = randBits(3)

        self.readCounter += 1
        self.updateDoFinish()
        return trans


    def updateDoFinish(self):
        if self.readCounter > 100:
            self.doFinish = False



class SharedMasterDriver(WriteOnlyMasterDriver, ReadOnlyMasterDriver):
    def __init__(self,idBase,axi,dut):
        WriteOnlyMasterDriver.__init__(self, idBase, axi, dut)
        ReadOnlyMasterDriver.__init__(self, idBase, axi, dut)
        self.readOrWriteRand = BoolRandomizer()


    def createInfrastructure(self):
        StreamDriverMaster(self.axi.arw, self.genSharedCmd, self.dut.clk, self.dut.reset)
        StreamDriverMaster(self.axi.w, self.genWriteData, self.dut.clk, self.dut.reset)
        StreamDriverSlave(self.axi.b, self.dut.clk, self.dut.reset)
        StreamDriverSlave(self.axi.r, self.dut.clk, self.dut.reset)
        return self

    def genSharedCmd(self):
        if self.readOrWriteRand.get():
            trans = self.genWriteCmd()
            if trans:
                trans.write = 1
        else:
            trans = self.genReadCmd()
            if trans:
                trans.write = 0

        return trans


    def updateDoFinish(self):
        if self.readCounter > 100 and self.writeCounter > 100:
            self.doFinish = False


    def isCompleted(self):
        return WriteOnlyMasterDriver.isCompleted(self) and ReadOnlyMasterDriver.isCompleted(self)
