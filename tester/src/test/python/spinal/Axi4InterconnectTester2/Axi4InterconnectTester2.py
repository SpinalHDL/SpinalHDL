import random
from Queue import Queue

import cocotb
from cocotb.triggers import Timer, RisingEdge

from spinal.common.Axi4 import Axi4, Axi4ReadOnly, Axi4WriteOnly, Axi4Shared
from spinal.common.Stream import StreamDriverSlave, StreamDriverMaster, Transaction, StreamMonitor
from spinal.common.misc import ClockDomainAsyncReset, simulationSpeedPrinter, randBits, BoolRandomizer, assertEquals



class WriteOnlyMaster:
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
            writeData.data = randBits(32)
            writeData.strb = randBits(4)
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
            self.doFinish = True


class ReadOnlyMaster:
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
            self.doFinish = True



class SharedMaster(WriteOnlyMaster,ReadOnlyMaster):
    def __init__(self,idBase,axi,dut):
        WriteOnlyMaster.__init__(self,idBase,axi,dut)
        ReadOnlyMaster.__init__(self,idBase,axi, dut)
        self.readOrWriteRand = BoolRandomizer()


    def createInfrastructure(self):
        StreamDriverMaster(self.axi.arw, self.genSharedCmd, self.dut.clk, self.dut.reset)
        StreamDriverMaster(self.axi.w, self.genWriteData, self.dut.clk, self.dut.reset)
        StreamDriverSlave(self.axi.b, self.dut.clk, self.dut.reset)
        StreamDriverSlave(self.axi.r, self.dut.clk, self.dut.reset)

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
            self.doFinish = True


    def isCompleted(self):
        return WriteOnlyMaster.isCompleted(self) and ReadOnlyMaster.isCompleted(self)


class ReadOnlySlave:
    def __init__(self,axi,dut):
        self.axi = axi
        self.dut = dut
        self.readRspRand = BoolRandomizer()
        self.readRspQueues = [Queue() for i in xrange(256)]
        self.nonEmptyReadRspQueues = []
        axi.r.payload.hid <= 0

    def createInfrastructure(self):
        StreamDriverSlave(self.axi.ar, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.ar,self.onReadCmd,self.dut.clk,self.dut.reset)
        StreamDriverMaster(self.axi.r, self.genReadRsp, self.dut.clk, self.dut.reset)


    def onReadCmd(self,trans):
        for i in xrange(trans.len+1):
            rsp = Transaction()
            rsp.data = trans.addr + i
            rsp.resp = 0
            rsp.hid = trans.hid
            if i == trans.len:
                rsp.last = 1
            else:
                rsp.last = 0
            queue = self.readRspQueues[trans.hid]
            if queue.empty():
                self.nonEmptyReadRspQueues.append(queue)
            queue.put(rsp)


    def genReadRsp(self):
        if len(self.nonEmptyReadRspQueues) == 0:
            return None
        if not self.readRspRand.get():
            return None
        queue = random.choice(self.nonEmptyReadRspQueues)
        trans = queue.get()
        if queue.empty():
            self.nonEmptyReadRspQueues.remove(queue)
        return trans

class WriteOnlySlave:
    def __init__(self,axi,dut):
        self.axi = axi
        self.dut = dut
        self.writeRspRand = BoolRandomizer()
        self.writeCmds = []
        self.writeDatas = []
        self.writeRspQueues = [Queue() for i in xrange(256)]
        self.nonEmptyWriteRspQueues = []
        axi.b.payload.hid <= 0

    def createInfrastructure(self):
        StreamDriverSlave(self.axi.aw, self.dut.clk, self.dut.reset)
        StreamDriverSlave(self.axi.w, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.aw,self.onWriteCmd,self.dut.clk,self.dut.reset)
        StreamMonitor(self.axi.w,self.onWriteData,self.dut.clk,self.dut.reset)
        StreamDriverMaster(self.axi.b, self.genWriteRsp, self.dut.clk, self.dut.reset)

    def onWriteCmd(self,trans):
        self.writeCmds.append(trans)
        self.managePendingWrites()

    def onWriteData(self, trans):
        self.writeDatas.append(trans)
        self.managePendingWrites

    def managePendingWrites(self):
        if len(self.writeCmds) != 0:
            cmd = self.writeCmds[0]
            beatCount = cmd.len + 1
            if len(self.writeDatas) >= beatCount:
                datas = self.writeDatas[0:beatCount - 1]

                # Clean
                self.writeCmds = self.writeCmds[1:]
                self.writeDatas = self.writeDatas[beatCount:]

                # Rsp
                rsp = Transaction()
                rsp.hid = cmd.hid
                rsp.resp = 0

                queue = self.writeRspQueues[cmd.hid]
                if queue.empty():
                    self.nonEmptyWriteRspQueues.append(queue)
                queue.put(rsp)

    def genWriteRsp(self):
        if len(self.nonEmptyWriteRspQueues) == 0:
            return None
        if not self.writeRspRand.get():
            return None
        queue = random.choice(self.nonEmptyWriteRspQueues)
        trans = queue.get()
        if queue.empty():
            self.nonEmptyWriteRspQueues.remove(queue)
        return trans

class SharedSlave(WriteOnlySlave,ReadOnlySlave):
    def __init__(self,axi,dut):
        WriteOnlySlave.__init__(self,axi,dut)
        ReadOnlySlave.__init__(self,axi, dut)


    def createInfrastructure(self):
        StreamMonitor(self.axi.arw,self.onSharedCmd,self.dut.clk,self.dut.reset)
        StreamDriverSlave(self.axi.arw, self.dut.clk, self.dut.reset)

        StreamDriverMaster(self.axi.r, self.genReadRsp, self.dut.clk, self.dut.reset)

        StreamDriverSlave(self.axi.w, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.w, self.onWriteData, self.dut.clk, self.dut.reset)
        StreamDriverMaster(self.axi.b, self.genWriteRsp, self.dut.clk, self.dut.reset)


    def onSharedCmd(self,trans):
        if trans.write == 1:
            self.onWriteCmd(trans)
        else:
            self.onReadCmd(trans)



@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))

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
        writeMaster = WriteOnlyMaster(0 + idx * 4,axiMaster,dut)
        writeMaster.createInfrastructure()

        readMaster = ReadOnlyMaster(0 + idx * 4, axiMaster, dut)
        readMaster.createInfrastructure()


    for idx,axiMaster in enumerate(axiReadOnlyMasters):
        readMaster = ReadOnlyMaster(4 + idx * 4, axiMaster, dut)
        readMaster.createInfrastructure()


    for idx,axiMaster in enumerate(axiWriteOnlyMasters):
        writeMaster = WriteOnlyMaster(8 + idx * 4,axiMaster,dut)
        writeMaster.createInfrastructure()


    for idx,axiMaster in enumerate(axiSharedMasters):
        master = SharedMaster(12 + idx * 4,axiMaster,dut)
        master.createInfrastructure()




    for idx,axiSlave in enumerate(axiSlaves):
        writeSlave = WriteOnlySlave(axiSlave,dut)
        writeSlave.createInfrastructure()

        readSlave = ReadOnlySlave(axiSlave, dut)
        readSlave.createInfrastructure()


    for idx,axiSlave in enumerate(axiReadOnlySlaves):
        readSlave = ReadOnlySlave(axiSlave, dut)
        readSlave.createInfrastructure()

    for idx,axiSlave in enumerate(axiWriteOnlySlaves):
        writeSlave = WriteOnlySlave(axiSlave,dut)
        writeSlave.createInfrastructure()

    for idx,axiSlave in enumerate(axiSharedSlaves):
        sharedSlave = SharedSlave(axiSlave, dut)
        sharedSlave.createInfrastructure()

    # Run until completion
    # while True:
    #     yield RisingEdge(dut.clk)
    #     done = True
    #     for handle in masterHandles:
    #         if not handle.isCompleted():
    #             done = False
    #
    #     for l in idToWrites:
    #         if l:
    #             done = False
    #     if done:
    #         break

    yield Timer(1000*1000)

    dut.log.info("Cocotb test done")
