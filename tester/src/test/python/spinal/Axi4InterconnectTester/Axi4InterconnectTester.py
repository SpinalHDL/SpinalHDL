import random
from Queue import Queue

import cocotb
from cocotb.triggers import Timer, RisingEdge

from spinal.common.Axi4 import Axi4
from spinal.common.Stream import StreamDriverSlave, StreamDriverMaster, StreamTransaction, StreamMonitor
from spinal.common.misc import ClockDomainAsyncReset, simulationSpeedPrinter, randBits, BoolRandomizer, assertEquals


class MasterHandle:
    def __init__(self,id,idToWrites):
        self.hid = id
        self.readCounter = 0
        self.writeCounter = 0
        self.doFinish = False
        self.readMonitorQueues = [Queue() for i in xrange(4)] # One queue for each transaction id
        self.writeCmdQueue = Queue()
        self.writeDataQueue = Queue()
        self.idToWrites = idToWrites

    def isCompleted(self):
        if not self.doFinish:
            return False
        for q in self.readMonitorQueues:
            if not q.empty():
                return False
        # print(str(len(self.writeDataQueue.queue)))
        # if not self.writeDataQueue.empty():
        #     return False
        # if not self.writeCmdQueue.empty():
        #     return False
        return True

    def genWrite(self):
        idOffset = randBits(2)
        writeCmd = StreamTransaction()
        writeCmd.addr = self.genRandomAddress()
        writeCmd.hid = self.hid*4 + idOffset #Each master can use 4 id
        writeCmd.region = randBits(4)
        writeCmd.len = randBits(4)
        writeCmd.size = randBits(3)
        writeCmd.burst = randBits(2)
        writeCmd.lock = randBits(1)
        writeCmd.cache = randBits(4)
        writeCmd.qos = randBits(4)
        writeCmd.prot = randBits(3)
        self.writeCmdQueue.put(writeCmd)

        writeCmd.linkedDatas = []
        for i in xrange(writeCmd.len + 1):
            writeData = StreamTransaction()
            writeData.data = randBits(32)
            writeData.strb = randBits(4)
            writeData.last = 1 if i == writeCmd.len else 0
            self.writeDataQueue.put(writeData)
            writeCmd.linkedDatas.append(writeData)

        self.idToWrites[writeCmd.hid].append(writeCmd)

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



    def genRandomAddress(self):
        while True:
            value = randBits(12)
            if (value >> 10) != self.hid  and ((value >> 8) & 0x3) == self.hid:
                return value

    def genReadCmd(self):
        if self.doFinish:
            return None
        idOffset = randBits(2)
        trans = StreamTransaction()
        trans.addr = self.genRandomAddress()
        trans.hid = self.hid*4 + idOffset #Each master can use 4 id
        trans.region = randBits(4)
        trans.len = randBits(4)
        trans.size = randBits(3)
        trans.burst = randBits(2)
        trans.lock = randBits(1)
        trans.cache = randBits(4)
        trans.qos = randBits(4)
        trans.prot = randBits(3)

        trans.progress = 0
        self.readMonitorQueues[idOffset].put(trans)
        # print("Master START  %d %x" % (trans.hid, trans.addr))
        return trans

    def onReadRsp(self, trans):
        queue = self.readMonitorQueues[trans.hid - self.hid * 4]
        task = queue.queue[0]
        assertEquals(trans.data,task.addr + task.progress,"Readed value is wrong")
        task.progress += 1
        if task.progress == task.len + 1:
            # print("Master FINISH %d %x" % (task.hid,task.addr))
            queue.get()
            self.readCounter += 1
            self.updateDoFinish()



    def genWriteCmd(self):
        return self.getNextWriteCmdTrans()

    def genWriteData(self):
        return self.getNextWriteDataTrans()

    def onWriteRsp(self,trans):
        self.writeCounter = self.writeCounter + 1
        self.updateDoFinish()

    def updateDoFinish(self):
        if self.readCounter > 100 and self.writeCounter > 100:
            self.doFinish = True


class SlaveHandle:
    def __init__(self,id,idToWrites):
        self.tasksQueues = [Queue()] * 64 # One queue of task for each transaction id
        self.genRandomizer = BoolRandomizer()
        self.hid = id
        self.writeCmds = []
        self.writeDatas = []
        self.idToWrites = idToWrites

    def getRandTaskList(self):
        tasksQueuesFiltred = [tasksList for tasksList in self.tasksQueues if not tasksList.empty()]
        if len(tasksQueuesFiltred) == 0:
            return None
        return random.choice(tasksQueuesFiltred)

    def genReadRsp(self):
        tasksQueue = self.getRandTaskList()
        if tasksQueue:
            if self.genRandomizer.get():
                task = tasksQueue.queue[0]
                trans = StreamTransaction()
                trans.data = task.addr + task.progress
                trans.resp = 0
                trans.hid = task.hid
                task.progress += 1
                if task.progress == task.len + 1:
                    trans.last = 1
                    tasksQueue.get()
                else:
                    trans.last = 0
                return trans


    def onReadCmd(self, trans):
        trans.progress = 0
        assertEquals(trans.addr >> 10, self.hid, ":(")
        self.tasksQueues[trans.hid].put(trans)


    def onWriteCmd(self,trans):
        assertEquals(trans.addr >> 10, self.hid, ":(")
        self.writeCmds.append(trans)

    def onWriteData(self, trans):
        self.writeDatas.append(trans)

    def genWriteRsp(self):
        if len(self.writeCmds) != 0:
            cmd = self.writeCmds[0]
            beatCount = cmd.len + 1
            if len(self.writeDatas) >= beatCount:
                datas = self.writeDatas[0:beatCount-1]

                #Check it
                masterWrite = next(write for write in self.idToWrites[cmd.hid & 0xF] if write.addr >> 10 == self.hid)
                assertEquals(cmd.addr,masterWrite.addr,"write cmd missmatch")
                assertEquals(cmd.len, masterWrite.len, "write cmd missmatch")
                for data, dataRef in zip(datas, masterWrite.linkedDatas):
                    assertEquals(data.data, dataRef.data, "write data missmatch")

                #Clean
                self.writeCmds = self.writeCmds[1:]
                self.writeDatas = self.writeDatas[beatCount:]
                self.idToWrites[cmd.hid & 0xF].remove(masterWrite)

                #Answer
                trans = StreamTransaction()
                trans.hid = cmd.hid
                trans.resp = 0
                return trans
        return None


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))

    axiMasters = [Axi4(dut, "axiMasters_" + str(i)) for i in range(3)]
    axiSlaves = [Axi4(dut, "axiSlaves_" + str(i)) for i in range(4)]

    masterHandles = []
    idToWrites = [[] for i in xrange(16)]

    # Instanciate master side
    for idx,axiMaster in enumerate(axiMasters):
        masterHandle = MasterHandle(idx,idToWrites)
        masterHandles.append(masterHandle)

        # Read
        StreamDriverMaster(axiMaster.ar, masterHandle.genReadCmd, dut.clk, dut.reset)
        StreamDriverSlave(axiMaster.r, dut.clk, dut.reset)
        StreamMonitor(axiMaster.r, masterHandle.onReadRsp, dut.clk, dut.reset)

        # Write
        StreamDriverMaster(axiMaster.aw, masterHandle.genWriteCmd, dut.clk, dut.reset)
        StreamDriverMaster(axiMaster.w, masterHandle.genWriteData, dut.clk, dut.reset)
        StreamDriverSlave(axiMaster.b, dut.clk, dut.reset)
        StreamMonitor(axiMaster.b, masterHandle.onWriteRsp, dut.clk, dut.reset)


    # instanciate slave side
    for idx,axiSlave in enumerate(axiSlaves):
        axiSlave.r.payload.hid <= 0
        axiSlave.b.payload.hid <= 0
        slaveHandle = SlaveHandle(idx,idToWrites)

        # Read
        StreamDriverSlave(axiSlave.ar,dut.clk,dut.reset)
        StreamDriverMaster(axiSlave.r, slaveHandle.genReadRsp, dut.clk, dut.reset)
        StreamMonitor(axiSlave.ar, slaveHandle.onReadCmd, dut.clk, dut.reset)

        # Write
        StreamMonitor(axiSlave.aw, slaveHandle.onWriteCmd, dut.clk, dut.reset)
        StreamDriverSlave(axiSlave.aw, dut.clk, dut.reset)
        StreamMonitor(axiSlave.w, slaveHandle.onWriteData, dut.clk, dut.reset)
        StreamDriverSlave(axiSlave.w, dut.clk, dut.reset)
        StreamDriverMaster(axiSlave.b, slaveHandle.genWriteRsp, dut.clk, dut.reset)

    # Run until completion
    while True:
        yield RisingEdge(dut.clk)
        done = True
        for handle in masterHandles:
            if not handle.isCompleted():
                done = False

        # for l in idToWrites:
        #     if l:
        #         done = False
        if done:
            break

    yield Timer(1000*10)

    dut.log.info("Cocotb test done")
