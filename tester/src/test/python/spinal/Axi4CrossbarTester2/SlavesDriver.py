import random
from queue import Queue

from cocotb.result import TestFailure
from cocotblib.misc import BoolRandomizer

from cocotblib.Stream import StreamDriverSlave, StreamDriverMaster, Transaction, StreamMonitor


class ReadOnlySlaveDriver:
    def __init__(self,axi,base,size,dut):
        self.axi = axi
        self.size = size
        self.base = base
        self.dut = dut
        self.readRspRand = BoolRandomizer()
        self.readRspQueues = [Queue() for i in range(256)]
        self.nonEmptyReadRspQueues = []
        axi.r.payload.hid <= 0

    def createInfrastructure(self):
        StreamDriverSlave(self.axi.ar, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.ar, self.onReadCmd, self.dut.clk, self.dut.reset)
        StreamDriverMaster(self.axi.r, self.genReadRsp, self.dut.clk, self.dut.reset)
        return self

    def onReadCmd(self,trans):
        if trans.addr < self.base or trans.addr >= self.base + self.size:
            raise TestFailure("WRONG ADDRESS addr=%d base=%d size=%d" %(trans.addr,self.base,self.size))
        for i in range(trans.len+1):
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

class WriteOnlySlaveDriver:
    def __init__(self,axi,base,size,dut):
        self.axi = axi
        self.size = size
        self.base = base
        self.dut = dut
        self.writeRspRand = BoolRandomizer()
        self.writeCmds = []
        self.writeDatas = []
        self.writeRspQueues = [Queue() for i in range(256)]
        self.nonEmptyWriteRspQueues = []
        axi.b.payload.hid <= 0

    def createInfrastructure(self):
        StreamDriverSlave(self.axi.aw, self.dut.clk, self.dut.reset)
        StreamDriverSlave(self.axi.w, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.aw, self.onWriteCmd, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.w, self.onWriteData, self.dut.clk, self.dut.reset)
        StreamDriverMaster(self.axi.b, self.genWriteRsp, self.dut.clk, self.dut.reset)
        return self

    def onWriteCmd(self,trans):
        if trans.addr < self.base or trans.addr >= self.base + self.size:
            raise TestFailure("WRONG ADDRESS addr=%d base=%d size=%d" %(trans.addr,self.base,self.size))
        self.writeCmds.append(trans)
        self.managePendingWrites()

    def onWriteData(self, trans):
        self.writeDatas.append(trans)
        self.managePendingWrites()

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

class SharedSlaveDriver(WriteOnlySlaveDriver, ReadOnlySlaveDriver):
    def __init__(self,axi,base,size,dut):
        WriteOnlySlaveDriver.__init__(self, axi,base,size, dut)
        ReadOnlySlaveDriver.__init__(self, axi,base,size, dut)


    def createInfrastructure(self):
        StreamMonitor(self.axi.arw, self.onSharedCmd, self.dut.clk, self.dut.reset)
        StreamDriverSlave(self.axi.arw, self.dut.clk, self.dut.reset)

        StreamDriverMaster(self.axi.r, self.genReadRsp, self.dut.clk, self.dut.reset)

        StreamDriverSlave(self.axi.w, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.w, self.onWriteData, self.dut.clk, self.dut.reset)
        StreamDriverMaster(self.axi.b, self.genWriteRsp, self.dut.clk, self.dut.reset)
        return self

    def onSharedCmd(self,trans):
        if trans.write == 1:
            self.onWriteCmd(trans)
        else:
            self.onReadCmd(trans)
