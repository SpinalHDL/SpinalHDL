from cocotblib.Phase import Infrastructure
from cocotblib.Scorboard import ScorboardOutOfOrder

from cocotblib.Stream import Transaction, StreamMonitor


class ReadOnlyMasterMonitor(Infrastructure):
    def __init__(self,name,parent,axi,dut):
        Infrastructure.__init__(self,name,parent)
        self.axi = axi
        self.dut = dut
        self.readRspCounter = 0
        self.readRspScoreboard = ScorboardOutOfOrder("readRspScoreboard", self)

    def createInfrastructure(self):
        StreamMonitor(self.axi.ar, self.onReadCmd, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.r, self.onReadRsp, self.dut.clk, self.dut.reset)

    def onReadCmd(self,cmd):
        for i in range(cmd.len + 1):
            rsp = Transaction()
            rsp.hid = cmd.hid
            if cmd.addr < (1 << 14):
                rsp.data = cmd.addr + i
                rsp.resp = 0
            else:
                rsp.data = 0
                rsp.resp = 3

            rsp.last = 1 if i == cmd.len else 0
            self.readRspScoreboard.refPush(rsp, cmd.hid)

    def onReadRsp(self,rsp):
        if rsp.resp == 3:
            rsp.data = 0
        self.readRspScoreboard.uutPush(rsp,rsp.hid)
        if rsp.last == 1:
            self.readRspCounter += 1

    def canPhaseProgress(self, phase):
        return self.readRspCounter > 50



class WriteOnlyMasterMonitor(Infrastructure):
    def __init__(self,name,parent,axi,dut):
        Infrastructure.__init__(self,name,parent)
        self.axi = axi
        self.dut = dut
        self.writeRspCounter = 0
        self.writeRspScoreboard = ScorboardOutOfOrder("writeRspScoreboard", self)

    def createInfrastructure(self):
        StreamMonitor(self.axi.aw, self.onWriteCmd, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.b, self.onWriteRsp, self.dut.clk, self.dut.reset)

    def onWriteCmd(self,cmd):
        rsp = Transaction()
        rsp.hid = cmd.hid
        if cmd.addr < (1 << 14):
            rsp.resp = 0
        else:
            rsp.resp = 3

        self.writeRspScoreboard.refPush(rsp, cmd.hid)

    def onWriteRsp(self,rsp):
        self.writeRspScoreboard.uutPush(rsp,rsp.hid)
        self.writeRspCounter += 1

    def canPhaseProgress(self, phase):
        return self.writeRspCounter > 50


class SharedMasterMonitor(ReadOnlyMasterMonitor,WriteOnlyMasterMonitor):
    def __init__(self,name,parent,axi,dut):
        ReadOnlyMasterMonitor.__init__(self,name,parent,axi,dut)
        WriteOnlyMasterMonitor.__init__(self,name,parent,axi,dut)

    def createInfrastructure(self):
        StreamMonitor(self.axi.arw, self.onSharedCmd, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.b, self.onWriteRsp, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.r, self.onReadRsp, self.dut.clk, self.dut.reset)

    def onSharedCmd(self,cmd):
        if cmd.write == 1:
            self.onWriteCmd(cmd)
        else:
            self.onReadCmd(cmd)