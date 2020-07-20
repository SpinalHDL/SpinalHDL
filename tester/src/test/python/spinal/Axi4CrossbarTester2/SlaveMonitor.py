from cocotblib.Phase import Infrastructure
from cocotblib.Scorboard import ScorboardInOrder

from cocotblib.Stream import Transaction, StreamMonitor


class WriteDataMonitor(Infrastructure):
    def __init__(self,name,parent,axi,dut):
        Infrastructure.__init__(self,name,parent)
        self.axi = axi
        self.dut = dut
        self.dataScoreboard = ScorboardInOrder("scoreboard", self)

    def createInfrastructure(self):
        StreamMonitor(self.axi.aw, self.onWriteCmd, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.w, self.onWriteData, self.dut.clk, self.dut.reset)

    def onWriteCmd(self,cmd):
        for i in range(cmd.len + 1):
            writeData = Transaction()
            writeData.data = cmd.addr + i
            writeData.strb = (cmd.addr + i) & 0xF
            writeData.last = 1 if i == cmd.len else 0
            self.dataScoreboard.refPush(writeData)

    def onWriteData(self,trans):
        self.dataScoreboard.uutPush(trans)


class SharedDataMonitor(WriteDataMonitor):
    def __init__(self,name,parent,axi,dut):
        WriteDataMonitor.__init__(self,name,parent,axi,dut)

    def createInfrastructure(self):
        StreamMonitor(self.axi.arw, self.onSharedCmd, self.dut.clk, self.dut.reset)
        StreamMonitor(self.axi.w, self.onWriteData, self.dut.clk, self.dut.reset)

    def onSharedCmd(self,cmd):
        if cmd.write == 1:
            self.onWriteCmd(cmd)



#
# class CmdRoutingMonitor(Infrastructure):
#     def __init__(self,name,parent,cmd,dut):
#         Infrastructure.__init__(self,name,parent)
#         self.axi = axi
#         self.dut = dut
#         self.dataScoreboard = StreamScorboardInOrder("scoreboard",self)
#
#     def createInfrastructure(self):
#         StreamMonitor(self.axi.aw,self.onWriteCmd,self.dut.clk,self.dut.reset)
#         StreamMonitor(self.axi.w, self.onWriteData, self.dut.clk, self.dut.reset)
#
#     def onWriteCmd(self,cmd):
#         for i in xrange(cmd.len + 1):
#             writeData = Transaction()
#             writeData.data = cmd.addr + i
#             writeData.strb = (cmd.addr + i) & 0xF
#             writeData.last = 1 if i == cmd.len else 0
#             self.dataScoreboard.refPush(writeData)
#
#     def onWriteData(self,trans):
#         self.dataScoreboard.uutPush(trans)
