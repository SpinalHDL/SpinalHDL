import os

import cocotb
from cocotb.handle import simulator
from cocotb.log import SimLogFormatter
from cocotblib.misc import ClockDomainAsyncReset

from cocotblib.Stream import StreamMonitor, Stream


# @cocotb.coroutine
# def axiClkLogger(uut):
#     core = uut.axi_core.core
#
#
#     while(True):
#         yield RisingEdge(uut.io_axiClk):
#             if


class PinsecLogger:
    def __init__(self):
        path = os.getenv('WAVES_DIR', ".")
        self.dCmdLog = open(path + '/coreDCmd.log', 'w')
        self.dRspLog = open(path + '/coreDRsp.log', 'w')

    def getTimestamp(self):
        timeh, timel = simulator.get_sim_time()
        return "%6.2f ns" % (((timeh << 32) | timel)* (10.0**SimLogFormatter.sim_precision) / 1e-9)

    def logDCmd(self,dCmd):
        # self.dCmdLog.write(self.getTimestamp() + "\n")
        self.dCmdLog.write(str(dCmd) + "\n")
        self.dCmdLog.flush()

    def logDRsp(self, dRsp):
        # self.dRspLog.write(self.getTimestamp() + "\n")
        self.dRspLog.write(str(dRsp) + "\n")
        self.dRspLog.flush()

def pinsecClockGen(dut):
    cocotb.fork(ClockDomainAsyncReset(dut.io_axiClk, dut.io_asyncReset,7500))
    cocotb.fork(ClockDomainAsyncReset(dut.io_vgaClk, None,40000))
    # cocotb.fork(axiClkLogger(dut.uut))
    core = dut.uut.axi_core.core
    logger = PinsecLogger()
    StreamMonitor(Stream(core, "dCmd"),logger.logDCmd,dut.io_axiClk,dut.io_asyncReset)
    StreamMonitor(Stream(core, "dRsp"),logger.logDRsp, dut.io_axiClk, dut.io_asyncReset)