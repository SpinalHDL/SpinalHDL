import random

import cocotb
from cocotb.triggers import Timer
from cocotblib.Phase import PhaseManager, Infrastructure, PHASE_WAIT_TASKS_END
from cocotblib.Scorboard import ScorboardInOrder
from cocotblib.misc import simulationSpeedPrinter, randBits, BoolRandomizer

from cocotblib.Stream import StreamDriverSlave, StreamDriverMaster, Transaction, StreamMonitor, Stream


class SdramTester(Infrastructure):
    def __init__(self,name,parent,cmd,rsp,clk,reset):
        Infrastructure.__init__(self, name, parent)
        StreamDriverMaster(cmd, self.genCmd, clk, reset)
        self.nonZeroRspCounter = 0
        self.cmdRandomizer = BoolRandomizer()
        self.writeRandomizer = BoolRandomizer()
        self.burstRandomizer = BoolRandomizer()
        self.lastAddr = 0
        self.closeIt = False
        self.ram = bytearray(b'\x00' * (1 << (9+2+2+1)))
        self.scorboard = ScorboardInOrder("scoreboard", self)
        StreamDriverSlave(rsp, clk, reset)
        # rsp.ready <= 1
        StreamMonitor(rsp, self.scorboard.uutPush, clk, reset)

    def canPhaseProgress(self, phase):
        return self.nonZeroRspCounter > 4000

    def startPhase(self, phase):
        Infrastructure.startPhase(self, phase)
        if phase == PHASE_WAIT_TASKS_END:
            self.closeIt = True

    def genCmd(self):
        if self.closeIt or not self.cmdRandomizer.get():
            return None

        trans = Transaction()

        if not self.burstRandomizer.get():
            trans.address = randBits(9+2+2)
        else:
            trans.address = self.lastAddr + 1
            trans.address = trans.address & ((1 << 13)-1)

        trans.write = self.writeRandomizer.get() and self.writeRandomizer.get()
        trans.mask = randBits(2)
        trans.data = randBits(16)
        trans.context = randBits(8)

        self.lastAddr = trans.address

        if trans.write == 0:
            rsp = Transaction()
            rsp.data = self.ram[trans.address*2] + (self.ram[trans.address*2+1] << 8)
            rsp.context = trans.context
            self.scorboard.refPush(rsp)
            if rsp.data != 0:
                self.nonZeroRspCounter += 1
                if self.nonZeroRspCounter % 50 == 0:
                    print(("self.nonZeroRspCounter=" + str(self.nonZeroRspCounter)))

        else:
            for i in range(2):
                if (trans.mask >> i) & 1 == 1:
                    self.ram[trans.address * 2 + i] = (trans.data >> (i*8)) & 0xFF

        return trans


@cocotb.coroutine
def ClockDomainAsyncResetCustom(clk,reset):
    if reset:
        reset <= 1
    clk <= 0
    yield Timer(100000)
    if reset:
        reset <= 0
    while True:
        clk <= 0
        yield Timer(3750)
        clk <= 1
        yield Timer(3750)

@cocotb.test()
def test1(dut):
    random.seed(0)
    from cocotblib.misc import cocotbXHack
    cocotbXHack()

    cocotb.fork(ClockDomainAsyncResetCustom(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))


    phaseManager = PhaseManager()
    phaseManager.setWaitTasksEndTime(1000*1000)

    SdramTester("sdramTester",phaseManager,Stream(dut,"io_bus_cmd"),Stream(dut,"io_bus_rsp"),dut.clk,dut.reset)

    yield phaseManager.run()

