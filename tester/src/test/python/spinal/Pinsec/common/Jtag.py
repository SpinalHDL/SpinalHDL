import cocotb
from cocotb.triggers import Timer

class JtagMaster:
    def __init__(self,jtag,period,instructionWidth):
        self.jtag = jtag
        self.halfPeriod = period/2
        self.instructionWidth = instructionWidth
        self.jtag.tck <= 0
        # self.jtag.tms <= 0
        # self.jtag.tdi <= 0

    @cocotb.coroutine
    def goToIdle(self):
        for i in range(5):
            yield self.tmsMove(1)
        yield self.tmsMove(0)


    @cocotb.coroutine
    def setInstr(self,value):
        yield self.tmsMove(1)
        yield self.tmsMove(1)
        yield self.tmsMove(0)
        yield self.tmsMove(0)
        for i in range(self.instructionWidth):
            self.jtag.tms <= 1 if i == self.instructionWidth-1 else 0
            yield self.tdiMove((value >> i) & 1)
        yield self.tmsMove(0)
        yield self.tmsMove(1)
        yield self.tmsMove(1)
        yield self.tmsMove(0)

    @cocotb.coroutine
    def setData(self,value,width):
        yield self.tmsMove(1)
        yield self.tmsMove(0)
        yield self.tmsMove(0)
        for i in range(width):
            self.jtag.tms <= 1 if i == width-1 else 0
            yield self.tdiMove((value >> i) & 1)
        yield self.tmsMove(0)
        yield self.tmsMove(1)
        yield self.tmsMove(1)
        yield self.tmsMove(0)

    @cocotb.coroutine
    def getData(self,value,width):
        yield self.tmsMove(1)
        yield self.tmsMove(0)
        yield self.tmsMove(0)
        for i in range(width):
            yield self.tmsMove(1 if i == width-1 else 0)
            value[0] |= (self.lastTdo << i)
        yield self.tmsMove(0)
        yield self.tmsMove(1)
        yield self.tmsMove(1)
        yield self.tmsMove(0)

    @cocotb.coroutine
    def tdiMove(self, value):
        self.jtag.tdi <= value
        yield self.tick()

    @cocotb.coroutine
    def tmsMove(self,value):
        self.jtag.tms <= value
        yield self.tick()

    @cocotb.coroutine
    def tick(self):
        yield Timer(self.halfPeriod)
        self.lastTdo = int(self.jtag.tdo)
        self.jtag.tck <= 1
        yield Timer(self.halfPeriod)
        self.jtag.tck <= 0