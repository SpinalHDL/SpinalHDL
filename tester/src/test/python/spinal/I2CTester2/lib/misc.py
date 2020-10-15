from cocotb import fork
from cocotb.decorators import coroutine
from cocotb.triggers import Timer, Edge, RisingEdge

from cocotblib.misc import clockedWaitTrue, testBit


class OpenDrainSoftConnection:
    def __init__(self, interconnect):
        self.interconnect = interconnect
        self._value = True

    def write(self,value):
        if self._value != value:
            self._value = value
            self.interconnect.evaluate()

    def read(self):
        return self.interconnect.value

class OpenDrainInterconnect:

    def __init__(self,applyChange = None):
        self.applyChange = applyChange
        self.softConnections = []
        self.hardWriters = []
        self.hardReaders = []
        self.value = True

    def newSoftConnection(self):
        endpoint = OpenDrainSoftConnection(self)
        self.softConnections.append(endpoint)
        return endpoint

    @coroutine
    def pinWatcher(self,driver):
        while True:
            yield Edge(driver)
            self.evaluate()

    def addHardDriver(self,driver):
        self.hardWriters.append(driver)
        fork(self.pinWatcher(driver))

    def addHardReader(self,reader):
        self.hardReaders.append(reader)
        reader <= self.value

    def evaluate(self):
        newValue = True
        for soft in self.softConnections:
            newValue &= soft._value

        for hard in self.hardWriters:
            newValue &= (int(hard) == 1)

        if newValue != self.value:
            self.value = newValue
            for reader in self.hardReaders:
                reader <= newValue
            if self.applyChange:
                self.applyChange(newValue)


class I2cSoftMaster:
    def __init__(self,scl,sda,period,clk):
        self.scl = scl
        self.sda = sda
        self.period = period
        self.clk = clk

    @coroutine
    def waitScl(self):
        while True:
            if self.scl.read():
                break
            yield RisingEdge(self.clk)

    @coroutine
    def sendStart(self):
        self.sda.write(False)
        yield Timer(self.period)
        self.scl.write(False)

    @coroutine
    def sendRestart(self):
        yield Timer(self.period/2)
        self.sda.write(True)
        yield Timer(self.period/2)
        self.scl.write(True)
        yield self.waitScl()
        yield Timer(self.period)
        yield self.sendStart()


    @coroutine
    def sendBit(self, value,ret = None):
        yield Timer(self.period / 2)
        self.sda.write(value)
        yield Timer(self.period / 2)
        self.scl.write(True)
        yield self.waitScl()
        yield Timer(self.period)
        if ret:
            ret[0] = self.sda.read()
        self.scl.write(False)

    @coroutine
    def sendBitCheck(self, value, expected):
        buffer = [0]
        yield self.sendBit(value, buffer)
        assert buffer[0] == expected


    @coroutine
    def sendByte(self, value ,ret = None):
        if ret != None :
            ret[0] = 0
        buffer = [False]
        for i in range(8):
            yield self.sendBit(testBit(value,7-i),buffer)
            if ret != None:
                ret[0] |= buffer[0] << (7-i)

    @coroutine
    def sendByteCheck(self, value, expected):
        buffer = [0]
        yield self.sendByte(value, buffer)
        assert buffer[0] == expected

    @coroutine
    def sendStop(self):
        yield Timer(self.period/2)
        self.sda.write(False)
        yield Timer(self.period/2)
        self.scl.write(True)
        yield self.waitScl()
        yield Timer(self.period)
        self.sda.write(True)
        yield Timer(self.period)

    @coroutine
    def sendDrop(self):
        yield Timer(self.period/2)
        self.sda.write(True)
        yield Timer(self.period/2)
        self.scl.write(True)

    @coroutine
    def wait(self,bauds):
        yield Timer(self.period*bauds)