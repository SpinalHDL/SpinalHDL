import random
from queue import Queue

import cocotb
from cocotb.triggers import RisingEdge
from cocotblib.AhbLite3 import AhbLite3MasterDriver, AhbLite3SlaveMemory, AhbLite3TraficGenerator, AhbLite3MasterReadChecker

from cocotblib.misc import ClockDomainAsyncReset, Bundle, simulationSpeedPrinter


class AhbLite3TraficGeneratorWithMemory(AhbLite3TraficGenerator):
    def genRandomAddress(self):
        while True:
            value = AhbLite3TraficGenerator.genRandomAddress(self)
            if (value >> 10) != self.id  and ((value >> 8) & 0x3) == self.id:
                return value

    def getTransactions(self):
        transactions = AhbLite3TraficGenerator.getTransactions(self)
        for trans in transactions:
            if trans.HTRANS >= 2:
                write = trans.HWRITE
                size = 1 << trans.HSIZE
                address = trans.HADDR
                addressOffset = address % (self.dataWidth // 8)

                if write == 1:
                    for idx in range(size):
                        self.ram[address  + idx] = (trans.HWDATA >> (8*(addressOffset + idx))) & 0xFF
                        # cocotb.log.info("WRITE %d %x %x" % (self.id,address  + idx, (trans.HWDATA >> (8*(addressOffset + idx))) & 0xFF))
                else:
                    data = 0
                    for idx in range(size):
                        data |= self.ram[address + idx] << (8*(addressOffset + idx))
                    self.readBuffer.put(data)
                    # cocotb.log.info("READ %d %x %x" % (self.id, trans.HADDR,data))


        return transactions

    def __init__(self, addressWidth, dataWidth,readBuffer,id):
        AhbLite3TraficGenerator.__init__(self, addressWidth, dataWidth)
        self.ram = bytearray(b'\x00' * (1 << addressWidth))
        self.readBuffer = readBuffer
        self.id = id


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)


    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))

    drivers = []
    checkers = []
    for i in range(3):
        readQueue = Queue()
        ahb = Bundle(dut, "ahbMasters_" + str(i))
        drivers.append(AhbLite3MasterDriver(ahb, AhbLite3TraficGeneratorWithMemory(12, 32,readQueue,i), dut.clk, dut.reset))
        checkers.append(AhbLite3MasterReadChecker(ahb, readQueue, dut.clk, dut.reset))

    # AhbLite3MasterIdle(Bundle(dut, "ahbMasters_1"))
    # AhbLite3MasterIdle(Bundle(dut, "ahbMasters_2"))
    AhbLite3SlaveMemory(Bundle(dut, "ahbSlaves_0"), 0x000, 0x400, dut.clk, dut.reset)
    AhbLite3SlaveMemory(Bundle(dut, "ahbSlaves_1"), 0x400, 0x400, dut.clk, dut.reset)
    AhbLite3SlaveMemory(Bundle(dut, "ahbSlaves_2"), 0x800, 0x400, dut.clk, dut.reset)
    AhbLite3SlaveMemory(Bundle(dut, "ahbSlaves_3"), 0xC00, 0x400, dut.clk, dut.reset)

    while True:
        yield RisingEdge(dut.clk)
        done = True
        for checker in checkers:
            if checker.counter < 1000:
                done = False
        if done:
            break

    dut.log.info("Cocotb test done")
