import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure, TestSuccess
from cocotb.triggers import Timer, Edge, RisingEdge, Join, FallingEdge

from spinal.common.Ahb3 import Ahb3MasterDriver, Ahb3SlaveMemory, Ahb3MasterIdle, Ahb3TraficGenerator, Ahb3MasterReadChecker
from spinal.common.misc import setBit, randSignal, assertEquals, truncUInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer, StreamRandomizer,StreamReader, FlowRandomizer, Bundle, simulationSpeedPrinter


class Ahb3TraficGeneratorWithMemory(Ahb3TraficGenerator):
    def genRandomAddress(self):
        while True:
            value = Ahb3TraficGenerator.genRandomAddress(self)
            if value < self.id*0x100 or value >= (self.id + 1)*0x100:
                return value

    def getTransactions(self):
        transactions = Ahb3TraficGenerator.getTransactions(self)
        for trans in transactions:
            if trans.HTRANS >= 2:
                write = trans.HWRITE
                size = 1 << trans.HSIZE
                address = trans.HADDR
                addressOffset = address % (self.dataWidth / 8)

                if write == 1:
                    for idx in range(size):
                        self.ram[address  + idx] = (trans.HWDATA >> (8*(addressOffset + idx))) & 0xFF
                else:
                    data = 0
                    for idx in xrange(size):
                        data |= self.ram[address + idx] << (8*(addressOffset + idx))
                    self.readBuffer.put(data)
                    # cocotb.log.info("FILL " + str(self.readBuffer.qsize()) + "  " + str(trans.HADDR))


        return transactions

    def __init__(self, addressWidth, dataWidth,readBuffer,id):
        Ahb3TraficGenerator.__init__(self, addressWidth, dataWidth)
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
        drivers.append(Ahb3MasterDriver(ahb, Ahb3TraficGeneratorWithMemory(10, 32,readQueue,i+1), dut.clk, dut.reset))
        checkers.append(Ahb3MasterReadChecker(ahb, readQueue, dut.clk, dut.reset))

    Ahb3MasterIdle(Bundle(dut, "ahbMasters_1"))
    Ahb3MasterIdle(Bundle(dut, "ahbMasters_2"))
    # Ahb3MasterDriver(Bundle(dut, "ahbMasters_1"), dut.clk, dut.reset)
    # Ahb3MasterDriver(Bundle(dut, "ahbMasters_2"), dut.clk, dut.reset)
    # Ahb3SlaveMemory(Bundle(dut, "ahbSlaves_0"), 0x000, 0x400, dut.clk, dut.reset)
    Ahb3SlaveMemory(Bundle(dut, "ahbSlaves_0"),0x000,0x100, dut.clk, dut.reset)
    Ahb3SlaveMemory(Bundle(dut, "ahbSlaves_1"),0x100,0x100, dut.clk, dut.reset)
    Ahb3SlaveMemory(Bundle(dut, "ahbSlaves_2"),0x200,0x100, dut.clk, dut.reset)
    Ahb3SlaveMemory(Bundle(dut, "ahbSlaves_3"),0x300,0x100, dut.clk, dut.reset)
    # ahbMaster0.HADDR <= 0
    while True:
        yield RisingEdge(dut.clk)
        done = True
        for checker in checkers:
            if checker.counter < 1000:
                done = False
        if done:
            break
    #     ahbMaster0.HADDR <= int(ahbMaster0.HADDR) + 1



    dut.log.info("Cocotb test done")
