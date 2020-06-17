import random
from queue import Queue

import cocotb
from cocotb.triggers import RisingEdge
from cocotblib.AhbLite3 import AhbLite3MasterDriver, AhbLite3TraficGenerator, AhbLite3MasterReadChecker, AhbLite3Terminaison

from cocotblib.misc import ClockDomainAsyncReset, Bundle, simulationSpeedPrinter


class AhbLite3TraficGeneratorWithMemory(AhbLite3TraficGenerator):
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
                        # cocotb.log.info("WRITE %x %x" % (address  + idx, (trans.HWDATA >> (8*(addressOffset + idx))) & 0xFF))
                else:
                    data = 0
                    for idx in range(size):
                        data |= self.ram[address + idx] << (8*(addressOffset + idx))
                    self.readBuffer.put(data)
                    # cocotb.log.info("READ %x %x" % (trans.HADDR,data))


        return transactions

    def __init__(self, addressWidth, dataWidth,readBuffer):
        AhbLite3TraficGenerator.__init__(self, addressWidth, dataWidth)
        self.ram = bytearray(b'\x00' * (1 << addressWidth))
        self.readBuffer = readBuffer



@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    from cocotblib.misc import cocotbXHack
    cocotbXHack()
    random.seed(0)

    cocotb.fork(simulationSpeedPrinter(dut.clk))

    # elements = [a for a in dut.AhbRam if a._name.startswith("")]
    # for e in elements:
    #     print(str(e._name))

    # while True:
    #     dut.AhbRam.ram_port1_enable <= 1
    #     dut.AhbRam.ram_port1_mask <= 0xF
    #     dut.AhbRam.ram_port1_address <= 0X90
    #     dut.AhbRam.ram_port1_data <= 0xCAFEF00D
    #
    #     dut.AhbRam.reset <= 1
    #     dut.AhbRam.ram_port1_enable <= 1
    #     yield Timer(1000)
    #     dut.AhbRam.ram_port1_enable <= 1
    #     dut.AhbRam.clk <= 0
    #     yield Timer(1000)
    #     dut.AhbRam.ram_port1_enable <= 1
    #     dut.AhbRam.clk <= 1
    #     yield Timer(1000)
    #     dut.AhbRam.ram_port1_enable <= 0
    #     yield Timer(1000)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))

    readQueue = Queue()
    ahb = Bundle(dut, "ahb")
    driver  = AhbLite3MasterDriver(ahb, AhbLite3TraficGeneratorWithMemory(10, 32,readQueue), dut.clk, dut.reset)
    checker = AhbLite3MasterReadChecker(ahb, readQueue, dut.clk, dut.reset)
    terminaison = AhbLite3Terminaison(ahb, dut.clk, dut.reset)



    while True:
        yield RisingEdge(dut.clk)
        if checker.counter > 4000:
            break

    dut.log.info("Cocotb test done")
