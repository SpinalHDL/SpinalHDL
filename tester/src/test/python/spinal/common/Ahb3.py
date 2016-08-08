import random

import cocotb
from cocotb.triggers import RisingEdge

from spinal.common.misc import randSignal, log2Up, BoolRandomizer

def Ahb3MasterIdle(ahb):
    ahb.HADDR <= 0
    ahb.HWRITE <= 0
    ahb.HSIZE <= 0
    ahb.HBURST <= 0
    ahb.HPROT <= 0
    ahb.HTRANS <= 0
    ahb.HMASTLOCK <= 0
    ahb.HWDATA <= 0


class Ahb3MasterDriver:
    def __init__(self,ahb,clk,reset):
        self.ahb = ahb
        self.clk = clk
        self.reset = reset

        cocotb.fork(self.stim())

    @cocotb.coroutine
    def stim(self):
        ahb = self.ahb
        ahb.HADDR     <= 0
        ahb.HWRITE    <= 0
        ahb.HSIZE     <= 0
        ahb.HBURST    <= 0
        ahb.HPROT     <= 0
        ahb.HTRANS    <= 0
        ahb.HMASTLOCK <= 0
        ahb.HWDATA    <= 0

        while True:
            yield self.waitReady()
            if random.random() < 0.2:
                burst = random.randint(0,7)
                burstLength = (1 << (burst >> 1)) << 1
                wrapped = burst != 1 and burst & 1 == 1
                incr = burst == 1
                write = random.random() < 0.5
                size = random.randint(0,len(ahb.HWDATA)/8)
                prot = random.randint(0,15)
                burstSize = (1 << size)*burstLength
                if wrapped:
                    address = random.randint(0,(1 << len(ahb.HADDR))-1) & ~((1 << size)-1)
                else:
                    address = random.randint(0, (1 << len(ahb.HADDR)) - 1) & ~((1 << burstSize) - 1)
                addressBase = address - address % burstSize
                if incr:
                    continue

                ahb.HWRITE <= write
                ahb.HSIZE <= log2Up(size)
                ahb.HBURST <= burst
                ahb.HPROT <= prot

                for beat in xrange(burstLength):
                    randSignal(ahb.HWDATA)
                    ahb.HADDR <= address
                    address += size
                    if(address == addressBase + size):
                        address = addressBase
                    yield self.waitReady()
                ahb.HTRANS <= 0



    @cocotb.coroutine
    def waitReady(self):
        yield RisingEdge(self.clk)
        while int(self.ahb.HREADY) == 0:
            yield RisingEdge(self.clk)



class Ahb3SlaveMemory:
    def __init__(self,ahb,base,size,clk,reset):
        self.ahb = ahb
        self.clk = clk
        self.reset = reset
        self.base = base
        self.size = size
        self.ram = bytearray(b'\x00' * size) #[0 for i in xrange(size)]

        cocotb.fork(self.stim())
        cocotb.fork(self.stimReady())

    @cocotb.coroutine
    def stimReady(self):
        randomizer = BoolRandomizer()
        self.ahb.HREADYOUT <= 1
        yield RisingEdge(self.clk)
        self.ahb.HREADYOUT <= randomizer.get()

    @cocotb.coroutine
    def stim(self):
        ahb = self.ahb
        ahb.HREADYOUT <= 1
        ahb.HRESP     <= 0
        ahb.HRDATA    <= 0
        valid = 0
        while True:
            yield self.waitReady()
            if valid == 1:
                if write == 1:
                    for idx in range(size):
                        self.ram[address + idx] = (ahb.HWDATA >> (addressOffset + 8*idx)) & 0xFF
                else:
                    data = 0
                    for idx in xrange(size):
                        data |= self.ram[address + idx] << (addressOffset + 8*idx)
                    ahb.HRDATA <= data

            valid = int(ahb.HSEL)
            write = int(ahb.HWRITE)
            size = 1 << int(ahb.HSIZE)
            address = int(ahb.HADDR)
            addressOffset = address % (len(ahb.HWDATA)/8)




    @cocotb.coroutine
    def waitReady(self):
        yield RisingEdge(self.clk)
        while int(self.ahb.HREADYIN) == 0:
            yield RisingEdge(self.clk)
