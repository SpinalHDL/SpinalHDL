import random
from Queue import Queue

import cocotb
from cocotb.triggers import Timer, RisingEdge

from spinal.common.Axi4 import Axi4, Axi4Shared, Axi4AddrIncr
from spinal.common.Stream import StreamDriverSlave, StreamDriverMaster, Transaction, StreamMonitor
from spinal.common.misc import ClockDomainAsyncReset, simulationSpeedPrinter, randBits, BoolRandomizer, assertEquals


class Checker:
    def __init__(self,axi,dut):
        self.axi = axi
        self.dut = dut
        self.ram = bytearray(b'\x00' * 4096)
        self.doReadWriteCmdRand = BoolRandomizer()
        self.readWriteRand = BoolRandomizer()
        self.writeDataRand = BoolRandomizer()
        self.phase = 0
        self.phaseToggler = 0
        self.idToWriteRsp  = [Queue() for i in xrange(4)]
        self.idToReadRsp    = [Queue() for i in xrange(4)]
        self.writeDataTasks =  Queue()
        self.counter = 0
        axi.w.payload.last <= 0
        axi.r.payload.last <= 0
        StreamDriverSlave(axi.r, dut.clk, dut.reset)
        StreamDriverSlave(axi.b, dut.clk, dut.reset)
        StreamDriverMaster(axi.arw, self.genReadWriteCmd, dut.clk, dut.reset)
        StreamDriverMaster(axi.w, self.genWriteData, dut.clk, dut.reset)
        StreamMonitor(axi.r, self.onReadRsp, dut.clk, dut.reset)
        StreamMonitor(axi.b, self.onWriteRsp, dut.clk, dut.reset)

    def noPendingTransaction(self):
        for queue in self.idToWriteRsp:
            if not queue.empty():
                return False
        for queue in self.idToReadRsp:
            if not queue.empty():
                return False
        if not self.writeDataTasks.empty():
            return False
        return True;

    def genReadWriteCmd(self):
        if self.phaseToggler == 20:
            if self.noPendingTransaction():
                self.phaseToggler = 0
                self.phase = 1-self.phase
            else:
                return None

        if self.doReadWriteCmdRand.get():
            self.phaseToggler += 1

            cmd = Transaction()
            cmd.hid = randBits(2)  # Each master can use 4 id
            cmd.region = randBits(4)
            cmd.len = randBits(4)
            cmd.size = random.randint(0,2)
            cmd.burst = random.randint(0,2)
            if cmd.burst == 2:
                cmd.len = random.choice([2,4,8,16])-1
            else:
                cmd.len = randBits(4)
            cmd.lock = randBits(1)
            cmd.cache = randBits(4)
            cmd.qos = randBits(4)
            cmd.prot = randBits(3)
            cmd.addr  = cmd.hid * 512 + random.randint(0,512/(1 << cmd.size)-1)*(1 << cmd.size)
            if cmd.burst == 1:
                if cmd.addr + (1 << cmd.size)*(cmd.len + 1) >= (1<<11):
                    cmd.addr -= (1 << cmd.size)*(cmd.len + 1)
            if self.readWriteRand.get():
                cmd.write = 1
                if self.phase == 1:
                    cmd.addr += 2048

                beatAddr = cmd.addr
                for i in xrange(cmd.len+1):
                    dataTrans = Transaction()
                    dataTrans.data = randBits(32)
                    dataTrans.strb = randBits(4)
                    dataTrans.last = 1 if cmd.len == i else 0
                    self.writeDataTasks.put(dataTrans)

                    for s in xrange(4):
                        if (dataTrans.strb >> s) & 1 == 1:
                            self.ram[(beatAddr & ~3) + s] = (dataTrans.data >> (s*8)) & 0xFF
                    beatAddr = Axi4AddrIncr(beatAddr,cmd.burst,cmd.len,cmd.size)

                writeRsp = Transaction()
                writeRsp.resp = 0
                writeRsp.hid = cmd.hid
                self.idToWriteRsp[cmd.hid].put(writeRsp)
            else:
                cmd.write = 0

                if self.phase == 0:
                    cmd.addr += 2048

                beatAddr = cmd.addr
                for s in xrange(cmd.len + 1):
                    readRsp = Transaction()
                    addrBase = beatAddr & ~3
                    readRsp.data = self.ram[addrBase + 0] + (self.ram[addrBase + 1] << 8) + (self.ram[addrBase + 2] << 16) + (self.ram[addrBase + 3] << 24)
                    readRsp.resp = 0
                    readRsp.last = 1 if cmd.len == s else 0
                    readRsp.hid = cmd.hid
                    self.idToReadRsp[cmd.hid].put(readRsp)
                    beatAddr = Axi4AddrIncr(beatAddr, cmd.burst, cmd.len, cmd.size)
            return cmd



    def genWriteData(self):
        if not self.writeDataTasks.empty():
            if self.writeDataRand.get():
                return self.writeDataTasks.get()


    def onWriteRsp(self,trans):
        ref = self.idToWriteRsp[trans.hid].get_nowait()
        trans.assertEqualRef(ref)
        self.counter += 1

    def onReadRsp(self, trans):
        ref = self.idToReadRsp[trans.hid].get_nowait()
        trans.assertEqualRef(ref)
        if trans.last == 1:
            self.counter += 1

@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(simulationSpeedPrinter(dut.clk))

    axi = Axi4Shared(dut, "io_axi")

    checker = Checker(axi,dut)

    # Run until completion
    while True:
        yield RisingEdge(dut.clk)
        if checker.counter > 1000:
            break

    yield Timer(1000*10)

    dut.log.info("Cocotb test done")
