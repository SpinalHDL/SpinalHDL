import random
from Queue import Queue

import cocotb
from cocotb.triggers import RisingEdge

from spinal.common.Phase import PHASE_SIM, Infrastructure
from spinal.common.Stream import Stream, Transaction, StreamDriverSlave, StreamDriverMaster, StreamMonitor, StreamScorboardOutOfOrder
from spinal.common.misc import BoolRandomizer, log2Up, randBits


class Axi4:
    def __init__(self,dut,name):
        self.ar = Stream(dut,name + "_ar")
        self.r  = Stream(dut, name + "_r")
        self.aw = Stream(dut, name + "_aw")
        self.w  = Stream(dut, name + "_w")
        self.b  = Stream(dut, name + "_b")

class Axi4ReadOnly:
    def __init__(self,dut,name):
        self.ar = Stream(dut,name + "_ar")
        self.r  = Stream(dut, name + "_r")

class Axi4WriteOnly:
    def __init__(self,dut,name):
        self.aw = Stream(dut, name + "_aw")
        self.w  = Stream(dut, name + "_w")
        self.b  = Stream(dut, name + "_b")

class Axi4Shared:
    def __init__(self,dut,name):
        self.arw = Stream(dut,name + "_arw")
        self.r  = Stream(dut, name + "_r")
        self.w  = Stream(dut, name + "_w")
        self.b  = Stream(dut, name + "_b")


def Axi4AddrIncr(address,burst,len,size):
    if burst == 0:
        return address
    if burst == 1:
        return address + (1 << size)
    if burst == 2:
        burstSize = (1 << size) * (len+1)
        burstMask = burstSize-1
        base = (address + (1 << size)) & burstMask
        return (address & ~burstMask) | base



class Axi4SharedMemoryChecker(Infrastructure):
    def __init__(self,name,parent,axi,addressWidth,clk,reset):
        Infrastructure.__init__(self,name,parent)
        self.axi = axi
        self.idWidth = len(axi.arw.payload.hid)
        self.addressWidth = addressWidth
        self.ram = bytearray(b'\x00' * ((1 << addressWidth)*len(axi.w.payload.data)/8))
        self.doReadWriteCmdRand = BoolRandomizer()
        self.readWriteRand = BoolRandomizer()
        self.writeDataRand = BoolRandomizer()
        self.writeRspScoreboard = StreamScorboardOutOfOrder("writeRspScoreboard",self)
        self.readRspScoreboard  = StreamScorboardOutOfOrder("readRspScoreboard", self)
        self.writeRspScoreboard.addListener(self.freeReservatedAddresses)
        self.readRspScoreboard.addListener(self.freeReservatedAddresses)
        self.cmdTasks   =  Queue()
        self.writeTasks =  Queue()
        self.nonZeroReadRspCounter = 0
        self.nonZeroReadRspCounterTarget = 1000
        self.reservedAddresses = {}
        self.dataWidth = len(axi.w.payload.data)
        StreamDriverSlave(axi.r, clk, reset)
        StreamDriverSlave(axi.b, clk, reset)
        StreamDriverMaster(axi.arw, self.genReadWriteCmd, clk, reset)
        StreamDriverMaster(axi.w, self.genWriteData, clk, reset)
        StreamMonitor(axi.r, self.onReadRsp, clk, reset)
        StreamMonitor(axi.b, self.onWriteRsp, clk, reset)
        axi.w.payload.last <= 0
        axi.r.payload.last <= 0

    def freeReservatedAddresses(self,uut,ref,equal):
        self.reservedAddresses.pop(ref,None)

    def isAddressRangeBusy(self,start,end):
        for r in self.reservedAddresses.itervalues():
            if start < r[1] and end > r[0]:
                return True
        return False

    def genRandomeAddress(self):
        return randBits(self.addressWidth)

    def genNewCmd(self):
        cmd = Transaction()
        cmd.hid = randBits(self.idWidth)  # Each master can use 4 id
        cmd.region = randBits(4)
        cmd.len = randBits(4)
        cmd.size = random.randint(0,log2Up(self.dataWidth/8))
        cmd.burst = random.randint(0,2)
        if cmd.burst == 2:
            cmd.len = random.choice([2,4,8,16])-1
        else:
            cmd.len = randBits(4) + (16 if random.random() < 0.1 else 0) + (32 if random.random() < 0.02 else 0)
        cmd.lock = randBits(1)
        cmd.cache = randBits(4)
        cmd.qos = randBits(4)
        cmd.prot = randBits(3)

        byteCount = (1 << cmd.size)*(cmd.len + 1)
        while(True):
            cmd.addr  = self.genRandomeAddress() & ~((1 << cmd.size)-1)
            if cmd.burst == 1:
                if cmd.addr + byteCount >= (1<<self.addressWidth):
                    continue
            if cmd.burst == 0:
                start = cmd.addr
                end   = start + cmd.size

            if cmd.burst == 1:
                start = cmd.addr
                end = start + byteCount

            if cmd.burst == 2:
                start = cmd.addr & ~(byteCount-1)
                end = start + byteCount

            if self.isAddressRangeBusy(start,end):
                continue
            break

        if self.readWriteRand.get():
            cmd.write = 1
            beatAddr = cmd.addr
            for i in xrange(cmd.len+1):
                dataTrans = Transaction()
                dataTrans.data = randBits(self.dataWidth)
                dataTrans.strb = randBits(self.dataWidth/8)
                dataTrans.last = 1 if cmd.len == i else 0
                self.writeTasks.put(dataTrans)

                for s in xrange(self.dataWidth/8):
                    if (dataTrans.strb >> s) & 1 == 1:
                        self.ram[(beatAddr & ~(self.dataWidth/8-1)) + s] = (dataTrans.data >> (s*8)) & 0xFF
                beatAddr = Axi4AddrIncr(beatAddr,cmd.burst,cmd.len,cmd.size)

            writeRsp = Transaction()
            writeRsp.resp = 0
            writeRsp.hid = cmd.hid

            self.reservedAddresses[writeRsp] = [start,end]
            self.writeRspScoreboard.refPush(writeRsp,writeRsp.hid)
        else:
            cmd.write = 0

            beatAddr = cmd.addr
            for s in xrange(cmd.len + 1):
                readRsp = Transaction()
                addrBase = beatAddr & ~(self.dataWidth/8-1)
                readRsp.data = 0
                for i in xrange(self.dataWidth / 8):
                    readRsp.data |= self.ram[addrBase + i] << (i*8)
                readRsp.resp = 0
                readRsp.last = 1 if cmd.len == s else 0
                readRsp.hid = cmd.hid
                if readRsp.last == 1:
                    self.reservedAddresses[readRsp] = [start, end]
                self.readRspScoreboard.refPush(readRsp, readRsp.hid)
                beatAddr = Axi4AddrIncr(beatAddr, cmd.burst, cmd.len, cmd.size)

        self.cmdTasks.put(cmd)
        # print(str(len(self.cmdTasks.queue)) + " " + str(len(self.writeTasks.queue)))


    def genReadWriteCmd(self):
        if self.doReadWriteCmdRand.get():
            while self.cmdTasks.empty():
                if self.getPhase() != PHASE_SIM:
                    return None
                self.genNewCmd()
            return self.cmdTasks.get()

    def genWriteData(self):
        if self.writeDataRand.get():
            while self.writeTasks.empty():
                if self.getPhase() != PHASE_SIM:
                    return None
                self.genNewCmd()
            return self.writeTasks.get()

    def onWriteRsp(self,trans):
        self.writeRspScoreboard.uutPush(trans,trans.hid)

    def onReadRsp(self, trans):
        self.readRspScoreboard.uutPush(trans, trans.hid)
        if trans.data != 0:
            self.nonZeroReadRspCounter += 1
            if self.nonZeroReadRspCounter % 50 == 0:
                print("progress=" + str(self.nonZeroReadRspCounter))

    # override
    def hasEnoughSim(self):
        return self.nonZeroReadRspCounter > self.nonZeroReadRspCounterTarget