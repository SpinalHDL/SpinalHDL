from queue import Queue

import random
from cocotblib.Stream import Stream, StreamDriverMaster, StreamMonitor, Transaction, StreamDriverSlave
from cocotblib.misc import BoolRandomizer, randBits
from cocotb.result import TestFailure, TestSuccess

class Bmb:
    def __init__(self,dut,name):
        self.cmd = Stream(dut, name + "_cmd")
        self.rsp = Stream(dut, name + "_rsp")

class Memory:
    def __init__(self, size):
        self.ram = bytearray(size)

    def read(self, address, size):
        data = 0
        trigger = False
        for i in range(size):
            data |= self.ram[address + i] << i*8
        if trigger:
            print(hex(data))
        return data


    def write(self, address, size, data, mask):
        for i in range(size):
            if mask & 1:
                self.ram[address + i] = data & 0xFF
            mask >>= 1
            data >>= 8

class BmbMemoryTester:
    def __init__(self, bmbs, addressRange, lengthMin, lengthMaxAll, clk, reset, doWriteInit = False):
        self.run = False
        self.hold = len(bmbs)
        self.ram = Memory(addressRange)
        self.bmbs = bmbs
        self.progress = [0 for x in bmbs]
        self.doWriteInit = doWriteInit
        self.writeInitAddress = 0

        for bmbId,bmb in enumerate(bmbs):
            self.initPort( bmbId,bmb, addressRange, lengthMin, lengthMaxAll, clk, reset)


    def initPort(self, bmbId, bmb, addressRange, lengthMin, lengthMaxAll, clk, reset):
        lengthMax = min(1 << len(bmb.cmd.payload.fragment_length), lengthMaxAll)
        doCmdRand = BoolRandomizer()
        doCmdRand.probLow = 0.1
        doCmdRand.probHigh = 0.5
        cmdTasks = Queue()
        rspTasks = Queue()
        bytePerBeat = len(bmb.cmd.payload.fragment_data) / 8

        def genNewCmd():
            length = random.randint(1, (lengthMax // lengthMin)) * lengthMin
            if self.doWriteInit:
                address = self.writeInitAddress
                self.writeInitAddress += length
            else:
                address = (random.randint(0, addressRange // lengthMaxAll // len(self.bmbs)-1)*len(self.bmbs) + bmbId) * lengthMaxAll
            write = random.random() < 0.5 or self.doWriteInit
            beatCount = (length + bytePerBeat-1) // bytePerBeat

            context = randBits(len(bmb.cmd.payload.fragment_context))
            print("* " + str(context) + " " + str(length) + " " + str(bytePerBeat) + " " + str(beatCount))
            source = 0
            if not write:
                cmd = Transaction()
                cmd.last = True
                cmd.fragment_source = source
                cmd.fragment_opcode = 0
                cmd.fragment_address = address
                cmd.fragment_length = length-1
                cmd.fragment_data = randBits(len(bmb.cmd.payload.fragment_data))
                cmd.fragment_mask = randBits(len(bmb.cmd.payload.fragment_mask))
                cmd.fragment_context = context
                cmdTasks.put(cmd)
                # print("***  R" + str(bmbId) + " " + hex(cmd.fragment_context))

                for beat in range(beatCount):
                    rsp = Transaction()
                    rsp.last = beat == beatCount - 1
                    rsp.fragment_source = source
                    rsp.fragment_context = context
                    rsp.fragment_opcode = 0
                    rsp.fragment_data = self.ram.read(address + beat * bytePerBeat, bytePerBeat)
                    # print("***   " + str(bmbId) + " " + hex(rsp.fragment_context))
                    rspTasks.put(rsp)
            else:
                rsp = Transaction()
                rsp.last = True
                rsp.fragment_source = source
                rsp.fragment_context = context
                rsp.fragment_opcode = 0
                rsp.fragment_data = None
                rspTasks.put(rsp)
                # print("***   " + str(bmbId) + " " + hex(rsp.fragment_context))

                for beat in range(beatCount):
                    cmd = Transaction()
                    cmd.last = beat == beatCount - 1
                    cmd.fragment_opcode = 1
                    cmd.fragment_address = address
                    cmd.fragment_length = length-1
                    cmd.fragment_source = source
                    cmd.fragment_context = context
                    cmd.fragment_data = randBits(len(bmb.cmd.payload.fragment_data))
                    if self.doWriteInit:
                        cmd.fragment_mask = (1 << len(bmb.cmd.payload.fragment_mask)) - 1
                    else:
                        cmd.fragment_mask = randBits(len(bmb.cmd.payload.fragment_mask))
                    cmdTasks.put(cmd)
                    # print("***  W " + str(bmbId) + " " + hex(cmd.fragment_context) + " " + str(length))

                    self.ram.write(address + beat * bytePerBeat, bytePerBeat, cmd.fragment_data, cmd.fragment_mask)

            if self.writeInitAddress >= addressRange:
                self.writeInitAddress = False

        def createCmd():
            if doCmdRand.get():
                while cmdTasks.empty():
                    if not self.run:
                        return None
                    genNewCmd()
                return cmdTasks.get()


        def checkRsp(trans):
            assert not rspTasks.empty()
            trans.assertEqualRef(rspTasks.get())
            if trans.last:
                self.progress[bmbId] += 1
                if self.progress[bmbId] == 10000:
                    self.hold -= 1
                    if self.hold == 0:
                        raise TestSuccess()

        StreamDriverMaster(bmb.cmd, createCmd, clk, reset)
        StreamMonitor(bmb.rsp, checkRsp, clk, reset)
        StreamDriverSlave(bmb.rsp, clk, reset).randomizer.probLow = 0.5

