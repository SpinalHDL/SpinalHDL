import random
from queue import Queue

import cocotb
from cocotb import fork, log
from cocotb.decorators import coroutine
from cocotb.triggers import RisingEdge, FallingEdge, Event, Timer

from cocotblib.Flow import Flow
from cocotblib.Stream import Stream, StreamDriverMaster, Transaction
from cocotblib.misc import assertEquals, randInt, ClockDomainAsyncReset, simulationSpeedPrinter, clockedWaitTrue, Bundle, randBits, randBool
from spinal.I2CTester2.lib.misc import OpenDrainInterconnect, I2cSoftMaster


cmdToData = [randBits(8) for x in range(256)]

crapyConflictCounter = 0
normalConflictCounter = 0
normalTransactionCounter = 0

@coroutine
def clockedFuncWaitTrue(clk,that):
    while True:
        yield RisingEdge(clk)
        if that() == True:
            break

@coroutine
def SlaveThread(scl,sda,clk,baudPeriod):
    global crapyConflictCounter
    global normalConflictCounter
    global normalTransactionCounter
    log.debug("x")
    IDLE = 0
    START = 1
    DATA = 2
    state = IDLE
    dataState = 0
    scl.write(True)
    sda.write(True)

    sclLast = True
    sdaLast = True
    while True:
        yield RisingEdge(clk)
        sclValue = scl.read()
        sdaValue = sda.read()
        sclRising  =     sclValue and not sclLast
        sclFalling = not sclValue and     sclLast
        sdaRising  =     sdaValue and not sdaLast
        sdaFalling = not sdaValue and     sdaLast
        sclLast = sclValue
        sdaLast = sdaValue
        if state == IDLE:
            if sdaFalling and sclValue:
                state = START
        elif state == START:
            if sclFalling:
                state = DATA
                dataState = 0
                address = 0
        elif state == DATA:
            if sclRising:
                if dataState < 8:
                    address |= sdaValue << (7-dataState)
            elif sclFalling:
                dataState += 1
                if dataState >= 8 and dataState < 16:
                    if random.random() < 0.2: #Clock stretching
                        scl.write(False)
                        yield Timer(randInt(baudPeriod/10, baudPeriod*10))
                        sda.write((cmdToData[address] >> (15-dataState)) & 1)
                        yield Timer(baudPeriod/4);
                        scl.write(True)
                        yield clockedFuncWaitTrue(clk, scl.read)
                        sclLast = False
                    else:
                        sda.write((cmdToData[address] >> (15 - dataState)) & 1)
                elif(dataState == 6 and random.random() < 0.2):
                    scl.write(False)
                    if random.random() < 0.2:  # Clock stretching
                        yield Timer(randInt(baudPeriod / 10, baudPeriod * 10))
                    yield Timer(baudPeriod / 2)
                    sda.write(False)
                    yield Timer(baudPeriod / 2)
                    scl.write(True)
                    yield clockedFuncWaitTrue(clk, scl.read)
                    yield Timer(baudPeriod)

                    if random.random() < 0.5:
                        #Normal conflict
                        normalConflictCounter += 1
                        for i in range(4):
                            rand = randBool()
                            scl.write(False)
                            yield Timer(baudPeriod/2)
                            sda.write(rand)
                            yield Timer(baudPeriod/2)
                            scl.write(True)
                            yield clockedFuncWaitTrue(clk, scl.read)
                            yield Timer(baudPeriod)
                            assert sda.read() == rand

                        scl.write(False)
                        yield Timer(baudPeriod / 2)
                        sda.write(False)
                        yield Timer(baudPeriod / 2)
                        scl.write(True)
                        yield clockedFuncWaitTrue(clk, scl.read)
                        yield Timer(baudPeriod)
                        sda.write(True)
                    else:
                        #crapy conflict wihtout STOP
                        crapyConflictCounter += 1
                        scl.write(False)
                        yield Timer(baudPeriod / 2)
                        sda.write(True)
                        yield Timer(baudPeriod / 2)
                        scl.write(True)
                        yield clockedFuncWaitTrue(clk, scl.read)
                        yield Timer(baudPeriod)
                    state = IDLE
                    continue
                else:
                    if random.random() < 0.2: #Clock stretching
                        scl.write(False)
                        yield Timer(randInt(baudPeriod/10, baudPeriod*10))
                        sda.write(True)
                        yield Timer(baudPeriod/4);
                        scl.write(True)
                        sclLast = False
                    else:
                        sda.write(True)
            elif sclValue:
                if sdaRising:
                    state = 0
                if sdaFalling:
                    state = 1
                    pass





class MasterThread:
    def __init__(self, cmd, rsp, clk, reset, baudPeriod,softMaster):
        self.cmd = cmd
        self.rsp = rsp
        self.clk = clk
        self.baudPeriod = baudPeriod
        self.softMaster = softMaster
        self.cmdQueue = []
        self.cmdDriver = StreamDriverMaster(cmd,lambda  : self.cmdQueue.pop(0) if self.cmdQueue and (random.random() < (1.0/10.0)) else None,clk,reset)



    @coroutine
    def run(self):
        global crapyConflictCounter
        global normalConflictCounter
        global normalTransactionCounter
        yield Timer(self.baudPeriod * 10)

        while crapyConflictCounter < 2 or normalConflictCounter < 3 or normalTransactionCounter < 40:
            while True:
                colision = False

                cmd = Transaction()
                cmd.mode = 0
                cmd.data = randBool()
                self.cmdQueue.append(cmd)

                address = randBits(8) | 2
                for bitId in range(8):
                    cmd = Transaction()
                    cmd.mode = 1
                    cmd.data = (address >> (7 - bitId)) & 1
                    self.cmdQueue.append(cmd)
                    yield clockedWaitTrue(self.clk,self.rsp.valid)

                    if self.rsp.payload.data != cmd.data:
                        assert bitId == 6
                        colision = True
                        cmd = Transaction()
                        cmd.mode = 3 #DROP
                        cmd.data = randBool()
                        self.cmdQueue.append(cmd)
                        break
                if colision:
                    continue

                for bitId in range(8):
                    cmd = Transaction()
                    cmd.mode = 1
                    cmd.data = True
                    self.cmdQueue.append(cmd)
                    yield clockedWaitTrue(self.clk,self.rsp.valid)
                    assert self.rsp.payload.data == ((cmdToData[address] >> (7-bitId)) & 1)

                if random.random() < 0.75:
                    cmd = Transaction()
                    cmd.mode = 2
                    cmd.data = randBool()
                    self.cmdQueue.append(cmd)
                    if random.random() < 0.75: #no other master frame
                        if random.random() < 0.5: # With inter frame delay
                            yield Timer(randInt(0,self.baudPeriod*20))
                    else:
                        @coroutine
                        def anotherFrameEmiter():
                            yield self.softMaster.sendStart()
                            for i in range(5):
                                yield self.softMaster.sendBit(randBool())
                            yield self.softMaster.sendStop()

                        yield Timer(randInt(self.baudPeriod * 4, self.baudPeriod * 10))
                        fork(anotherFrameEmiter())
                        yield Timer(randInt(self.baudPeriod * 1, self.baudPeriod * 14))
                normalTransactionCounter += 1
                break


        while self.cmdQueue:
            yield Timer(self.baudPeriod * 10)


@cocotb.test()
def test1(dut):
    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset,100000))
    cocotb.fork(simulationSpeedPrinter(dut.clk))

    baudPeriod = 2500000
    sclInterconnect = OpenDrainInterconnect()
    sclInterconnect.addHardDriver(dut.io_i2c_scl_write)
    sclInterconnect.addHardReader(dut.io_i2c_scl_read)

    sdaInterconnect = OpenDrainInterconnect()
    sdaInterconnect.addHardDriver(dut.io_i2c_sda_write)
    sdaInterconnect.addHardReader(dut.io_i2c_sda_read)

    dut.io_config_samplingClockDivider <= 3
    dut.io_config_timerClockDivider <= 24

    softMaster = I2cSoftMaster(sclInterconnect.newSoftConnection(), sdaInterconnect.newSoftConnection(), baudPeriod,dut.clk)
    slaveThread  = fork(SlaveThread(sclInterconnect.newSoftConnection(), sdaInterconnect.newSoftConnection(),dut.clk,baudPeriod))
    masterThread = fork(MasterThread(Stream(dut,"io_cmd"),Flow(dut,"io_rsp"), dut.clk, dut.reset, baudPeriod,softMaster).run())

    yield masterThread.join()
