import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, FallingEdge, Event

from spinal.I2CTester.HAL.I2CHAL import *


###############################################################################
# I2C Slave Hal model
class I2CSlaveModelHAL:


    def __init__(self, helperMaster ): # initMemory = dict()):

        self.startEvent  = Event()
        self.stopEvent   = Event()
        self.dataRxEvent = Event()
        self.dataTXEvent = Event()

        self.sda_rd      = helperMaster.io.sda_wr
        self.sda_wr      = helperMaster.io.sda_rd
        self.scl_rd      = helperMaster.io.scl_wr
        self.scl_wr      = helperMaster.io.scl_rd

        self.resetn      = helperMaster.io.resetn
        self.clk         = helperMaster.io.clk

        self.sda         = 1
        self.scl         = 1


    ##########################################################################
    # Launch all slave process
    @cocotb.coroutine
    def startSlave(self,listOperations):

        yield RisingEdge(self.clk)

        cocotb.fork(self._manageSDA())
        cocotb.fork(self._startDetection())
        cocotb.fork(self._stopDetection())
        cocotb.fork(self._runSlave(listOperations))


    ##########################################################################
    # Slave simulation
    @cocotb.coroutine
    def _runSlave(self, listOperations):

        for index in range(0,len(listOperations)):

            operation = listOperations[index]

            if isinstance(operation, START):
                if index != 0:
                    yield FallingEdge(self.scl_wr)
                    yield FallingEdge(self.scl_wr)
                else:
                    yield FallingEdge(self.scl_wr)


            elif isinstance(operation, WRITE):
                yield self._writeData()

            elif isinstance(operation, READ):
                yield self._readData(operation.data)

            elif isinstance(operation, ACK):
                prevOperation = listOperations[index-1]

                self.sda = 0 if isinstance(prevOperation, WRITE) else 1
                yield FallingEdge(self.scl_rd)
                self.sda = 1

            elif isinstance(operation, ACK):

                self.sda = 1
                yield FallingEdge(self.scl_wr)
                self.sda = 1

            elif isinstance(operation, STOP):
                pass





    ##########################################################################
    # Simulate an open drain pin
    @cocotb.coroutine
    def _manageSDA(self):
        while True:
            yield RisingEdge(self.clk)

            if int(self.sda_rd) == 0:
                self.sda_wr <= 0
            else:
                self.sda_wr <= self.sda


            if int(self.scl_rd) == 0 :
                self.scl_wr <= 0
            else:
                self.scl_wr <= self.scl



    ##########################################################################
    # Detect the start/restart sequence
    @cocotb.coroutine
    def _startDetection(self):
        while True:
            yield RisingEdge(self.clk)
            prev = int(self.sda_rd)
            yield RisingEdge(self.clk)
            if prev == 1 and int(self.sda_rd) == 0:
                if int(self.scl_rd) == 1:
                    self.startEvent.set()


    ##########################################################################
    # Detect the stop sequence
    @cocotb.coroutine
    def _stopDetection(self):
        while True:
            yield RisingEdge(self.clk)
            prev = int(self.sda_rd)
            yield RisingEdge(self.clk)
            if prev == 0 and int(self.sda_rd) == 1:
                if int(self.scl_rd) == 1:
                    self.stopEvent.set()


    ##########################################################################
    # Read a data comming from the master
    @cocotb.coroutine
    def _writeData(self):
        cnt  = 0
        dataRead = list()
        while True:
            if (cnt == 8):
                yield FallingEdge(self.scl_wr)
                dataInt = int("".join([str(x) for x in dataRead]), 2)
                self.dataTXEvent.set(data= dataInt )

                break
            else:
                yield RisingEdge(self.scl_wr)
                dataRead.append( int(self.sda_wr) )

            cnt += 1

    ##########################################################################
    # Write a data to the master
    @cocotb.coroutine
    def _readData(self, data):

        write = True
        cnt   = 0
        data2Send = bin(data)[2:].zfill(8)

        self.sda = int(data2Send[0])
        cnt += 1

        while write:
            yield FallingEdge(self.scl_rd)
            self.sda = int(data2Send[cnt])

            if (cnt == 7):

                result = dict()
                result["data"] = int("".join([str(x) for x in data2Send]), 2)
                result["ack"]  = int(self.sda_rd)

                self.dataRxEvent.set(result)

                yield FallingEdge(self.scl_rd)
                self.sda = 1


                break

            cnt += 1

