import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, FallingEdge, Event

from spinal.I2CTester.HAL.I2CHAL import *

###############################################################################
# I2C Master Hal model
class I2CMasterModelHAL:


    def __init__(self, helperSlave, clockDivider):

        self.wr_scl   = helperSlave.io.scl_rd
        self.wr_sda   = helperSlave.io.sda_rd
        self.rd_scl   = helperSlave.io.scl_wr
        self.rd_sda   = helperSlave.io.sda_wr
        self.clk      = helperSlave.io.clk

        self.sda       = 1
        self.scl       = 1

        self.clockDivider = clockDivider

        self.scl_en       = 0

        self.trigger     = Event()
        self.sclRising   = Event()
        self.sclFalling  = Event()

        self.dataRead    = Event()

        self.freezeBus   = False


    ##########################################################################
    # Start the master
    @cocotb.coroutine
    def startMaster(self, listOperations):

        yield RisingEdge(self.clk)

        self.fork_scl    = cocotb.fork(self._genSCL())
        self.fork_drain  = cocotb.fork(self._manageOpenDrain())
        cocotb.fork(self._runMaster(listOperations))


    ##########################################################################
    # Stop all processes
    def stop(self):

        self.fork_scl.kill()
        self.fork_drain.kill()


    ##########################################################################
    # Execute all operation
    @cocotb.coroutine
    def _runMaster(self, listOperations):

        for index in range(0,len(listOperations)):

            operation = listOperations[index]

            # START -----------------------------------------------------------
            if isinstance(operation, START):
                if index != 0 :
                    yield self.sclRising.wait()

                yield self._genStart()

            # WRITE -----------------------------------------------------------
            elif isinstance(operation, WRITE):
                yield  self._writeData(operation.data)

            # READ ------------------------------------------------------------
            elif isinstance(operation, READ):
                yield self._readData()

            # ACK -------------------------------------------------------------
            elif isinstance(operation, ACK):
                prevOperation = listOperations[index-1]

                yield self.sclFalling.wait()
                self.sda = 1 if isinstance(prevOperation, WRITE) else 0
                yield self.sclRising.wait()
                yield self.sclFalling.wait()
                self.sda = 1

            # NACK ------------------------------------------------------------
            elif isinstance(operation, NACK):

                yield self.sclFalling.wait()
                self.sda  = 1
                yield self.sclRising.wait()
                yield self.sclFalling.wait()

            # STOP ------------------------------------------------------------
            elif isinstance(operation, STOP):
                yield self._genStop()




    ##########################################################################
    # Simulation of the openDrain
    @cocotb.coroutine
    def _manageOpenDrain(self):
        while True:
            yield RisingEdge(self.clk)

            if int(self.rd_sda) == 0 :
                self.wr_sda <= 0
            else:
                self.wr_sda <= self.sda

            if int(self.rd_scl) == 0 :
                self.wr_scl <= 0
            else:
                self.wr_scl <= self.scl


    ##########################################################################
    # SCL generation
    @cocotb.coroutine
    def _genSCL(self):
        cnt = 0
        self.scl = 1
        while True:

            yield RisingEdge(self.clk)

            if self.scl_en == 1 and self.freezeBus == False:
                cnt += 1
                if (cnt >= self.clockDivider):
                    if self.scl == 0:
                        self.scl = 1
                        self.sclRising.set()

                    else:
                        self.scl = 0
                        self.sclFalling.set()

                    cnt = 0

                elif(cnt == self.clockDivider/2):
                    self.trigger.set()
            else:
                self.scl <= 1


    ##########################################################################
    # Generate the start condition
    @cocotb.coroutine
    def _genStart(self):

        self.scl_en = 1

        yield self.trigger.wait()

        self.sda = 0

        yield self.sclFalling.wait()

        self.sda = 1


    ##########################################################################
    # Generate the stop condition
    @cocotb.coroutine
    def _genStop(self):

        self.sda = 0

        yield self.sclRising.wait()
        yield self.trigger.wait()

        self.sda    = 1
        self.scl_en = 0


    ##########################################################################
    # Write a data
    @cocotb.coroutine
    def _writeData(self, data):

        data2Send = bin(data)[2:].zfill(I2CConfig.dataWdith)
        write     = True
        index     = 0

        while write:

            yield FallingEdge(self.wr_scl)

            self.sda = int(data2Send[index])

            if index == I2CConfig.dataWdith-1:
                break

            index += 1

    ##########################################################################
    # Read a data
    @cocotb.coroutine
    def _readData(self):
        cnt  = 0
        dataRead = list()
        while True:

            if (cnt == I2CConfig.dataWdith):
                dataInt = int("".join([str(x) for x in dataRead]), 2)
                self.dataRead.set(data= dataInt )

                break
            else:
                yield self.sclRising.wait()
                dataRead.append( int(self.rd_sda) )

            cnt += 1


