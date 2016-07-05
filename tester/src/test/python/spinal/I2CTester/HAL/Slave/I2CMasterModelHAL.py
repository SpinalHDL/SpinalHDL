import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, FallingEdge, Event


###############################################################################
# I2C Master Hal model
class I2CMasterModelHAL:


    ##########################################################################
    # Construcot
    def __init__(self, clock, wr_scl, wr_sda, rd_scl, rd_sda, clockDivider):

        self.wr_scl       = wr_scl
        self.wr_sda       = wr_sda

        self.rd_scl    = rd_scl
        self.rd_sda    = rd_sda
        self.sda       = 1
        self.scl       = 1

        self.clockDivider = clockDivider

        self.scl_en       = 0
        self.clk          = clock

        self.trigger     = Event()
        self.sclRising   = Event()
        self.sclFalling  = Event()

        self.dataRead    = Event()

        self.freezeBus   = False


    ##########################################################################
    # Start the master
    def startMaster(self):
        cocotb.fork(self._genSCL())
        cocotb.fork(self._manageOpenDrain())


    def writeTest(self, data2Write):
        cocotb.fork(self._runWrite(data2Write))

    def readTest(self, data2Read):
        cocotb.fork(self._runRead(data2Read))


    ##########################################################################
    # Execute : START - READ* - STOP
    @cocotb.coroutine
    def _runRead(self,data2Write):

        yield(self._genStart())

        for data in data2Write:
            yield self.readData()

        yield(self._genStop())



    ##########################################################################
    # Execute : START - WRITE* - STOP
    @cocotb.coroutine
    def _runWrite(self,data2Write):

        yield(self._genStart())

        for data in data2Write:

            yield self._writeData(data)

        yield self.sclFalling.wait()
        yield(self._genStop())


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

        data2Send = bin(data)[2:].zfill(8)
        write     = True
        index     = 0

        while write:

            yield FallingEdge(self.wr_scl)

            self.sda = int(data2Send[index])

            if index == 7:

                yield FallingEdge(self.wr_scl)
                self.sda = 1

                break

            index += 1

    ##########################################################################
    # Read a data
    @cocotb.coroutine
    def readData(self):
        cnt  = 0
        dataRead = list()
        while True:
            if (cnt == 8):
                yield self.sclFalling.wait()
                self.sda = 0
                yield self.sclRising.wait()
                dataInt = int("".join([str(x) for x in dataRead]), 2)
                self.dataRead.set(data= dataInt )
                yield self.sclFalling.wait()
                self.sda = 1

                break
            else:
                yield self.sclRising.wait()
                dataRead.append( int(self.rd_sda) )

            cnt += 1


