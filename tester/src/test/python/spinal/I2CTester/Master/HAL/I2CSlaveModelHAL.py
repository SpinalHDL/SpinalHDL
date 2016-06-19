import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, FallingEdge, Event


###############################################################################
# I2C Slave Hal model
class I2CSlaveModelHAL:

    ##########################################################################
    # Construcot
    def __init__(self, _clock, _resetn,  _wr_sda, _rd_sda, _wr_scl ): # initMemory = dict()):

        self.startEvent  = Event()
        self.stopEvent   = Event()
        self.dataRxEvent = Event()
        self.dataTXEvent = Event()
        self.sda_wr      = _wr_sda
        self.sda_rd      = _rd_sda
        self.scl_wr      = _wr_scl
       # self.scl_rd     = _rd_scl
        self.resetn      = _resetn
        self.clk         = _clock
        self.forceSDA    = 1

    ##########################################################################
    # Launch all slave process
    def startSlave(self):

        cocotb.fork(self._startDetection())
        cocotb.fork(self._stopDetection())
        cocotb.fork(self._manageSDA())


    ##########################################################################
    # Simulate an open drain pin
    @cocotb.coroutine
    def _manageSDA(self):
        while True:
            yield Edge(self.clk)
            if self.forceSDA == 0:
                self.sda_rd <= 0
            else:
                self.sda_rd <= int(self.sda_wr)

    ##########################################################################
    # Detect the start/restart sequence
    @cocotb.coroutine
    def _startDetection(self):
        while True:
            yield FallingEdge(self.sda_wr)
            if int(self.scl_wr) == 1:
                print("Start detected ...")
                self.startEvent.set()

    ##########################################################################
    # Detect the stop sequence
    @cocotb.coroutine
    def _stopDetection(self):
        while True:
            yield RisingEdge(self.sda_wr)
            if int(self.scl_wr) == 1:
                print("Stop detected  ...")
                self.stopEvent.set()

    ##########################################################################
    # Read a data comming from the master
    @cocotb.coroutine
    def readData(self):
        cnt  = 0
        dataRead = list()
        while True:
            if (cnt == 8):
                yield FallingEdge(self.scl_wr)
                self.forceSDA = 0
                yield RisingEdge(self.scl_wr)
                dataInt = int("".join([str(x) for x in dataRead]), 2)
                self.dataTXEvent.set(data= dataInt )
                yield FallingEdge(self.scl_wr)
                self.forceSDA = 1

                break
            else:
                yield RisingEdge(self.scl_wr)
                dataRead.append( int(self.sda_wr) )

            cnt += 1

    ##########################################################################
    # Write a data to the master
    @cocotb.coroutine
    def writeData(self, data):

        write = True
        cnt   = 0
        data2Send = bin(data)[2:].zfill(8)

        while write:
            yield FallingEdge(self.scl_wr)
            self.forceSDA = int(data2Send[cnt])

            if (cnt == 7):

                result = dict()
                result["data"] = int("".join([str(x) for x in data2Send]), 2)
                result["ack"]  = int(self.sda_wr)

                self.dataRxEvent.set(result)

                yield FallingEdge(self.scl_wr)
                self.forceSDA = 1


                break

            cnt += 1

