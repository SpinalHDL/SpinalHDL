import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, FallingEdge, Event

class I2CSlaveModelHALState:
    IDLE    = 0
    WRITE   = 1
    READ    = 2
    NONE    = 3




class I2CSlaveModelHAL:


    def __init__(self, _clock, _resetn,  _wr_sda, _rd_sda, _wr_scl ): # initMemory = dict()):

        self.startEvent  = Event()
        self.stopEvent   = Event()
       # self.dataRxEvent = Event()
        self.dataTXEvent = Event()
        self.sda_wr      = _wr_sda
        self.sda_rd      = _rd_sda
        self.scl_wr      = _wr_scl
       # self.scl_rd      = _rd_scl
        #self.modeAddr    = addrMode   # 7 = 7 bits , 10 = 10 bits
        #self.mem         = initMemory # Create a dictionnary (address, value)
        #self.addr        = 0
        self.resetn      = _resetn
        self.clk         = _clock
        self.state       = I2CSlaveModelHALState.NONE
        self.nextState   = I2CSlaveModelHALState.NONE


        self.forceSDA    = 1
        #self.startSlave()

    def startSlave(self):

        cocotb.fork(self._startDetection())
        cocotb.fork(self._stopDetection())
     #   cocotb.fork(self._stateMachine())
        cocotb.fork(self._manageSDA())

    def setNextState(self, mode):
        self.nextState = mode

    @cocotb.coroutine
    def _stateMachine(self):

        while True:

            if self.state == I2CSlaveModelHALState.NONE:
                yield RisingEdge(self.clk)


            elif self.state == I2CSlaveModelHALState.IDLE:

            #    yield self.startEvent.wait()
                self.state =  self.nextState

            elif self.state == I2CSlaveModelHALState.READ:

                self.state = self.nextState

                yield self._readData()

                yield FallingEdge(self.scl_wr)
                self.forceSDA = 1





    @cocotb.coroutine
    def _manageSDA(self):
        while True:
            yield Edge(self.clk)
            if self.forceSDA == 0:
                self.sda_rd <= 0
            else:
                self.sda_rd <= int(self.sda_wr)


    @cocotb.coroutine
    def _startDetection(self):
        while True:
            yield FallingEdge(self.sda_wr)
            if int(self.scl_wr) == 1:
                print("Start detected ...")
                self.state = I2CSlaveModelHALState.IDLE
                self.startEvent.set()



    @cocotb.coroutine
    def _stopDetection(self):
        while True:
            yield RisingEdge(self.sda_wr)
            if int(self.scl_wr) == 1:
                print("Stop detected  ...")
                self.stopEvent.set()
               # self.state = I2CSlaveState.IDLE



    @cocotb.coroutine
    def _readData(self):
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


    @cocotb.coroutine
    def _writeData(self, data):
        print("write data")
        write = True
        cnt   = 0
        data2Send = bin(data)[2:].zfill(8)
        while write:
            yield FallingEdge(self.scl)
            self.forceSDA = int(data2Send[cnt])

            if (cnt == 7):
                yield RisingEdge(self.scl)
                yield RisingEdge(self.scl)
                # read ACK
                result = dict()
                result["data"] = data2Send
                result["ack"]  = int(self.sda_wr)

                self.dataRxEvent.set(result)
                break

            cnt += 1

