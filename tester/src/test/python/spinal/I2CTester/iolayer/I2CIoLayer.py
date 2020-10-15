import cocotb
from cocotb.triggers import RisingEdge, FallingEdge, Event

from cocotblib.misc import assertEquals, randInt


###############################################################################
# I2C - Configuration
#
class I2CConfig:
    dataWdith = 8

###############################################################################
# I2C - Analyse data passing on the i2c bus
#
class I2CIoLayerAnalyser:

    def __init__(self, helperSlave, listOperation):

        self.sda = helperSlave.io.sda_rd
        self.scl = helperSlave.io.scl_rd
        self.clk = helperSlave.io.clk

        self.event_RisingEdge  = Event()
        self.event_FallingEdge  = Event()

        self.event_Start = Event()
        self.event_Stop  = Event()

        self.listOp = list()
        self.refListOp = listOperation
        self.dataBinRead = list()

        self.startSeq = 0


    #==========================================================================
    # Start to analyse the bus
    #==========================================================================
    @cocotb.coroutine
    def start(self):

        self.fork_falling = cocotb.fork(self._FallingEdgeDetection())
        self.fork_rising  = cocotb.fork(self._RisingEdgeDetection())
        self.fork_start   = cocotb.fork(self._startDetection())
        self.fork_stop    = cocotb.fork(self._stopDetection())
        yield self._analyser()


    #==========================================================================
    # Stop all processes
    #==========================================================================
    def stop(self):
        self.fork_falling.kill()
        self.fork_rising.kill()
        self.fork_start.kill()
        self.fork_stop.kill()


    #==========================================================================
    # Store all event appening on the bus
    #==========================================================================
    @cocotb.coroutine
    def _analyser(self):

        self.listOp = list()

        # Start ---------------------------------------------------------------
        yield self.event_Start.wait()
        yield RisingEdge(self.clk)
        self.startSeq = 0

        while True:

            dataBinRead = list()
            index = 0
            # Read data -----------------------------------------------------------
            while index < I2CConfig.dataWdith:
                yield self.event_RisingEdge.wait()
                dataBinRead.append(int(self.sda))
                #print("data ", index, " value " , int(self.sda), "index ", index, "start " , self.startSeq )

                if self.startSeq == 1:
                    index = 0
                    self.startSeq = 0
                    dataBinRead = list()
                    dataBinRead.append(int(self.sda))

                index += 1

            dataInRead = int("".join([str(x) for x in dataBinRead]), 2)
            self.listOp.append(DATA(dataInRead))

            # Read ACK ------------------------------------------------------------
            yield self.event_RisingEdge.wait()
            if int(self.sda) == 0:
                self.listOp.append(ACK())
            else:
                self.listOp.append(NACK())

            #print()

    #==========================================================================
    # Detect the start condition
    #==========================================================================
    @cocotb.coroutine
    def _startDetection(self):
        yield RisingEdge(self.clk)
        while True:
            prev = int(self.sda)
            yield RisingEdge(self.clk)
            if prev == 1 and int(self.sda) == 0:
                if int(self.scl) == 1:
                    self.event_Start.set()
                    self.listOp.append(START())
                    self.startSeq = 1


    #==========================================================================
    # Detect the stop condition
    #==========================================================================
    @cocotb.coroutine
    def _stopDetection(self):
        yield RisingEdge(self.clk)
        while True:
            prev = int(self.sda)
            yield RisingEdge(self.clk)
            if prev == 0 and int(self.sda) == 1:
                if int(self.scl) == 1:
                    self.event_Stop.set()
                    self.listOp.append(STOP())

                    # check sequence...
                    for (op, ref) in zip(self.listOp, self.refListOp):

                        if isinstance(ref, START) and isinstance(op, START):
                            pass
                        elif isinstance(ref, STOP) and isinstance(op, STOP):
                            pass
                        elif isinstance(ref, ACK) and isinstance(op, ACK):
                            pass
                        elif isinstance(ref, NACK) and isinstance(op, NACK):
                            pass
                        elif (isinstance(ref, WRITE) or isinstance(ref, READ)) and isinstance(op, DATA):
                            if ref.data != op.data:
                                print(("ref ", hex(ref.data), " op ", hex(op.data)))
                                assertEquals(ref.data , op.data , "Analyser data ERROR")

                        else:
                            assertEquals(True , False , "%s is not equal to %s" % (ref, op))


    #==========================================================================
    # Detect a Rising edge of scl
    #==========================================================================
    @cocotb.coroutine
    def _RisingEdgeDetection(self):
        while True:
            yield RisingEdge(self.scl)
            self.event_RisingEdge.set()


    #==========================================================================
    # Detect a Falling edge of scl
    #==========================================================================
    @cocotb.coroutine
    def _FallingEdgeDetection(self):
        while True:
            yield FallingEdge(self.scl)
            self.event_FallingEdge.set()



###############################################################################
# I2C - Define all operation available
#
class I2COperation(object):
    def __init__(self, delayCmd=0, delayRsp=0):
        self.delayCMD = delayCmd
        self.delayRSP = delayRsp

class START(I2COperation):
    def __repr__(self):
        return "Start - "

class WRITE_BIT(I2COperation):
    def __init__(self, data=-1):
        if data == -1 :
            self.data = randInt(0,1)
        else:
            self.data = data

    def __repr__(self):
        return "Write %08X - " % (self.data)

class READ_BIT(I2COperation):

    def __init__(self, data=-1):
        if data == -1 :
            self.data = randInt(0,1)
        else:
            self.data = data

    def __repr__(self):
        return "Read %08X - " % (self.data)


class STOP(I2COperation):
    def __repr__(self):
        return "STOP"

