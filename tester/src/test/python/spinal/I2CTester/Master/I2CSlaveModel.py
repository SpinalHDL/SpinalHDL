import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, FallingEdge, Event

class I2CSlaveState:
    IDLE    = 0
    RD_ADDR = 1
    RD_DATA = 2
    WR_DATA = 3
    WR_ACK  = 4


class I2CSlaveModel:


    def __init__(self, _sda_wr, _sda_rd, _scl, _clk, _resetn, addrMode, initMemory = dict()):
       
        self.startEvent  = Event()
        self.stopEvent   = Event()
        self.dataRxEvent = Event()
        self.dataTXEvent = Event()
        self.sda_wr      = _sda_wr
        self.sda_rd      = _sda_rd 
        self.scl         = _scl 
        self.modeAddr    = addrMode   # 7 = 7 bits , 10 = 10 bits
        self.mem         = initMemory # Create a dictionnary (address, value)
        self.addr        = 0
        self.resetn      = _resetn
        self.clk         = _clk 
        self.state       = I2CSlaveState.IDLE


        self.forceSDA    = 1

    def startSlave(self):
        
        cocotb.fork(self.startDetection())
        cocotb.fork(self.stopDetection())
        cocotb.fork(self.stateMachine())
        cocotb.fork(self.manageSDA())

    @cocotb.coroutine
    def stateMachine(self):
        
        isFirstData = True # first data read avec a start is the address

        while True:     
        
            if self.state == I2CSlaveState.IDLE:                
                
                yield self.startEvent.wait()
                
                isFirstData = True 
                self.state  = I2CSlaveState.RD_ADDR


            elif self.state == I2CSlaveState.RD_ADDR:
                
                yield self._readData()

                if self.dataTXEvent.data[7] == 0 :                   
                    self.state = I2CSlaveState.RD_DATA
                else:                                           
                    self.state = I2CSlaveState.WR_DATA


            elif self.state == I2CSlaveState.WR_DATA :

                data = self.mem.get(self.addr, 55) #BinaryValue(0xAA, 8)) # if addr doesn't exist read AA

                yield self._writeData(data)

                res = self.dataRxEvent.data

                if res["ack"] == 1 : # NACK received
                    yield self.stopEvent.wait()
                    self.state = I2CSlaveState.IDLE
                else :                          # ACK received
                    self.state = I2CSlaveState.WR_DATA
                    self.addr += 1


            elif self.state == I2CSlaveState.RD_DATA :

                yield FallingEdge(self.scl)
                self.forceSDA = 1 

                yield self._readData()

                dataReceived = self.dataTXEvent.data

                if isFirstData == True :    
                    self.addr   = int("".join([str(x) for x in dataReceived]), 2)                    
                    isFirstData = False
                else:
                    self.mem[self.addr] = int("".join([str(x) for x in dataReceived]), 2)
                    self.addr += 1
                    print("Memory", self.mem)

                self.state = I2CSlaveState.RD_DATA

    @cocotb.coroutine
    def manageSDA(self):
        while True:
            yield Edge(self.clk)
            #self.sda_rd <= self.forceSDA
            if self.forceSDA == 0:
                self.sda_rd <= 0
            else:
                self.sda_rd <= int(self.sda_wr)



    @cocotb.coroutine
    def startDetection(self):
        while True:
            yield FallingEdge(self.sda_wr)
            if int(self.scl) == 1:
                print("Start detected ...")
                self.startEvent.set()
                self.addr = 0


    @cocotb.coroutine
    def stopDetection(self):
        while True:
            yield RisingEdge(self.sda_wr)
            if int(self.scl) == 1:
                print("Stop detected  ...")
                self.stopEvent.set()
                self.state = I2CSlaveState.IDLE



    @cocotb.coroutine
    def _readData(self):
        cnt  = 0
        dataRead = list()
        while True:            
            if (cnt == 8):
                yield FallingEdge(self.scl)                
                self.forceSDA = 0 # send ACK
                yield RisingEdge(self.scl)
                self.dataTXEvent.set(data=dataRead)

                break
            else:
                yield RisingEdge(self.scl)                
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

