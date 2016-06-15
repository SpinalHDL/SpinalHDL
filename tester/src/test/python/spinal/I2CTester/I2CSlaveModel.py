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
      #  cocotb.fork(self.manageSDA())

    @cocotb.coroutine
    def stateMachine(self):
        
        isFirstData = True # first data read avec a start is the address

        while True:     
        
            if self.state == I2CSlaveState.IDLE:                
                
                yield self.startEvent.wait()
                
                isFirstData = True 
                self.state  = I2CSlaveState.RD_ADDR


            elif self.state == I2CSlaveState.RD_ADDR:
                
                yield self.readTrame()

                if self.dataTXEvent.data[7] == 0 : # read operatrion                    
                    self.state = I2CSlaveState.WR_DATA  
                else:                              # write operation              
                    self.state = I2CSlaveState.RD_DATA 


            elif self.state == I2CSlaveState.RD_DATA :

                data = self.mem.get(self.addr, 55) #BinaryValue(0xAA, 8)) # if addr doesn't exist read AA
                cocotb.fork(self.writeTrame(data))
                yield self.dataTXEvent.wait()
                if self.dataTXEvent.data == 1 : # NACK received
                    yield self.stopEvent.wait()
                    self.state = I2CSlaveState.IDLE
                else :                          # ACK received
                    self.state = I2CSlaveState.WR_DATA


            elif self.state == I2CSlaveState.WR_DATA :

                yield RisingEdge(self.scl)

                yield self.readTrame()

                dataReceived = self.dataTXEvent.data

                if isFirstData == True :    
                    self.addr   = int("".join([str(x) for x in dataReceived]), 2)
                    isFirstData = False
                else:
                    self.mem[self.addr] = int(self.dataTXEvent.data)
                    self.addr += 1

                self.state = I2CSlaveState.IDLE

                

    @cocotb.coroutine
    def manageSDA(self):
        while True:
            yield Edge(self.clk)

            if self.forceSDA == 0 :                
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



    @cocotb.coroutine
    def stopDetection(self):
        while True:
            yield RisingEdge(self.sda_wr)
            if int(self.scl) == 1:
                print("Stop detected  ...")
                self.stopEvent.set()
                self.state = I2CSlaveState.IDLE


    @cocotb.coroutine
    def readTrame(self):    
        cnt  = 0
        dataRead = list()
        while True:
                        
            if (cnt == 8):
                print("Send ack")
                yield FallingEdge(self.scl)
                self.sda_rd <= 0 # send ACK                
                self.dataTXEvent.set(data=dataRead)
                break
            else:
                yield RisingEdge(self.scl)                
                dataRead.append( int(self.sda_wr) )

            cnt += 1


    @cocotb.coroutine
    def writeTrame(self, data):
        write = True
        cnt   = 0 
        while write:
            yield FallingEdge(self.scl)
            self.forceSDA <= int(data(cnt))
            cnt += 1
            if (cnt == 7):
                yield RisingEdge(self.scl)
                yield RisingEdge(self.scl)
                # read ACK 
                ack = int(self.sda_wr)
                dataRXEvent.set(ack)

