import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, FallingEdge, Event

class I2CSlaveState:
    IDLE    = 0
    RD_ADDR = 1
    RD_DATA = 2
    WR_DATA = 3


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
                state       = I2CSlaveState.RD_ADDR

            elif self.state == I2CSlaveState.RD_ADDR:
                cocotb.fork(readTrame)          
                print("SEnd address")      
                yield self.dataTXEvent.wait() 
                print("ADDRESS received ", seld.data.dataTXEvent.data())
                
                if self.dataTXEvent.data(7) == 0 : # read operatrion
                    state == I2CSlaveState.WR_DATA # write view from the master
                else:                              # write operation              
                    state == I2CSlaveState.RD_DATA # read view form the slave

            elif self.state == I2CSlaveState.RD_DATA :
                cocotb.fork(readTrame)
                yield self.dataRxEvent.wait()
                
                if isFirstData == True :    
                    self.addr   = int(self.dataRxEvent.data)
                    isFirstData = False
                else:
                    self.mem[self.addr] = int(self.dataRxEvent.data())
                    self.addr += 1

            elif self.state == I2CSlaveState.WR_DATA :
                data = self.mem.get(self.addr, BinaryValue(0xAA, 8)) # if addr doesn't exist read AA
                cocotb.fork(writeTrame(data))
                yield self.dataTXEvent.wait()
                if self.dataTXEvent.data == 1 : # NACK received
                    yield self.stopEvent.wait()
                    state = I2CSlaveState.IDLE
                else :                           # ACK received
                    state = I2CSlaveState.WR_DATA


    @cocotb.coroutine
    def manageSDA(self):
        while True:
            yield Edge(self.clk)
            self.sda_rd <= int(self.sda_wr)
            #if int(self.sda_rd) == 1:
            #    self.sda_rd <= self.sda_wr
            #else:
            #    self.sda_rd <= 0
            
            

    @cocotb.coroutine
    def startDetection(self):
        while True:
            yield FallingEdge(self.sda_wr)
            if int(self.scl) == 1:
                self.startEvent.set()                


    @cocotb.coroutine
    def stopDetection(self):
        while True:
            yield RisingEdge(self.sda_wr)
            if int(self.scl) == 1:
                self.stopEvent.set()


    @cocotb.coroutine
    def readTrame(self):
        read = True
        cnt  = 0
        dataRead = list()
        while read:
            yield RisingEdge(self.scl)
            # read value
            dataRead.append( int(self.sda_wr) )
            cnt += 1
            if (cnt == 7):
                yield FallingEdge(self.scl)
                self.sda_rd <= 0 # send ACK
                read = False
                self.dataTXEvent.set(dataRead)


    @cocotb.coroutine
    def writeTrame(self, data):
        write = True
        cnt   = 0 
        while write:
            yield FallingEdge(self.scl)
            self.sda_rd <= int(data(cnt))
            cnt += 1
            if (cnt == 7):
                yield RisingEdge(self.scl)
                yield RisingEdge(self.scl)
                # read ACK 
                ack = int(self.sda_wr)
                dataTXEvent.set(ack)

