import cocotb
from cocotb.triggers import Timer

from cocotblib.misc import assertEquals, Bundle, simulationSpeedPrinter, log2Up
from spinal.Pinsec.common.HexLoader import loadIHex
from spinal.Pinsec.common.Jtag import JtagMaster
from spinal.Pinsec.common.Misc import pinsecClockGen


@cocotb.coroutine
def jtagBridgeCmd(ctrl,address,data,size,write):
    yield ctrl.setInstr(2)
    value = (address << 8) | (data << 40) | (write << 72) | (log2Up(size) << 73)
    yield ctrl.setData(value,75)

@cocotb.coroutine
def jtagBridgeRsp(ctrl,tab):
    yield ctrl.setInstr(3)
    tab[0] = 0
    tab[1] = 0
    yield ctrl.getData(tab,34)
    tab[1] = tab[0] >> 2
    tab[0] = tab[0] & 3
    print("flag=%d data=%x" %(tab[0],tab[1]))

# @cocotb.coroutine
# def jtagBridgeRspAssert(ctrl,value):
#     tab = [0]
#     yield jtagBridgeRsp(ctrl,tab)
#     assertEquals(tab[0] >> 2, value, "Wrong rsp")


@cocotb.coroutine
def jtagBridgeWrite(ctrl,address,data,size):
    yield jtagBridgeCmd(ctrl,address,data,size,1)

@cocotb.coroutine
def jtagBridgeRead(ctrl,address,size,tab):
    yield jtagBridgeCmd(ctrl,address,0,size,0)
    yield jtagBridgeRsp(ctrl,tab)

@cocotb.coroutine
def jtagBridgeReadAssert(ctrl, address, size,value,mask = -1):
    yield jtagBridgeCmd(ctrl, address,0, size, 0)
    tab = [0,0]
    yield jtagBridgeRsp(ctrl, tab)
    assertEquals(tab[0], 1, "rsp not ready")
    assertEquals(tab[1] & mask, value, "wrong rsp, expected" + str(value))


@cocotb.test()
def jtagTest(dut):
    from cocotblib.misc import cocotbXHack
    cocotbXHack()
    uut = dut.uut
    log = open('uartTx.log', 'w')

    cocotb.fork(simulationSpeedPrinter(uut.io_axiClk))
    yield loadIHex(dut,"../hex/dummy.hex",uut.io_axiClk,uut.io_asyncReset)
    pinsecClockGen(dut)

    yield Timer(1000*10)

    jtag = JtagMaster(Bundle(uut,"io_jtag"),20000*4,4)

    yield Timer(1000*50)

    yield jtag.goToIdle()
    yield Timer(1000*8)

    #yield jtagBridgeWrite(jtag,0xF00F0200,0x00030000,4)
    #yield Timer(1000*80)


    # Check rom write/read via jtag
    yield jtagBridgeWrite(jtag,8,0x11223344,4)
    yield jtagBridgeWrite(jtag,10,0x00550000,1)
    yield jtagBridgeReadAssert(jtag,8,4,0x11553344)
    yield jtagBridgeReadAssert(jtag,10,2,0x1155)



    yield jtagBridgeWrite(jtag,0x40004FF0,0x77665544,4)
    yield Timer(1000*50)
    yield jtagBridgeReadAssert(jtag,0x40004FF0,4,0x77665544)

    yield jtagBridgeWrite(jtag,0x40004FF2,0x00FF0000,1)
    yield Timer(1000*50)
    yield jtagBridgeReadAssert(jtag,0x40004FF0,4,0x77FF5544)


    # Check RISCV APB debug module via jtag
    yield jtagBridgeWrite(jtag,0xF00F0200,1 << 17,4) #halt CPU
    yield jtagBridgeReadAssert(jtag,0xF00F0200,4,(1 << 1),0x0F)
    yield jtagBridgeWrite(jtag,0xF00F0008,0x99887766,4) #write R2
    yield jtagBridgeReadAssert(jtag,0xF00F0008,4,0x99887766)
    yield jtagBridgeReadAssert(jtag,0xF00F0000+10*4,4,0x55) #Written by dummy.hex

    yield Timer(1000*500)
