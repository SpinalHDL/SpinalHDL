import random

import cocotb
from cocotb.triggers import Timer

from cocotblib.misc import assertEquals, ClockDomainAsyncReset, Bundle, simulationSpeedPrinter, log2Up



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
    dut.log.info("Cocotb test boot")
    random.seed(0)

    cocotb.fork(simulationSpeedPrinter(dut.io_axiClk))
    yield loadIHex(dut,"e:/vm/share/pinsec_test.hex",dut.io_axiClk,dut.io_asyncReset)
    cocotb.fork(ClockDomainAsyncReset(dut.io_axiClk, dut.io_asyncReset))

    jtag = JtagMaster(Bundle(dut,"io_jtag"),4000,4)

    yield Timer(1000*50)

    yield jtag.goToIdle()
    yield Timer(1000*8)

    # Check rom write/read via jtag
    yield jtagBridgeWrite(jtag,0x3000,0x11223344,4)
    yield jtagBridgeWrite(jtag,0x3002,0x00550000,1)
    yield jtagBridgeReadAssert(jtag,0x3000,4,0x11553344)
    yield jtagBridgeReadAssert(jtag,0x3002,2,0x1155)


    # Check RISCV APB debug module via jtag
    yield jtagBridgeWrite(jtag,0xF00F0200,1 << 17,4) #halt CPU
    yield jtagBridgeReadAssert(jtag,0xF00F0200,4,(1 << 1),0x0F)
    yield jtagBridgeWrite(jtag,0xF00F0008,0x99887766,4) #write R2
    yield jtagBridgeReadAssert(jtag,0xF00F0008,4,0x99887766)


    yield Timer(1000*500)

    dut.log.info("Cocotb test done")
