import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure, TestSuccess
from cocotb.triggers import Timer, Edge, RisingEdge, Join, FallingEdge

from spinal.common.AhbLite3 import AhbLite3MasterDriver, AhbLite3SlaveMemory, AhbLite3MasterIdle, AhbLite3TraficGenerator, AhbLite3MasterReadChecker, AhbLite3Terminaison
from spinal.common.misc import setBit, randSignal, assertEquals, truncUInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer, StreamRandomizer,StreamReader, FlowRandomizer, Bundle, simulationSpeedPrinter, readIHex

# @cocotb.coroutine
# def loadIHexCallbackCoroutine(address,array,dut):
#     dut.rom.reset <= 1
#     data = 0
#     print("a")
#     for b in array:
#         data |= b << ((address & 3)*8)
#         print("b")
#         if(address & 3 == 3):
#             yield Timer(1000)
#             dut.rom.ram_port1_enable <= 1
#             dut.rom.ram_port1_mask <= 0xF
#             dut.rom.ram_port1_address <= address-3
#             dut.rom.ram_port1_data <= data
#             dut.rom.clk <= 0
#             yield Timer(1000)
#             dut.rom.clk <= 1
#             yield Timer(1000)
#             dut.rom.ram_port1_enable <= 0
#             yield Timer(1000)
#         address += 1

@cocotb.coroutine
def loadIHexCallback(address,array,dut):
    # print("miaou")
    # print("address : " + str(address) + " " + str(array))
    assert(address & 3 == 0)
    assert(len(array) & 3 == 0)
    # yield loadIHexCallbackCoroutine(address,array,dut)
    data = 0
    # print("a")
    for b in array:
        data |= b << ((address & 3) * 8)
        # print("b")
        if (address & 3 == 3):
            yield Timer(5)
            dut.rom.ram_port0_write <= 1
            dut.rom.ram_port0_enable <= 1
            dut.rom.ram_port0_mask <= 0xF
            dut.rom.ram_port0_address <= (address) >> 2
            dut.rom.ram_port0_writeData <= data
            data = 0
            dut.rom.clk <= 0
            yield Timer(5)
            dut.rom.clk <= 1
            yield Timer(5)
            dut.rom.ram_port0_enable <= 0
            yield Timer(5)
        address += 1



@cocotb.coroutine
def loadIHex(dut,hexPath):

    dut.rom.reset <= 1
    yield Timer(5)
    writeBuffer = int(dut.rom.ram_port0_write)
    enableBuffer = int(dut.rom.ram_port0_enable)
    maskBuffer = int(dut.rom.ram_port0_mask)
    addressBuffer = int(dut.rom.ram_port0_address)
    writeDataBuffer = int(dut.rom.ram_port0_writeData)


    # readIHex(hexPath,loadIHexCallback,dut)
    with open(hexPath) as f:
        offset = 0
        for line in f:
            if len(line) > 0:
                assert line[0] == ':'
                byteCount = int(line[1:3], 16)
                nextAddr = int(line[3:7], 16) + offset
                key = int(line[7:9], 16)
                if key == 0:
                    array = [int(line[9 + i * 2:11 + i * 2], 16) for i in range(0, byteCount)]
                    yield loadIHexCallback(nextAddr,array,dut)
                elif key == 2:
                    offset = int(line[9:13], 16)
                else:
                    pass
    dut.rom.reset <= 0
    yield Timer(5)
    dut.rom.reset <= 1

    dut.rom.ram_port0_write <= writeBuffer
    dut.rom.ram_port0_enable <= enableBuffer
    dut.rom.ram_port0_mask <= maskBuffer
    dut.rom.ram_port0_address <= addressBuffer
    dut.rom.ram_port0_writeData <= writeDataBuffer

@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    random.seed(0)

    cocotb.fork(simulationSpeedPrinter(dut.clk))

    yield loadIHex(dut,"e:/vm/share/pinsec_test.hex")

    # elements = [a for a in dut.AhbRam if a._name.startswith("")]
    # for e in elements:
    #     print(str(e._name))

    # while True:
    #     dut.AhbRam.ram_port1_enable <= 1
    #     dut.AhbRam.ram_port1_mask <= 0xF
    #     dut.AhbRam.ram_port1_address <= 0X90
    #     dut.AhbRam.ram_port1_data <= 0xCAFEF00D
    #
    #     dut.AhbRam.reset <= 1
    #     dut.AhbRam.ram_port1_enable <= 1
    #     yield Timer(1000)
    #     dut.AhbRam.ram_port1_enable <= 1
    #     dut.AhbRam.clk <= 0
    #     yield Timer(1000)
    #     dut.AhbRam.ram_port1_enable <= 1
    #     dut.AhbRam.clk <= 1
    #     yield Timer(1000)
    #     dut.AhbRam.ram_port1_enable <= 0
    #     yield Timer(1000)

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))


    dut.io_interrupt <= 0
    # dut.io_debugResetIn <= 0

    yield Timer(1000*5000)
    # while True:
    #     yield RisingEdge(dut.clk)
    #     if checker.counter > 4000:
    #         break

    dut.log.info("Cocotb test done")
