import random

import cocotb
from cocotb.triggers import Timer, Edge

from cocotblib.misc import simulationSpeedPrinter
from spinal.Pinsec.common.CoreCom import readCoreValueAssert
from spinal.Pinsec.common.HexLoader import loadIHex
from spinal.Pinsec.common.Misc import pinsecClockGen


@cocotb.coroutine
def txToRxBypass(dut):
    while True:
        dut.io_uart_rxd <= int(dut.io_uart_txd)
        yield Edge(dut.io_uart_txd)

@cocotb.coroutine
def assertions(dut):
    yield readCoreValueAssert(dut,16, "A")
    yield readCoreValueAssert(dut,16, "A2")
    yield readCoreValueAssert(dut,0xAA, "B")
    yield readCoreValueAssert(dut,3, "C")
    yield readCoreValueAssert(dut,0x01, "D")
    yield readCoreValueAssert(dut,0x02, "E")
    yield readCoreValueAssert(dut,0x03, "F")
    yield readCoreValueAssert(dut,0, "C")

    yield readCoreValueAssert(dut,0x10, "L")
    yield readCoreValueAssert(dut,0x99, "M")
    yield readCoreValueAssert(dut,0x12345678, "N")

    yield readCoreValueAssert(dut,0x9A, "O")

    yield readCoreValueAssert(dut, 0x10, "P")
    yield readCoreValueAssert(dut, 0x9B, "Q")
    yield readCoreValueAssert(dut, 0x1234567B, "R")
    yield Timer(1000*10)

@cocotb.test()
def test1(dut):
    random.seed(0)
    from cocotblib.misc import cocotbXHack
    cocotbXHack()
    uut = dut.uut

    cocotb.fork(simulationSpeedPrinter(uut.io_axiClk))
    yield loadIHex(dut,"../hex/uart.hex",uut.io_axiClk,uut.io_asyncReset)
    pinsecClockGen(dut)
    cocotb.fork(txToRxBypass(uut))

    yield assertions(uut)
