import random

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer, RisingEdge, FallingEdge

from cocotblib.misc import simulationSpeedPrinter
from spinal.Pinsec.common.CoreCom import readCoreValueAssert
from spinal.Pinsec.common.HexLoader import loadIHex
from spinal.Pinsec.common.Misc import pinsecClockGen


@cocotb.coroutine
def uartTxBypass(uart,clk,log):
    while True:
        yield RisingEdge(clk)
        if int(uart.io_write_valid) == 1 and int(uart.io_write_ready) == 1:
            log.write(str(chr(int(uart.io_write_payload))))
        yield FallingEdge(clk)
        uart.io_write_ready <= 1


@cocotb.coroutine
def assertions(dut,log):
    yield readCoreValueAssert(dut, 0x42, "A")
    log.flush()
    log.close()

    log = open('uartTx.log', 'r').read()
    ref = open('uartTx.ref', 'r').read()
    if len(log) < len(ref):
        raise TestFailure("log is smaller than ref")

    if log[0:len(ref)] != ref:
        raise TestFailure("log doesn't match with ref")


def _unidiff_output(expected, actual):
    """
    Helper function. Returns a string containing the unified diff of two multiline strings.
    """

    import difflib
    expected=expected.splitlines(1)
    actual=actual.splitlines(1)

    diff=difflib.unified_diff(expected, actual)

    return ''.join(diff)

@cocotb.test()
def test1(dut):
    from cocotblib.misc import cocotbXHack
    cocotbXHack()
    random.seed(0)

    uut = dut.uut
    log = open('uartTx.log', 'w')

    cocotb.fork(simulationSpeedPrinter(uut.io_axiClk))
    yield loadIHex(dut,"../hex/dhrystone.hex",uut.io_axiClk,uut.io_asyncReset)
    pinsecClockGen(dut)
    cocotb.fork(uartTxBypass(uut.axi_uartCtrl.uartCtrl,uut.io_axiClk,log))

    yield assertions(uut,log)
    yield Timer(1000*10)
