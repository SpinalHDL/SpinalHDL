import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure, TestSuccess
from cocotb.triggers import Timer, Edge, RisingEdge, Join, FallingEdge

from spinal.Pinsec.common.CoreCom import readCoreValue, readCoreValueAssert
from spinal.Pinsec.common.HexLoader import loadIHex
from spinal.common.AhbLite3 import AhbLite3MasterDriver, AhbLite3SlaveMemory, AhbLite3MasterIdle, AhbLite3TraficGenerator, AhbLite3MasterReadChecker, AhbLite3Terminaison
from spinal.common.misc import setBit, randSignal, assertEquals, truncUInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer, StreamRandomizer,StreamReader, FlowRandomizer, Bundle, simulationSpeedPrinter, readIHex, log2Up


@cocotb.coroutine
def uartTxBypass(uart,clk,log):
    while True:
        yield RisingEdge(clk)
        if int(uart.io_write_valid) == 1 and int(uart.io_write_ready) == 1:
            log.write(str(unichr(int(uart.io_write_payload))))
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
    dut.log.info("Cocotb test boot")
    random.seed(0)

    log = open('uartTx.log', 'w')

    cocotb.fork(simulationSpeedPrinter(dut.io_axiClk))
    yield loadIHex(dut,"../hex/dhrystone.hex",dut.io_axiClk,dut.io_asyncReset)
    cocotb.fork(ClockDomainAsyncReset(dut.io_axiClk, dut.io_asyncReset))
    cocotb.fork(uartTxBypass(dut.axi_uartCtrl.uartCtrl,dut.io_axiClk,log))

    yield assertions(dut,log)
    yield Timer(1000*10)

    dut.log.info("Cocotb test done")
