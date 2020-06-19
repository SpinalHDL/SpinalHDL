import random
from queue import Queue

import cocotb
from cocotb.triggers import Edge, RisingEdge, FallingEdge

from cocotblib.misc import assertEquals, ClockDomainAsyncReset, simulationSpeedPrinter

UartParityType_NONE = 0
UartParityType_EVEN = 1
UartParityType_ODD  = 2


UartStopType_ONE = 0
UartStopType_TWO = 1



@cocotb.coroutine
def sendRandomByte(dut, queueTx, queueRx):
    value = random.randint(0,255)
    queueTx.put(value)
    queueRx.put(value)

    dut.io_uart_write_valid <= 1
    dut.io_uart_write_payload <= value
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.io_uart_write_ready) == 1 :
            dut.io_uart_write_valid <= 0
            break

@cocotb.coroutine
def sendRandomPackets(dut, queueTx, queueRx):
    dut.io_uart_write_valid <= 0
    dut.io_uart_config_frame_dataLength <= 7;
    dut.io_uart_config_frame_stop <= UartStopType_ONE;
    dut.io_uart_config_frame_parity <= UartParityType_EVEN;
    dut.io_uart_config_clockDivider <= 2;

    while True:
        yield RisingEdge(dut.clk)
        packetSize = random.randint(1,10)
        for i in range(0,packetSize):
            yield sendRandomByte(dut, queueTx, queueRx)

        for i in range(0, random.randint(100,1000)):
            yield RisingEdge(dut.clk)

@cocotb.coroutine
def checkCtrlReadedBytes(dut, queue):
    for i in range(0,200):
        while True:
            yield RisingEdge(dut.clk)
            if int(dut.io_uart_read_valid) == 1 :
                break
        assertEquals(queue.get(), dut.io_uart_read_payload,"io_uart_read_payload")




@cocotb.coroutine
def txToRxBypass(dut):
    while True:
        dut.io_uart_uart_rxd <= int(dut.io_uart_uart_txd)
        yield Edge(dut.io_uart_uart_txd)


@cocotb.coroutine
def checkPin(value,cycles,pin,clk):
    for i in range(0,cycles):
        yield RisingEdge(clk)
        assertEquals(value, pin, "io_uart_uart_txd")

@cocotb.coroutine
def checkTx(dut,queue):
    dutTx = dut.io_uart_uart_txd
    while True:
        yield FallingEdge(dutTx)
        baudCycles = (int(dut.io_uart_config_clockDivider)+1)*8
        assert not queue.empty()
        data = queue.get()
        yield checkPin(0,baudCycles,dutTx,dut.clk)

        if int(dut.io_uart_config_frame_parity) == UartParityType_EVEN:
          parity = 0
        else:
          parity = 1

        for i in range(0,int(dut.io_uart_config_frame_dataLength)+1):
            value = (data >> i) & 1
            parity ^= value
            yield checkPin(value, baudCycles, dutTx, dut.clk)

        if int(dut.io_uart_config_frame_parity) != UartParityType_NONE:
            yield checkPin(parity, baudCycles, dutTx, dut.clk)

        for i in range(0, int(dut.io_uart_config_frame_stop) + 1):
            yield checkPin(1, baudCycles, dutTx, dut.clk)



@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")

    from cocotblib.misc import cocotbXHack
    cocotbXHack()

    queueTx = Queue()
    queueRx = Queue()

    dut.io_uart_writeBreak <= 0

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(sendRandomPackets(dut, queueTx, queueRx))
    cocotb.fork(checkTx(dut,queueTx))
    cocotb.fork(txToRxBypass(dut))
    cocotb.fork(simulationSpeedPrinter(dut.clk))
    yield checkCtrlReadedBytes(dut, queueRx)

    dut.log.info("Cocotb test done")
