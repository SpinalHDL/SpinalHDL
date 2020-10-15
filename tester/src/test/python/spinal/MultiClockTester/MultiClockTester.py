from queue import Queue

import cocotb
from cocotb.triggers import Timer, RisingEdge

from cocotblib.misc import randSignal, assertEquals, BoolRandomizer


class Packet:
    def __init__(self,a,b):
        self.a = a
        self.b = b


@cocotb.coroutine
def cmd(dut,queue):
    validRandomizer = BoolRandomizer()
    dut.io_slave0_valid <= 0
    while True:
        yield RisingEdge(dut.io_clkA)
        if int(dut.io_slave0_valid) == 1 and int(dut.io_slave0_ready) == 1:
            queue.put(Packet(int(dut.io_slave0_payload_a),int(dut.io_slave0_payload_b)))
        dut.io_slave0_valid <= validRandomizer.get()
        randSignal(dut.io_slave0_payload_a)
        randSignal(dut.io_slave0_payload_b)



@cocotb.coroutine
def rsp(dut,queue):
    readyRandomizer = BoolRandomizer()
    dut.io_master0_ready <= 0
    for i in range(0,1000):
        while True:
            yield RisingEdge(dut.io_clkB)
            dut.io_master0_ready <= readyRandomizer.get()
            if int(dut.io_master0_valid) == 1 and int(dut.io_master0_ready) == 1:
                break
        pop = queue.get()
        assertEquals(pop.a, dut.io_master0_payload_a,"io_master0_payload_a")
        assertEquals(pop.b, dut.io_master0_payload_b, "io_master0_payload_b")





@cocotb.coroutine
def clockProcess(dut):
    randomizer = BoolRandomizer()
    dut.io_clkA <= 0
    dut.io_clkB <= 0
    dut.io_resetB <= 1
    dut.io_resetA <= 1
    yield Timer(1000)
    dut.io_resetA <= 0
    dut.io_resetB <= 0
    while True:
        dut.io_clkA <= 0
        dut.io_clkB <= 0
        yield Timer(500)
        if randomizer.get():
            dut.io_clkA <= 1
        else:
            dut.io_clkB <= 1
        yield Timer(500)



@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    from cocotblib.misc import cocotbXHack
    cocotbXHack()
    #random.seed(0)

    queue = Queue()

    cocotb.fork(clockProcess(dut))
    cocotb.fork(cmd(dut,queue))
    yield rsp(dut,queue)

    dut.log.info("Cocotb test done")
