import random
from Queue import Queue

import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, Join

from spinal.common.misc import setBit, randSignal, assertEquals, truncInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer


class Packet:
    def __init__(self,a,b):
        self.a = a
        self.b = b


@cocotb.coroutine
def cmd(dut,queue):
    validRandomizer = BoolRandomizer()
    dut.io_slave0_valid <= 0
    while True:
        yield RisingEdge(dut.clk)
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
            yield RisingEdge(dut.clk)
            dut.io_master0_ready <= readyRandomizer.get()
            if int(dut.io_master0_valid) == 1 and int(dut.io_master0_ready) == 1:
                break
        pop = queue.get()
        assertEquals(pop.a, dut.io_master0_payload_a,"io_master0_payload_a")
        assertEquals(pop.b, dut.io_master0_payload_b, "io_master0_payload_b")





@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    #random.seed(0)

    queue = Queue()

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(cmd(dut,queue))
    yield rsp(dut,queue)

    dut.log.info("Cocotb test done")
