import random
from Queue import Queue

import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, Join

from spinal.common.misc import setBit, randSignal, assertEquals, truncInt, sint, ClockDomainAsyncReset, randBoolSignal


class Packet:
    def __init__(self,a,b):
        self.a = a
        self.b = b


@cocotb.coroutine
def cmd(dut,queue):
    prob = 0.5
    counter = 0
    dut.io_slave0_valid <= 0
    while True:
        yield RisingEdge(dut.clk)
        if int(dut.io_slave0_valid) == 1 and int(dut.io_slave0_ready) == 1:
            queue.put(Packet(int(dut.io_slave0_payload_a),int(dut.io_slave0_payload_b)))
        randBoolSignal(dut.io_slave0_valid,prob)
        randSignal(dut.io_slave0_payload_a)
        randSignal(dut.io_slave0_payload_b)

        counter = counter + 1
        if counter == 50:
            counter = 0
            prob = random.uniform(0.1,0.9)




@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    #random.seed(0)

    queue = Queue()

    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))
    cocotb.fork(cmd(dut,queue))

    prob = 0.5
    counter = 0
    dut.io_master0_ready <= 0
    for i in range(0,1000):
        while True:
            yield RisingEdge(dut.clk)
            randBoolSignal(dut.io_master0_ready,prob)
            if int(dut.io_master0_valid) == 1 and int(dut.io_master0_ready) == 1:
                break
        pop = queue.get()
        assertEquals(pop.a, dut.io_master0_payload_a,"io_master0_payload_a")
        assertEquals(pop.b, dut.io_master0_payload_b, "io_master0_payload_b")
        counter = counter + 1
        if counter == 50:
            counter = 0
            prob = random.uniform(0.1,0.9)

    dut.log.info("Cocotb test done")
