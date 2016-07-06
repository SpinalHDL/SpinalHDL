import random
from Queue import Queue

import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge, Join

from spinal.common.misc import setBit, randSignal, assertEquals, truncUInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer, StreamRandomizer,StreamReader


class FifoPacket:
    def __init__(self,a,b):
        self.a = a
        self.b = b

class Fifo:
    def __init__(self,dut):
        self.queue = Queue()
        self.dut = dut

    @cocotb.coroutine
    def run(self):
        cocotb.fork(self.push())
        yield self.pop()

    @cocotb.coroutine
    def push(self):
        dut = self.dut
        queue = self.queue
        validRandomizer = BoolRandomizer()
        dut.io_slave0_valid <= 0
        while True:
            yield RisingEdge(dut.clk)
            if int(dut.io_slave0_valid) == 1 and int(dut.io_slave0_ready) == 1:
                queue.put(FifoPacket(int(dut.io_slave0_payload_a), int(dut.io_slave0_payload_b)))
            dut.io_slave0_valid <= validRandomizer.get()
            randSignal(dut.io_slave0_payload_a)
            randSignal(dut.io_slave0_payload_b)

    @cocotb.coroutine
    def pop(self):
        dut = self.dut
        queue = self.queue
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


class Fork:
    def __init__(self,dut):
        self.queues = [Queue() for i in range(0,3)]
        self.counters = [0 for i in range (0,3)]
        self.dut = dut

    def onInput(self,payload,handle):
        for queue in self.queues:
            queue.put(payload)

    def onOutput(self,payload,portId):
        assertEquals(payload,self.queues[portId].get(),"fork error")
        self.counters[portId] += 1

    @cocotb.coroutine
    def run(self):
        cocotb.fork(StreamRandomizer("forkInput", self.onInput,None, self.dut, self.dut.clk))
        for idx in range(0,3):
            cocotb.fork(StreamReader("forkOutputs_" + str(idx), self.onOutput, idx, self.dut, self.dut.clk))

        while not reduce(lambda x,y: x and y, map(lambda x: x > 100, self.counters)):
            yield RisingEdge(self.dut.clk)




#
# forkInput_valid = > forkInput_valid,
# forkInput_ready = > forkInput_ready,
# forkInput_payload = > forkInput_payload,
# forkOutput_0_valid = > forkOutput_0_valid,
# forkOutput_0_ready = > forkOutput_0_ready,
# forkOutput_0_payload = > forkOutput_0_payload,
# forkOutput_1_valid = > forkOutput_1_valid,
# forkOutput_1_ready = > forkOutput_1_ready,
# forkOutput_1_payload = > forkOutput_1_payload,
# forkOutput_2_valid = > forkOutput_2_valid,
# forkOutput_2_ready = > forkOutput_2_ready,
# forkOutput_2_payload = > forkOutput_2_payload,


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    #random.seed(0)


    cocotb.fork(ClockDomainAsyncReset(dut.clk, dut.reset))

    fifo = cocotb.fork(Fifo(dut).run())
    fork = cocotb.fork(Fork(dut).run())

    yield fifo.join()
    yield fork.join()

    #
    #
    # yield fork


    dut.log.info("Cocotb test done")
