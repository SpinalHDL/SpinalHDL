import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import RisingEdge, Timer

from spinal.common.misc import Bundle, BoolRandomizer, assertEquals


class Stream:
    def __init__(self, dut, name):
        self.valid = dut.__getattr__(name + "_valid")
        self.ready = dut.__getattr__(name + "_ready")
        self.payload = Bundle(dut,name + "_payload")

StreamTransaction = type('StreamTransaction', (object,), {})


class StreamDriverMaster:
    def __init__(self,stream,transactor,clk,reset):
        self.stream = stream
        self.clk = clk
        self.reset = reset
        self.transactor = transactor

        cocotb.fork(self.stim())

    @cocotb.coroutine
    def stim(self):
        stream = self.stream
        stream.valid <= 0
        while True:
            yield RisingEdge(self.clk)
            if int(stream.valid) == 1 and int(stream.ready) == 1:
                stream.valid <= 0
                for i in xrange(nextDelay):
                    yield RisingEdge(self.clk)

            if self.transactor != None and (int(stream.valid) == 0 or int(stream.ready) == 1):
                trans = self.transactor()
                if trans != None:
                    if hasattr(trans,"nextDelay"):
                        nextDelay = trans.nextDelay
                    else:
                        nextDelay = 0
                    stream.valid <= 1

                    for name in stream.payload.nameToElement:
                        if hasattr(trans,name) == False:
                            raise TestFailure("Missing element in bundle :" + name)
                        e = stream.payload.nameToElement[name] <= getattr(trans,name)



class StreamDriverSlave:
    def __init__(self,stream,clk,reset):
        self.stream = stream
        self.clk = clk
        self.reset = reset
        cocotb.fork(self.stim())

    @cocotb.coroutine
    def stim(self):
        stream = self.stream
        stream.ready <= 1
        randomizer = BoolRandomizer()
        while True:
            yield RisingEdge(self.clk)
            stream.ready <= randomizer.get()


class BundleTransaction:
    def __init__(self,bundle):
        for name in bundle.nameToElement:
            setattr(self,name, int(bundle.nameToElement[name]))


class StreamMonitor:
    def __init__(self,stream,callback,clk,reset):
        self.stream = stream
        self.callback = callback
        self.clk = clk
        self.reset = reset
        cocotb.fork(self.stim())

    @cocotb.coroutine
    def stim(self):
        stream = self.stream
        stream.valid <= 0
        while True:
            yield RisingEdge(self.clk)
            if int(stream.valid) == 1 and int(stream.ready) == 1:
                trans = BundleTransaction(stream.payload)
                yield Timer(1)
                self.callback(trans)