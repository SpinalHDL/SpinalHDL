import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import RisingEdge, Timer

from spinal.common.misc import Bundle, BoolRandomizer, assertEquals


class Stream:
    def __init__(self, dut, name):
        self.valid = dut.__getattr__(name + "_valid")
        self.ready = dut.__getattr__(name + "_ready")
        self.payload = Bundle(dut,name + "_payload")

class Transaction(object):
    def __init__(self):
        object.__setattr__(self,"_nameToElement",{})

    def __setattr__(self, key, value):
        # print("set " + key)
        if key[0] != '_':
            self._nameToElement[key] = value
        object.__setattr__(self,key,value)

    def equalRef(self,ref):
        if(len(self._nameToElement) != len(ref._nameToElement)):
            return False
        for name in self._nameToElement:
            if self._nameToElement[name] != getattr(ref,name):
                return False
        return True

    def assertEqualRef(self,ref):
        if not self.equalRef(ref):
            raise TestFailure("\nFAIL transaction not equal\ntransaction =>\n%s\nref =>\n%s\n\n" % (self,ref))


    def __str__(self):
        buffer = ""
        biggerName = 0
        for n in self._nameToElement:
            if len(n) > biggerName:
                biggerName = len(n)
        for name in self._nameToElement:
            buffer += "%s %s: 0x%x\n" % (name," "*(biggerName-len(name)),self._nameToElement[name])
        return buffer

# Transaction = type('Transaction', (object,), {})

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


def TransactionFromBundle(bundle):
    trans = Transaction()
    for name in bundle.nameToElement:
        setattr(trans,name, int(bundle.nameToElement[name]))
    return trans


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
                trans = TransactionFromBundle(stream.payload)
                yield Timer(1)
                self.callback(trans)