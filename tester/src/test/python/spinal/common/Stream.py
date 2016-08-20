from Queue import Queue

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import RisingEdge, Timer

from spinal.common.Phase import Infrastructure, PHASE_CHECK_SCORBOARDS
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


class StreamScorboardInOrder(Infrastructure):
    def __init__(self,name,parent):
        Infrastructure.__init__(self,name,parent)
        self.refs = Queue()
        self.uuts = Queue()

    def refPush(self,ref):
        self.refs.put(ref)
        self.update()

    def uutPush(self, uut,):
        self.uuts.put(uut)
        self.update()

    def update(self):
        if (not self.refs.empty()) and (not self.uuts.empty()):
            ref = self.refs.get()
            uut = self.uuts.get()

            self.match(uut,ref)


    def match(self,uut,ref):
        if not uut.equalRef(ref):
            cocotb.log.error("Missmatch detected in " + self.getPath())
            uut.assertEqualRef(ref)

    def startPhase(self, phase):
        Infrastructure.startPhase(self, phase)
        if phase == PHASE_CHECK_SCORBOARDS:
            if (not self.refs.empty()) or (not self.uuts.empty()):
                error = self.getPath() + " has some remaining transaction :\n"
                for e in self.refs.queue:
                    error += "REF:\n" + str(e) + "\n"

                for e in self.uuts.queue:
                    error += "UUT:\n" + str(e) + "\n"

                cocotb.log.error(error)


    def endPhase(self, phase):
        Infrastructure.endPhase(self, phase)
        if phase == PHASE_CHECK_SCORBOARDS:
            if (not self.refs.empty()) or (not self.uuts.empty()):
                raise TestFailure("Scoreboard not empty")


class StreamScorboardOutOfOrder(Infrastructure):
    def __init__(self,name,parent):
        Infrastructure.__init__(self,name,parent)
        self.refsDic = {}
        self.uutsDic = {}

    def refPush(self,ref,oooid):
        if not self.refsDic.has_key(oooid):
            self.refsDic[oooid] = Queue()
        self.refsDic[oooid].put(ref)
        self.update(oooid)

    def uutPush(self, uut, oooid):
        if not self.uutsDic.has_key(oooid):
            self.uutsDic[oooid] = Queue()
        self.uutsDic[oooid].put(uut)
        self.update(oooid)

    def update(self,oooid):
        if self.uutsDic.has_key(oooid) and self.refsDic.has_key(oooid):
            refs = self.refsDic[oooid]
            uuts = self.uutsDic[oooid]

            ref = refs.get()
            uut = uuts.get()

            self.match(uut,ref)

            #Clean
            if refs.empty():
                self.refsDic.pop(oooid)
            if uuts.empty():
                self.uutsDic.pop(oooid)


    def match(self,uut,ref):
        if not uut.equalRef(ref):
            cocotb.log.error("Missmatch detected in " + self.getPath())
            uut.assertEqualRef(ref)

    def startPhase(self, phase):
        Infrastructure.startPhase(self, phase)
        if phase == PHASE_CHECK_SCORBOARDS:
            if len(self.refsDic) != 0 or len(self.uutsDic) != 0:
                error = self.getPath() + " has some remaining transaction :\n"
                for l in self.refsDic.itervalues():
                    for e in l.queue:
                        error += "REF:\n" + str(e) + "\n"

                for l in self.uutsDic.itervalues():
                    for e in l.queue:
                        error += "UUT:\n" + str(e) + "\n"

                cocotb.log.error(error)


    def endPhase(self, phase):
        Infrastructure.endPhase(self, phase)
        if phase == PHASE_CHECK_SCORBOARDS:
            if len(self.refsDic) != 0 or len(self.uutsDic) != 0:
                raise TestFailure("Scoreboard not empty")




