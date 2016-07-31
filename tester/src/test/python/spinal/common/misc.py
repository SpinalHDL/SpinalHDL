import random

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer, RisingEdge


def randInt(min,max):
    return random.randint(min, max)

def randBool():
    return bool(random.getrandbits(1))

def randBits(width):
    return random.getrandbits(width)

def randSignal(that):
    that <= random.getrandbits(len(that))

def randBoolSignal(that,prob):
    that <= (random.random() < prob)


def assertEquals(a, b, name):
    if int(a) != int(b):
        raise TestFailure("FAIL %s    %d != %d" % (name,int(a),int(b)))

def truncUInt(value, signal):
    if isinstance( signal, int ):
        return value & ((1 << signal)-1)
    else:
        return value & ((1 << len(signal)) - 1)

def truncSInt(value, signal):
    if isinstance( signal, int ):
        bitCount = signal
    else:
        bitCount = len(signal)
    masked = value & ((1 << bitCount)-1)
    if (masked & (1 << bitCount-1)) != 0:
        return - (1 << bitCount) + masked
    else:
        return masked


def setBit(v, index, x):
  mask = 1 << index
  v &= ~mask
  if x:
    v |= mask
  return v

def uint(signal):
    return signal.value.integer

def sint(signal):
    return signal.value.signed_integer


@cocotb.coroutine
def ClockDomainAsyncReset(clk,reset):
    if reset:
        reset <= 1
    clk <= 0
    yield Timer(1000)
    if reset:
        reset <= 0
    while True:
        clk <= 0
        yield Timer(500)
        clk <= 1
        yield Timer(500)



import time;
@cocotb.coroutine
def simulationSpeedPrinter(clk):
    counter = 0
    lastTime = time.time()
    while True:
        yield RisingEdge(clk)
        counter += 1
        thisTime = time.time()
        if thisTime - lastTime >= 1.0:
            lastTime = thisTime
            print("Sim speed : %f khz" %(counter/1000.0))
            counter = 0



class BoolRandomizer:
    def __init__(self):
        self.prob = 0.5
        self.counter = 0

    def get(self):
        self.counter += 1
        if self.counter == 100:
            self.counter = 0
            self.prob = random.uniform(0.1, 0.9)
        return random.random() < self.prob


# class Stream:
#     def __init__(self,name,dut):
#         self.valid = getattr(dut, name + "_valid")
#         self.ready = getattr(dut, name + "_ready")
#         payloads = [a for a in dut if a._name.startswith(name + "_payload")]
#         if len(payloads) == 1 and payloads[0]._name == name + "_payload":
#             self.payload = payloads[0]



MyObject = type('MyObject', (object,), {})

@cocotb.coroutine
def StreamRandomizer(streamName, onNew,handle, dut, clk):
    validRandomizer = BoolRandomizer()
    valid = getattr(dut, streamName + "_valid")
    ready = getattr(dut, streamName + "_ready")
    payloads = [a for a in dut if a._name.startswith(streamName + "_payload")]

    valid <= 0
    while True:
        yield RisingEdge(clk)
        if int(ready) == 1:
            valid <= 0

        if int(valid) == 0 or int(ready) == 1:
            if validRandomizer.get():
                valid <= 1
                for e in payloads:
                    randSignal(e)
                yield Timer(1)
                if len(payloads) == 1 and payloads[0]._name == streamName + "_payload":
                    payload = int(payloads[0])
                else:
                    payload = MyObject()
                    for e in payloads:
                        payload.__setattr__(e._name[len(streamName + "_payload_"):], int(e))
                if onNew:
                    onNew(payload,handle)

@cocotb.coroutine
def FlowRandomizer(streamName, onNew,handle, dut, clk):
    validRandomizer = BoolRandomizer()
    valid = getattr(dut, streamName + "_valid")
    payloads = [a for a in dut if a._name.startswith(streamName + "_payload")]

    valid <= 0
    while True:
        yield RisingEdge(clk)
        if validRandomizer.get():
            valid <= 1
            for e in payloads:
                randSignal(e)
            yield Timer(1)
            if len(payloads) == 1 and payloads[0]._name == streamName + "_payload":
                payload = int(payloads[0])
            else:
                payload = MyObject()
                for e in payloads:
                    payload.__setattr__(e._name[len(streamName + "_payload_"):], int(e))
            if onNew:
                onNew(payload,handle)
        else:
            valid <= 0

@cocotb.coroutine
def StreamReader(streamName, onTransaction, handle, dut, clk):
    validRandomizer = BoolRandomizer()
    valid = getattr(dut, streamName + "_valid")
    ready = getattr(dut, streamName + "_ready")
    payloads = [a for a in dut if a._name.startswith(streamName + "_payload")]

    ready <= 0
    while True:
        yield RisingEdge(clk)
        ready <= validRandomizer.get()
        if int(valid) == 1 and int(ready) == 1:
            if len(payloads) == 1 and payloads[0]._name == streamName + "_payload":
                payload = int(payloads[0])
            else:
                payload = MyObject()
                for e in payloads:
                    payload.__setattr__(e._name[len(streamName + "_payload_"):], int(e))

            if onTransaction:
                onTransaction(payload,handle)

