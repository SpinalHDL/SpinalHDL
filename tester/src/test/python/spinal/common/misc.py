import random

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer, RisingEdge


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

def truncInt(value,signal):
    return value & ((1 << len(signal))-1)

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




# print(ret.io_outRegComplex)
#     print("LEN = " + str(len(dut.io_outRegComplex)))
