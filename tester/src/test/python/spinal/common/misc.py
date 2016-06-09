import random

import cocotb
from cocotb.result import TestFailure
from cocotb.triggers import Timer

def randBool():
    return bool(random.getrandbits(1))

def randBits(width):
    return random.getrandbits(width)

def randSignal(that):
    that <= random.getrandbits(len(that))

def assertEguality(a,b,name):
    if int(a) != int(b):
        raise TestFailure("FAIL %s    %d != %d" % (name,int(a),int(b)))

def setBit(v, index, x):
  mask = 1 << index
  v &= ~mask
  if x:
    v |= mask
  return v

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
