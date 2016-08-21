import random
from Queue import Queue

import cocotb
from cocotb.result import TestFailure, TestSuccess
from cocotb.triggers import Timer, Edge, RisingEdge, Join, FallingEdge

from spinal.PinsecTest.HexLoader import loadIHex
from spinal.PinsecTest.Jtag import JtagMaster
from spinal.common.AhbLite3 import AhbLite3MasterDriver, AhbLite3SlaveMemory, AhbLite3MasterIdle, AhbLite3TraficGenerator, AhbLite3MasterReadChecker, AhbLite3Terminaison
from spinal.common.misc import setBit, randSignal, assertEquals, truncUInt, sint, ClockDomainAsyncReset, randBoolSignal, \
    BoolRandomizer, StreamRandomizer,StreamReader, FlowRandomizer, Bundle, simulationSpeedPrinter, readIHex


