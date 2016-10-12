import cocotb
from cocotb.result import ReturnValue
from cocotb.triggers import RisingEdge

from cocotblib.misc import assertEquals


@cocotb.coroutine
def readCoreValue(dut):
    bridge = dut.axi_apbBridge
    while True:
        yield RisingEdge(dut.io_axiClk)
        if int(bridge.io_apb_PENABLE) == 1 and int(bridge.io_apb_PSEL) == 1 and int(bridge.io_apb_PWRITE) == 1 and int(bridge.io_apb_PADDR) == 0xFFF00:
            raise ReturnValue(int(bridge.io_apb_PWDATA))

@cocotb.coroutine
def readCoreValueAssert(dut,value,message):
    print(message)
    rsp = yield readCoreValue(dut)
    assertEquals(rsp,value,message)