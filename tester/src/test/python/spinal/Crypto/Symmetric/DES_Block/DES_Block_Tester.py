import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge

from spinal.common.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL
from spinal.common.Stream import Stream
from spinal.common.Flow import Flow


from spinal.common.misc import assertEquals, randInt

from spinal.crypto.symmetric.pyDES import *

###############################################################################
# DES Block Helper
#
class DES_Block_Helper:

    def __init__(self,dut):

        # IO definition -----------------------------------
        self.io = DES_Block_Helper.IO(dut)


    #==========================================================================
    # Rename IO
    #==========================================================================
    class IO:

        def __init__ (self, dut):
            self.cmd    = Stream(dut, "io_cmd")
            self.rsp    = Flow(dut, "io_res")
            self.clk    = dut.clk
            self.resetn = dut.resetn

        def init(self):
            self.cmd.valid         <= 0
            self.cmd.payload.block <= 0
            self.cmd.payload.key   <= 0


class KeyTmp:


    def __init__(self):
       self.__pc1 = [57, 49, 41, 33, 25, 17,  9,  1, 58, 50, 42, 34, 26, 18,
                     10,  2, 59, 51, 43, 35, 27, 19, 11,  3, 60, 52, 44, 36,
                     63, 55, 47, 39, 31, 23, 15,  7, 62, 54, 46, 38, 30, 22,
                     14,  6, 61, 53, 45, 37, 29, 21, 13,  5, 28, 20, 12,  4]


    def keyDropParity(self, binList):

        """Permutate this block with the specified table"""
        return list(map(lambda x: binList[x], self.__pc1))


def int_2_String(integer):

    kesList = [int(x) for x in '{0:064b}'.format(integer)]#[::-1]

    k = des("DESCRYPT", CBC, "\0\0\0\0\0\0\0\0", pad=None, padmode=PAD_PKCS5)
    return k.bitList2String(kesList)


###############################################################################
# Test DES Block
#
@cocotb.test()
def test_DES_Block(dut):

    dut.log.info("Cocotb test DES Block")

    helperDES    = DES_Block_Helper(dut)
    clockDomain  = ClockDomain(helperDES.io.clk, 200, helperDES.io.resetn , RESET_ACTIVE_LEVEL.LOW)

    # Start clock
    cocotb.fork(clockDomain.start())

    # Init IO and wait the end of the reset
    helperDES.io.init()
    yield clockDomain.event_endReset.wait()


    key  = 0xAABB09182736CCDD
    data = 0x123456ABCD132536

    #
    helperDES.io.cmd.valid         <= 1
    helperDES.io.cmd.payload.key   <= key #keyDrop
    helperDES.io.cmd.payload.block <= data


    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)
    yield RisingEdge(helperDES.io.clk)


    helperDES.io.cmd.valid         <= 0

    yield RisingEdge(helperDES.io.clk)


    # model DES
    #data = "Please encrypt my data"
    #sdata =
    k    = des(int_2_String(key), CBC, "\0\0\0\0\0\0\0\0", pad=None, padmode=PAD_PKCS5)


    print("Encrypted message", (k.encrypt(int_2_String(data))).encode('hex') )


    dut.log.info("Cocotb end test DES Block")