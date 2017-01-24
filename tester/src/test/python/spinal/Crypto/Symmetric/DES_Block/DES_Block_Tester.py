import cocotb
from cocotb.triggers import Timer, Edge, RisingEdge

from cocotblib.ClockDomain import ClockDomain, RESET_ACTIVE_LEVEL
from cocotblib.Stream import Stream
from cocotblib.Flow import Flow
from cocotblib.misc import randBits, assertEquals

from spinal.Crypto.Symmetric.pyDES import *


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
            self.rsp    = Flow(dut, "io_rsp")
            self.clk    = dut.clk
            self.resetn = dut.resetn

        def init(self):
            self.cmd.valid          <= 0
            self.cmd.payload.block  <= 0
            self.cmd.payload.key    <= 0
            self.cmd.payload.enc    <= 0



###############################################################################
# Convert an integer to a String
#
def int_2_String(integer):

    kesList = [int(x) for x in '{0:064b}'.format(integer)]

    k = des("DESCRYPT", CBC, "\0\0\0\0\0\0\0\0", pad=None, padmode=PAD_PKCS5)
    return k.bitList2String(kesList)


###############################################################################
# Test DES Block
#
@cocotb.test()
def test_DES_Block(dut):

    dut.log.info("Cocotb test DES Block")
    from cocotblib.misc import cocotbXHack
    cocotbXHack()

    helperDES    = DES_Block_Helper(dut)
    clockDomain  = ClockDomain(helperDES.io.clk, 200, helperDES.io.resetn , RESET_ACTIVE_LEVEL.LOW)

    # Start clock
    cocotb.fork(clockDomain.start())

    # Init IO and wait the end of the reset
    helperDES.io.init()
    yield clockDomain.event_endReset.wait()

    # start monitoring the Valid signal
    helperDES.io.rsp.startMonitoringValid(helperDES.io.clk)


    for _ in range(0,5):

        # Vector test ...
        #key  = 0xAABB09182736CCDD
        #data = 0x123456ABCD132536
        #data = 0xC0B7A8D05F3A829C

        # Gen random value
        key    = randBits(64)
        data   = randBits(64)

        # Encrpytion
        helperDES.io.cmd.valid          <= 1
        helperDES.io.cmd.payload.key    <= key
        helperDES.io.cmd.payload.block  <= data
        helperDES.io.cmd.payload.enc    <= 1  # do an encryption


        # Wait the end of the process and read the result
        yield helperDES.io.rsp.event_valid.wait()

        rtlEncryptedBlock = int(helperDES.io.rsp.event_valid.data.block)

        #print("RTL encrypted", hex(rtlEncryptedBlock))

        helperDES.io.cmd.valid         <= 0

        yield RisingEdge(helperDES.io.clk)

        # Encrpytion
        helperDES.io.cmd.valid          <= 1
        helperDES.io.cmd.payload.key    <= key
        helperDES.io.cmd.payload.block  <= rtlEncryptedBlock
        helperDES.io.cmd.payload.enc    <= 0 # do a decryption


        # Wait the end of the process and read the result
        yield helperDES.io.rsp.event_valid.wait()

        rtlDecryptedBlock = int(helperDES.io.rsp.event_valid.data.block)

        #print("RTL decrypted", hex(rtlDecryptedBlock))

        helperDES.io.cmd.valid         <= 0

        yield RisingEdge(helperDES.io.clk)

        # Encrypted data with the model
        k    = des(int_2_String(key), CBC, "\0\0\0\0\0\0\0\0", pad=None, padmode=PAD_PKCS5)
        refEncryptedOutput = (k.encrypt(int_2_String(data))).encode('hex')[:16]


        # print("Ref encrypted ", refEncryptedOutput)

        # compare result
        assertEquals(int(refEncryptedOutput, 16), rtlEncryptedBlock, "Encryption data wrong ")
        assertEquals(rtlDecryptedBlock, data, "Decryption data wrong ")


    dut.log.info("Cocotb end test DES Block")