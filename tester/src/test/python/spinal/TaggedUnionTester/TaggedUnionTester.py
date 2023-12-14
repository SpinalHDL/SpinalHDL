import cocotb
from cocotb.triggers import Timer

from cocotblib.misc import randSignal, assertEquals


class Ref:
    def __init__(self,dut):
        # localparam SpinalEnum_a1 = 2'd0;
        # localparam SpinalEnum_a2 = 2'd1;
        # localparam SpinalEnum_b = 2'd2;

        # localparam SpinalEnum_1_b = 1'd0;
        # localparam SpinalEnum_1_c = 1'd1;

        if int(dut.io_i_tag) == 0: #a1
            self.io_o_tag = 1 #c
            self.io_o_unionPayload = int(dut.io_i_unionPayload) & 0xFF
        elif int(dut.io_i_tag) == 1:#a2
            self.io_o_tag = 1 #c
            self.io_o_unionPayload = (int(dut.io_i_unionPayload) >> 8) & 0xFF
        else:#b
            self.io_o_tag = 0 #b
            self.io_o_unionPayload = int(dut.io_i_unionPayload)



@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot TaggedUnion")
    #random.seed(0)

    # module TaggedUnionTester (
    #     input  wire [15:0]   io_i_unionPayload,
    #     input  wire [1:0]    io_i_tag,
    #     output reg  [10:0]   io_o_unionPayload,
    #     output reg  [0:0]    io_o_tag
    # )


    for i in range(0,1000):
        randSignal(dut.io_i_tag)
        randSignal(dut.io_i_unionPayload)
        yield Timer(1000)
        ref = Ref(dut)
        assertEquals(ref.io_o_tag, dut.io_o_tag, "io_o_tag")
        assertEquals(ref.io_o_unionPayload, dut.io_o_unionPayload, "io_o_unionPayload")

    dut.log.info("Cocotb test done")
