import math

import cocotb
from cocotb.triggers import Timer
from cocotb.binary import BinaryValue

from cocotblib.misc import randSignal, assertEquals, truncUInt, sint, uint


class Ref:

    ops = ["+", "-", "*", "/", "%"]
    rounds = ["ceil", "floor", "zero", "inf", "halfUp", "halfDown", "halfZero", "halfInf", "halfEven", "halfOdd"]

    def __init__(self,dut):
        print(f"inFix0 = {dut.io_inFix_0}")
        print(f"inFix1 = {dut.io_inFix_1}")
        print(f"opMode = {dut.io_opMode}")
        print(f"roundMode = {dut.io_opMode}")
        self.io_inFix_0 = sint(dut.io_inFix_0)/(2**4)
        self.io_inFix_1 = sint(dut.io_inFix_1)/(2**6)
        self.io_outFix = sint(dut.io_outFix)
        opMode = uint(dut.io_opMode)
        roundMode = uint(dut.io_roundMode)

        opResult = 0.0
        if opMode == 0:
            opResult = self.io_inFix_0 + self.io_inFix_1
        elif opMode == 1:
            opResult = self.io_inFix_0 - self.io_inFix_1
        elif opMode == 2:
            opResult = self.io_inFix_0 * self.io_inFix_1
        elif opMode == 3:
            opResult = self.io_inFix_0 / self.io_inFix_1
        elif opMode == 4:
            opResult = self.io_inFix_0 % self.io_inFix_1

        print(f"Ref: {self.io_inFix_0} {self.ops[opMode]} {self.io_inFix_1} = {opResult}")

        roundResult = 0.0
        if roundMode == 0:
            # Ceil
            roundResult = math.ceil(opResult)
        elif roundMode == 1:
            # Floor
            roundResult = math.floor(opResult)
        elif roundMode == 2:
            # Floor to zero
            if (opResult >= 0):
                roundResult = math.floor(opResult)
            else:
                roundResult = math.ceil(opResult)
        elif roundMode == 3:
            # Ceil to infinity
            if (opResult >= 0):
                roundResult = math.ceil(opResult)
            else:
                roundResult = math.floor(opResult)
        elif roundMode == 4:
            # Half up
            roundResult = math.floor(opResult + 0.5)
        elif roundMode == 5:
            # Half down
            roundResult = math.ceil(opResult - 0.5)
        elif roundMode == 6:
            # Half to zero
            roundResult = math.ceil(abs(opResult) - 0.5)
            if opResult < 0:
                roundResult = -roundResult
        elif roundMode == 7:
            # Half to infinity
            roundResult = math.floor(abs(opResult) + 0.5)
            if opResult < 0:
                roundResult = -roundResult
        elif roundMode == 8:
            # Half to even
            frac_msb = math.trunc(opResult*10.0) % 10
            whole_lsb = math.trunc(opResult*10.0)
            if (frac_msb == 5):
                # Make the number even
                roundResult = abs(math.trunc(opResult)) + (whole_lsb % 2)
            else:
                # Half to zero
                roundResult = math.ceil(abs(opResult) - 0.5)
            if opResult < 0:
                roundResult = -roundResult
        elif roundMode == 9:
            # Half to odd
            frac_msb = math.trunc(opResult*10.0) % 10
            whole_lsb = math.trunc(opResult*10.0)
            if (frac_msb == 5):
                # Make the number even
                roundResult = abs(math.trunc(opResult)) + (1 - whole_lsb % 2)
            else:
                # Half to infinity
                roundResult = math.floor(abs(opResult) + 0.5)
            if opResult < 0:
                roundResult = -roundResult

        print(f"Ref: {self.rounds[roundMode]}({opResult}) = {roundResult}")

        self.io_outFix_expected = roundResult


ops = ["+", "-", "*", "/", "%"]
rounds = ["ceil", "floor", "zero", "inf", "halfUp", "halfDown", "halfZero", "halfInf", "halfEven", "halfOdd"]


def check_results(dut, op, mode):
    ref = Ref(dut)
    op_str = f"{dut.io_inFix_0} {ops[op]} {dut.io_inFix_1}"
    print(f"{op_str} = {dut.io_outRaw}")
    round_str = f"{rounds[mode]}(ans)"
    print(f"{round_str:>{len(op_str)}} = {dut.io_outFix}")
    print(f"opRaw = {sint(dut.io_outRaw)/(2**12)}")
    print(f"Result = {sint(dut.io_outFix)}. Expected = {ref.io_outFix_expected}")
    # assertEquals(ref.io_outFix, ref.io_outFix_expected, "io_outFix")


def to_sint(i: int):
    if i >= 0:
        return i
    else:
        return (1 << i.bit_length())-(abs(i)-1)


@cocotb.test()
def test1(dut):
    dut.log.info("Cocotb test boot")
    #random.seed(0)

    # for op in range(0,4):
    #     print(f"Op = {op}")
    #     for mode in range(0,10):
    #         print(f"Mode = {mode}")
    #         for i in range(0,10):
    #             print("")
    #             # dut.io_inFix_0.value = -1
    #             # dut.io_inFix_1.value = 0
    #             randSignal(dut.io_inFix_0)
    #             randSignal(dut.io_inFix_1)
    #             dut.io_opMode.value = op
    #             dut.io_roundMode.value = mode
    #             yield Timer(1000)
    #             check_results(dut, op=op, mode=mode)

    for mode in range(0,10):
        dut.io_roundMode.value = mode
        dut.io_opMode.value = 0
        # Specific cases to check the half rounding
        for i in [-2.75, -2.5, -2.25, -2, -1.75, -1.5, -1.25, -1, -0.75, -0.5, -0.25,
                  0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5, 2.75]:
            dut.io_inFix_0.value = math.trunc(i*(2**4))
            dut.io_inFix_1.value = 0
            yield Timer(1000)
            check_results(dut, op=0, mode=mode)

    dut.log.info("Cocotb test done")
