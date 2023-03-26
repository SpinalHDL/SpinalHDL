import random
import cocotb
from cocotb.triggers import Timer

@cocotb.coroutine
async def try_tri(pad, write, writeEnable, read):
    val = random.getrandbits(1) == 1
    we = random.getrandbits(1) == 1
    writeEnable.value = we
    write.value = val
    await Timer(10)
    #print(get_sim_time(), pad.value, write.value, writeEnable.value, read.value, pad.name)
    if not we:
        assert(str(pad.value).lower()[-1] == 'z')
    elif val:
        assert(str(pad.value).lower()[-1] == '1')
    else:
        assert(str(pad.value).lower()[-1] == '0')


@cocotb.test()
async def test_tri_side(dut):
    bundles = [
        (dut.simple_driver_pad, dut.simple_driver_tri_write, dut.simple_driver_tri_writeEnable, dut.simple_driver_tri_read),
        (dut.analog_bus_pad, dut.analog_bus_tri_write, dut.analog_bus_tri_writeEnable, dut.analog_bus_tri_read),
        (dut.blackbox_pad, dut.blackbox_tri_write, dut.blackbox_tri_writeEnable, dut.blackbox_tri_read),
        (dut.sliced_bus_pad, dut.sliced_bus_tri_write, dut.sliced_bus_tri_writeEnable, dut.sliced_bus_tri_read),
    ]
    # don't deposit to PAD signals - which messes up at least VHDL simulation
    for i in range(1000):
        pad, write, writeEnable, read = random.choice(bundles)
        await try_tri(pad, write, writeEnable, read)
