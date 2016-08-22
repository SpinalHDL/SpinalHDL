import cocotb
from cocotb.triggers import Timer


@cocotb.coroutine
def loadIHexCallback(address,array,dut,clk):
    assert(address & 3 == 0)
    assert(len(array) & 3 == 0)
    data = 0
    for b in array:
        data |= b << ((address & 3) * 8)
        if (address & 3 == 3):
            yield Timer(5)
            dut.axi_rom.ram_port0_write <= 1
            dut.axi_rom.ram_port0_enable <= 1
            dut.axi_rom.ram_port0_mask <= 0xF
            dut.axi_rom.ram_port0_address <= (address) >> 2
            dut.axi_rom.ram_port0_writeData <= data
            data = 0
            clk <= 0
            yield Timer(5)
            clk <= 1
            yield Timer(5)
            dut.axi_rom.ram_port0_enable <= 0
            yield Timer(5)
        address += 1



@cocotb.coroutine
def loadIHex(dut,hexPath,clk,reset):

    reset <= 1
    yield Timer(5)
    writeBuffer = int(dut.axi_rom.ram_port0_write)
    enableBuffer = int(dut.axi_rom.ram_port0_enable)
    maskBuffer = int(dut.axi_rom.ram_port0_mask)
    addressBuffer = int(dut.axi_rom.ram_port0_address)
    writeDataBuffer = int(dut.axi_rom.ram_port0_writeData)


    # readIHex(hexPath,loadIHexCallback,dut)
    with open(hexPath) as f:
        offset = 0
        for line in f:
            if len(line) > 0:
                assert line[0] == ':'
                byteCount = int(line[1:3], 16)
                nextAddr = int(line[3:7], 16) + offset
                key = int(line[7:9], 16)
                if key == 0:
                    array = [int(line[9 + i * 2:11 + i * 2], 16) for i in range(0, byteCount)]
                    yield loadIHexCallback(nextAddr,array,dut,clk)
                elif key == 2:
                    offset = int(line[9:13], 16)
                else:
                    pass
    reset <= 0
    yield Timer(5)
    reset <= 1

    dut.axi_rom.ram_port0_write <= writeBuffer
    dut.axi_rom.ram_port0_enable <= enableBuffer
    dut.axi_rom.ram_port0_mask <= maskBuffer
    dut.axi_rom.ram_port0_address <= addressBuffer
    dut.axi_rom.ram_port0_writeData <= writeDataBuffer
