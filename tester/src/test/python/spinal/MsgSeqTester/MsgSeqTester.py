import cocotb
from cocotb.triggers import Timer
from cocotb.result import TestFailure


async def set_packet_header(dut, length, ptype, value, checksum):
    dut.io_packetIn_packetLength.value = length
    dut.io_packetIn_packetType.value = ptype
    dut.io_packetIn_payload_value.value = value
    dut.io_packetIn_payload_checksum.value = checksum


def assert_equals(actual, expected, message=""):
    if actual != expected:
        raise TestFailure(
            f"Assertion failed: {message} - Expected {expected}, got {actual}")
    else:
        cocotb.log.info(
            f"Assertion passed: {message} - Expected {expected}, got {actual}")


@cocotb.test()
async def test_simple_packet_processor(dut):
    dut._log.info("Cocotb test boot for SimplePacketProcessor (MsgSeqTester)")

    dut.clk.value = 0
    dut.reset.value = 0

    dut._log.info("Initializing inputs...")
    dut.io_controlSignal.value = 0
    await set_packet_header(dut, length=0, ptype=0, value=0, checksum=0)

    await Timer(1, units="ps")

    # Initial State Check
    cocotb.log.info(f"Raw value of io_isValid.value: <{dut.io_isValid.value}>")
    cocotb.log.info(f"Type of io_isValid.value: {type(dut.io_isValid.value)}")
    cocotb.log.info(
        f"Integer value of io_isValid.value: {dut.io_isValid.value.integer}")

    assert_equals(dut.io_isValid.value.integer, 0,
                  "Initially, isValid should be false")
    assert_equals(dut.io_processedValue.value.integer, 0,
                  "Initially, processedValue should be 0")
    dut._log.info(
        f"Initial state: isValid={dut.io_isValid.value.integer}, processedValue={dut.io_processedValue.value.integer}")

    # Test Case: Control Signal Disabled
    dut.io_controlSignal.value = 0
    await set_packet_header(dut, length=10, ptype=1, value=0xABCD, checksum=0xCD)
    await Timer(1, units="ps")

    assert_equals(dut.io_isValid.value.integer, 0,
                  "isValid should be false when controlSignal is disabled")
    assert_equals(dut.io_processedValue.value.integer, 0,
                  "processedValue should be 0 when controlSignal is disabled")

    # Test Case: Control Enabled, Type Mismatch
    dut.io_controlSignal.value = 1
    await set_packet_header(dut, length=10, ptype=2, value=0x1234, checksum=0x34)
    await Timer(1, units="ps")

    assert_equals(dut.io_isValid.value.integer, 0,
                  "isValid should be false when packetType mismatches")
    assert_equals(dut.io_processedValue.value.integer, 0,
                  "processedValue should be 0 when packetType mismatches")

    # Test Case: Valid Packet
    valid_value = 0x5678
    valid_checksum = 0x78
    dut.io_controlSignal.value = 1
    await set_packet_header(dut, length=12, ptype=1, value=valid_value, checksum=valid_checksum)
    await Timer(1, units="ps")

    assert_equals(dut.io_isValid.value.integer, 1,
                  "isValid should be true for a valid packet")
    assert_equals(dut.io_processedValue.value.integer, valid_value,
                  "processedValue should match input value for a valid packet")

    # Test Case: Checksum Mismatch
    invalid_value = 0xABCD
    wrong_checksum = 0xCC
    dut.io_controlSignal.value = 1
    await set_packet_header(dut, length=10, ptype=1, value=invalid_value, checksum=wrong_checksum)
    await Timer(1, units="ps")

    assert_equals(dut.io_isValid.value.integer, 0,
                  "isValid should be false when checksum mismatches")
    assert_equals(dut.io_processedValue.value.integer, 0,
                  "processedValue should be 0 when checksum mismatches")

    dut._log.info("Cocotb test done for SimplePacketProcessor")
