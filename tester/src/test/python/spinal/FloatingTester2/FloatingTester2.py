import cocotb
from cocotb.triggers import Timer
from cocotb.result import TestFailure
import struct
from decimal import Decimal

from spinal.FloatingTestCommon import f32_to_bits, get_recfloat_fields


@cocotb.test()
async def bigdecimal_conversion_test(dut):
    """Test BigDecimal to Floating and RecFloating assignments"""
    await Timer(10, units="ns")

    test_cases = [
        ("0.0", 0.0),
        ("1.5", 1.5)
    ]

    # Test Case 0.0
    bd_str, py_float_val = test_cases[0]
    dut._log.info(f"Testing BigDecimal: {bd_str}")

    # Floating
    expected_f_bits = f32_to_bits(py_float_val)
    actual_f_bits = dut.io_f_zero_bits.value.integer
    if actual_f_bits != expected_f_bits:
        raise TestFailure(
            f"Floating['{bd_str}']: Expected 0x{expected_f_bits:08X}, Got 0x{actual_f_bits:08X}")

    # RecFloating
    expected_rf = get_recfloat_fields(Decimal(bd_str))
    if expected_rf:
        if dut.io_rf_zero_sign.value.integer != expected_rf['sign']:
            raise TestFailure(
                f"RecFloating['{bd_str}'] Sign: Expected {expected_rf['sign']}, Got {dut.io_rf_zero_sign.value.integer}")
        if dut.io_rf_zero_exp.value.integer != expected_rf['exp']:
            raise TestFailure(
                f"RecFloating['{bd_str}'] Exp: Expected {expected_rf['exp']}, Got {dut.io_rf_zero_exp.value.integer}")
        if dut.io_rf_zero_mant.value.integer != expected_rf['mant']:
            raise TestFailure(
                f"RecFloating['{bd_str}'] Mant: Expected {expected_rf['mant']}, Got {dut.io_rf_zero_mant.value.integer}")
    else:
        dut._log.warning(
            f"RecFloating['{bd_str}']: No pre-calculated expectation. Sign={dut.io_rf_zero_sign.value}, Exp={dut.io_rf_zero_exp.value}, Mant={dut.io_rf_zero_mant.value}")

    # Test Case 1.5
    bd_str, py_float_val = test_cases[1]
    dut._log.info(f"Testing BigDecimal: {bd_str}")

    expected_f_bits = f32_to_bits(py_float_val)
    actual_f_bits = dut.io_f_one_point_five_bits.value.integer
    if actual_f_bits != expected_f_bits:
        dut._log.warning(
            f"Floating['{bd_str}']: Expected (standard IEEE) 0x{expected_f_bits:08X}, Got (DUT logic) 0x{actual_f_bits:08X}")
        if actual_f_bits != 0x3FC00000:  # Standard for 1.5f
            raise TestFailure(
                f"Floating['{bd_str}']: Expected 0x3FC00000, Got 0x{actual_f_bits:08X}")

    expected_rf = get_recfloat_fields(Decimal(bd_str))
    if expected_rf:
        if dut.io_rf_one_point_five_sign.value.integer != expected_rf['sign']:
            raise TestFailure(
                f"RecFloating['{bd_str}'] Sign: Expected {expected_rf['sign']}, Got {dut.io_rf_one_point_five_sign.value.integer}")
        if dut.io_rf_one_point_five_exp.value.integer != expected_rf['exp']:
            raise TestFailure(
                f"RecFloating['{bd_str}'] Exp: Expected 0x{expected_rf['exp']:X}, Got 0x{dut.io_rf_one_point_five_exp.value.integer:X}")
        if dut.io_rf_one_point_five_mant.value.integer != expected_rf['mant']:
            raise TestFailure(
                f"RecFloating['{bd_str}'] Mant: Expected 0x{expected_rf['mant']:X}, Got 0x{dut.io_rf_one_point_five_mant.value.integer:X}")
    else:
        dut._log.warning(
            f"RecFloating['{bd_str}']: No pre-calculated expectation. Sign={dut.io_rf_one_point_five_sign.value}, Exp={dut.io_rf_one_point_five_exp.value}, Mant={dut.io_rf_one_point_five_mant.value}")
