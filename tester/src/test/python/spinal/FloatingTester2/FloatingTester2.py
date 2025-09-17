import cocotb
from cocotb.triggers import Timer
from decimal import Decimal

from spinal.FloatingTestCommon import f32_to_bits, get_recfloat_fields

async def check_conversion(dut, bd_str, py_float_val, f_bits_signal_name, rf_sign_signal_name, rf_exp_signal_name, rf_mant_signal_name):
    bd_val = Decimal(bd_str)
    prefix = "io_"
    expected_f_bits = f32_to_bits(py_float_val)
    try:
        actual_f_bits_signal = getattr(dut, prefix + f_bits_signal_name)
        actual_f_bits = actual_f_bits_signal.value.integer
    except AttributeError:
        assert False, f"DUT signal dut.{prefix + f_bits_signal_name} not found!"

    assert actual_f_bits == expected_f_bits, \
        f"Floating['{bd_str}']: Expected 0x{expected_f_bits:08X}, Got 0x{actual_f_bits:08X}"

    expected_rf = get_recfloat_fields(bd_val)

    assert expected_rf is not None, \
        f"Could not calculate expected RecFloat fields for {bd_str}"

    try:
        actual_rf_sign_signal = getattr(dut, prefix + rf_sign_signal_name)
        actual_rf_exp_signal  = getattr(dut, prefix + rf_exp_signal_name)
        actual_rf_mant_signal = getattr(dut, prefix + rf_mant_signal_name)

        actual_rf_sign = actual_rf_sign_signal.value.integer
        actual_rf_exp  = actual_rf_exp_signal.value.integer
        actual_rf_mant = actual_rf_mant_signal.value.integer
    except AttributeError as e:
        assert False, f"DUT signal access error: {e}"


    # Compare fields
    assert actual_rf_sign == expected_rf['sign'], \
        f"RecFloating['{bd_str}'] Sign: Expected {expected_rf['sign']}, Got {actual_rf_sign}"

    assert actual_rf_exp == expected_rf['exp'], \
        f"RecFloating['{bd_str}'] Exp: Expected 0x{expected_rf['exp']:03X}, Got 0x{actual_rf_exp:03X}"

    assert actual_rf_mant == expected_rf['mant'], \
        f"RecFloating['{bd_str}'] Mant: Expected 0x{expected_rf['mant']:06X}, Got 0x{actual_rf_mant:06X}"

@cocotb.test()
async def bigdecimal_conversion_test(dut):
    """Test BigDecimal to Floating and RecFloating assignments."""
    await Timer(10, units="ns") # Allow signals to settle

    test_data = [
        # ("<bd_str>",  <py_float>,    "<f_signal>",          "<rf_s_sig>",           "<rf_e_sig>",                "<rf_m_sig>")
        ("0.0",          0.0,          "f_zero_bits",          "rf_zero_sign",          "rf_zero_exp",          "rf_zero_mant"),
        ("1.5",          1.5,          "f_one_point_five_bits","rf_one_point_five_sign","rf_one_point_five_exp","rf_one_point_five_mant"),
        ("-2.0",         -2.0,         "f_neg_two_bits",       "rf_neg_two_sign",       "rf_neg_two_exp",       "rf_neg_two_mant"),
        ("0.125",        0.125,        "f_one_eighth_bits",    "rf_one_eighth_sign",    "rf_one_eighth_exp",    "rf_one_eighth_mant"),
        ("0.0009765625", 0.0009765625, "f_small_val_bits",     "rf_small_val_sign",     "rf_small_val_exp",     "rf_small_val_mant"),
    ]

    for test_case in test_data:
        bd_s, py_f, f_sig, rf_s_sig, rf_e_sig, rf_m_sig = test_case
        await check_conversion(dut, bd_s, py_f, f_sig, rf_s_sig, rf_e_sig, rf_m_sig)

    dut._log.info("All BigDecimal conversion checks passed.")
