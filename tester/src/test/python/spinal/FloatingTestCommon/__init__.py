
import math
import struct
from decimal import Decimal

# IEEE 754 SP Consts
IEEE_EXP_BITS = 8
IEEE_MANT_BITS = 23
IEEE_BIAS = (1 << (IEEE_EXP_BITS - 1)) - 1

# RecFloating32 Consts
REC_EXP_BITS = 9
REC_MANT_BITS = 23
REC_EXP_ADD_NORMAL = (1 << (REC_EXP_BITS - 2)) | 1
REC_EXP_ADD_SUBNORMAL = (1 << (REC_EXP_BITS - 2)) | 3
REC_EXP_MAX_VAL = (1 << REC_EXP_BITS) - 1


def f32_to_bits(f_val):
    return struct.unpack('>I', struct.pack('>f', float(f_val)))[0]


def get_ieee754_single_fields(val):
    """
    Converts a Python float or Decimal into IEEE 754 single-precision fields.
    Handles normal, subnormal, zero, inf, nan.

    Returns: dict {'sign': int, 'exp_field': int, 'mant_field': int, 'category': str}
             category is 'zero', 'subnormal', 'normal', 'inf', 'nan'
    """
    if isinstance(val, Decimal):

        try:
            f_val = float(val)
        except (OverflowError, ValueError):

            if val.is_zero():
                f_val = 0.0
            elif val > 0:
                f_val = float('inf')
            else:
                f_val = float('-inf')

    elif isinstance(val, (float, int)):
        f_val = float(val)
    else:
        raise TypeError("Input must be float, int, or Decimal")

    if math.isnan(f_val):

        bits = struct.unpack('>I', struct.pack('>f', f_val))[0]
        sign = (bits >> 31) & 1
        exp_field = (bits >> 23) & 0xFF
        mant_field = bits & 0x7FFFFF

        if mant_field == 0:
            mant_field = 1 << (IEEE_MANT_BITS - 1)
        return {'sign': sign, 'exp_field': 0xFF, 'mant_field': mant_field, 'category': 'nan'}

    if math.isinf(f_val):
        sign = 1 if f_val < 0 else 0
        return {'sign': sign, 'exp_field': 0xFF, 'mant_field': 0, 'category': 'inf'}

    if f_val == 0.0:
        sign = 1 if math.copysign(1.0, f_val) < 0 else 0
        return {'sign': sign, 'exp_field': 0, 'mant_field': 0, 'category': 'zero'}

    sign = 1 if f_val < 0 else 0
    abs_f_val = abs(f_val)

    m, e = math.frexp(abs_f_val)

    unbiased_exp = e - 1

    if unbiased_exp < -IEEE_BIAS:
        category = 'subnormal'
        exp_field = 0

        scale = 1 << (IEEE_BIAS - 1 + IEEE_MANT_BITS)
        mant_field = int(abs_f_val * scale + 0.5)
        mant_field &= ((1 << IEEE_MANT_BITS) - 1)
    else:
        category = 'normal'
        exp_field = unbiased_exp + IEEE_BIAS

        mant_field = int((m * 2.0 - 1.0) * (1 << IEEE_MANT_BITS) + 0.5)
        mant_field &= ((1 << IEEE_MANT_BITS) - 1)

        if mant_field == 0 and m != 0.5:
            exp_field += 1

            if exp_field >= 0xFF:
                return {'sign': sign, 'exp_field': 0xFF, 'mant_field': 0, 'category': 'inf'}

    return {'sign': sign, 'exp_field': exp_field, 'mant_field': mant_field, 'category': category}


def get_recfloat_fields(val, rec_exp_bits=REC_EXP_BITS, rec_mant_bits=REC_MANT_BITS):
    """
    Calculates the expected RecFloating fields based on IEEE fields and HardFloat rules.
    Matches the logic in the provided SpinalHDL 'toRecFloating'.

    Returns: dict {'sign': int, 'exp': int,'mant': int}
    """
    ieee = get_ieee754_single_fields(val)
    ieee_sign = ieee['sign']
    ieee_exp = ieee['exp_field']
    ieee_mant = ieee['mant_field']

    rec_sign = ieee_sign
    rec_exp = 0
    rec_mant = 0

    is_exponent_zero = (ieee_exp == 0)
    is_mantissa_zero = (ieee_mant == 0)
    is_zero = is_exponent_zero and is_mantissa_zero
    is_inf = (ieee_exp == 0xFF) and is_mantissa_zero
    is_nan = (ieee_exp == 0xFF) and not is_mantissa_zero

    if is_zero:
        rec_sign = 0
        rec_exp = 0
        rec_mant = 0
    elif is_inf:
        rec_sign = ieee_sign
        rec_exp = 0x180
        rec_mant = 0
    elif is_nan:
        rec_sign = ieee_sign
        rec_exp_intermediate = 0x180
        rec_exp = 0x1C0
        rec_mant = ieee_mant
    elif is_exponent_zero:
        if ieee_mant == 0:
            raise ValueError("Subnormal case called with zero mantissa")

        lzc = IEEE_MANT_BITS - ieee_mant.bit_length()
        first_mantissa_bit = lzc + 1
        norm_mant_tmp = ieee_mant << first_mantissa_bit
        rec_mant = norm_mant_tmp & ((1 << rec_mant_bits) - 1)

        denorm_exp_mask = (1 << rec_exp_bits) - 1
        first_mantissa_bit_resized = first_mantissa_bit & denorm_exp_mask
        denorm_exp = denorm_exp_mask ^ first_mantissa_bit_resized

        rec_exp_intermediate = (
            denorm_exp + REC_EXP_ADD_SUBNORMAL) & denorm_exp_mask

        rec_exp = rec_exp_intermediate

    else:

        rec_exp_intermediate = (
            ieee_exp + REC_EXP_ADD_NORMAL) & ((1 << rec_exp_bits) - 1)
        rec_exp = rec_exp_intermediate
        rec_mant = ieee_mant

    return {'sign': rec_sign, 'exp': rec_exp, 'mant': rec_mant}
