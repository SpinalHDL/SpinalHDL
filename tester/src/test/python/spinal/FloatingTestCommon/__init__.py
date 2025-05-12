
import math
import struct
from decimal import Decimal, getcontext

# IEEE 754 SP Consts
IEEE_EXP_BITS = 8
IEEE_MANT_BITS = 23
IEEE_BIAS = (1 << (IEEE_EXP_BITS - 1)) - 1


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
