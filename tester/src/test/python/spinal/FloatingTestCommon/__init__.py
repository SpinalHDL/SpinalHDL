
import struct

def f32_to_bits(f_val):
    return struct.unpack('>I', struct.pack('>f', float(f_val)))[0]
