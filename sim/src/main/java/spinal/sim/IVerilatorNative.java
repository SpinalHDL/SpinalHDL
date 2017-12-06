package spinal.sim;

//javac spinal/sim/*.java; javah -jni spinal.sim.IVerilatorNative

import jnr.ffi.annotations.IgnoreError;

public interface IVerilatorNative {
    public long wrapperNewHandle();
    public void wrapperEval(long handle);
    @IgnoreError public byte wrapperGetCData(long handle, int id);
    @IgnoreError public void wrapperSetCData(long handle, int id, byte value);
}
