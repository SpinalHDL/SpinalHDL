package spinal.sim;

//javac spinal/sim/*.java; javah -jni spinal.sim.IVerilatorNative

import jnr.ffi.Pointer;
import jnr.ffi.annotations.IgnoreError;
import jnr.ffi.annotations.In;
import jnr.ffi.annotations.Out;

public interface IVerilatorNative {
    public long wrapperNewHandle(@In String name);
    @IgnoreError public void wrapperEval(long handle);
    @IgnoreError public void wrapperSleep(long handle, long cycles);
    @IgnoreError public long wrapperGetU64(long handle, int id);
    @IgnoreError public void wrapperSetU64(long handle, int id, long value);
    @IgnoreError public void wrapperGetAU8(long handle, int id,@Out byte[] value);
    @IgnoreError public void wrapperSetAU8(long handle, int id,@In byte[] value, int length);
    public void wrapperDeleteHandle(long handle);
//    public void wrapperTest(@Out int[] value);
//    public void wrapperTest2(@Out int value);
}
//  val tmp = Array[Int](4,5,6)
//  var miaou : Integer = 0
//  backend.native.wrapperTest(tmp)
//  println(tmp(0))
//  println(tmp(1))
//  backend.native.wrapperTest2(miaou)
//  println(miaou)
//  System.exit(0)