package spinal.sim;

public interface IVerilatorNative {
    public long wrapperNewHandle( String name, int seed);
    public void wrapperEval(long handle);
    public void wrapperSleep(long handle, long cycles);
    public long wrapperGetU64(long handle, int id);
    public void wrapperSetU64(long handle, int id, long value);
    public void wrapperGetAU8(long handle, int id, byte[] value);
    public void wrapperSetAU8(long handle, int id, byte[] value, int length);
    public void wrapperDeleteHandle(long handle);
}

