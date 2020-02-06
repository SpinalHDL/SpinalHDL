package spinal.sim;

public interface IVerilatorNative {
    public long newHandle(String name, int seed);
    public boolean eval(long handle);
    public void sleep(long handle, long cycles);
    public long getU64(long handle, int id);
    public void setU64(long handle, int id, long value);
    public void getAU8(long handle, int id, byte[] value);
    public void setAU8(long handle, int id, byte[] value, int length);
    public void deleteHandle(long handle);
    public void enableWave(long handle);
    public void disableWave(long handle);
}

