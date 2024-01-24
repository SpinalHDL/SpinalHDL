package spinal.sim;

public interface IVerilatorNative {
    public long newHandle(String name, String wavePath, int seed);
    public boolean eval(long handle);
    public int get_time_precision(long handle);
    public void sleep(long handle, long cycles);
    public long getU64(long handle, int id);
    public void setU64(long handle, int id, long value);
    public void getAU8(long handle, int id, byte[] value);
    public void setAU8(long handle, int id, byte[] value, int length);
    public long getU64_mem(long handle, int id, long index);
    public void setU64_mem(long handle, int id, long value, long index);
    public void getAU8_mem(long handle, int id, byte[] value, long index);
    public void setAU8_mem(long handle, int id, byte[] value, int length, long index);
    public void deleteHandle(long handle);
    public void enableWave(long handle);
    public void disableWave(long handle);
}

