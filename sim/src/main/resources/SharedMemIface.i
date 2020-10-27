%module JNISharedMemIface

%include "stdint.i"
%include "std_vector.i"
%include "std_string.i"

%{
 #include "SharedMemIface.hpp"
%}

namespace std {
    %template(VectorInt8) vector<int8_t>;
};

%exception {
  try {
    $action
  } catch (VpiException &e) {
    jclass clazz = jenv->FindClass("spinal/sim/VpiException");
    jenv->ThrowNew(clazz, e.what());
    return $null;
  } catch (std::exception &e) {
    jclass clazz = jenv->FindClass("java/lang/Exception");
    jenv->ThrowNew(clazz, e.what());
    return $null;
  }

}

class SharedMemIface {
public:
    SharedMemIface(const std::string& shmem_name_, size_t shmem_size_);
    ~SharedMemIface();
    std::string print_signals();
    int64_t get_signal_handle(const std::string& handle_name);
   
    std::vector<int8_t> read(int64_t handle);
    void read(int64_t handle, std::vector<int8_t>& data);
    int64_t read64(int64_t handle);
    int32_t read32(int64_t handle);
    void write(int64_t handle, const std::vector<int8_t>& data);
    void write64(int64_t handle, int64_t data);
    void write32(int64_t handle, int32_t data);
    
    std::vector<int8_t> read_mem(int64_t handle, int64_t index);
    void read_mem(int64_t handle, std::vector<int8_t>& data, int64_t index);
    int64_t read64_mem(int64_t handle, int64_t index);
    int32_t read32_mem(int64_t handle, int64_t index);
    void write_mem(int64_t handle, const std::vector<int8_t>& data, int64_t index);
    void write64_mem(int64_t handle, int64_t data, int64_t index);
    void write32_mem(int64_t handle, int32_t data, int64_t index);

    void sleep(int64_t sleep_cycles);
    void eval();
    void set_seed(int64_t seed);
    void randomize(int64_t seed);
    void close();
    void check_ready();
    void set_crashed(int64_t ret_code_);
    bool is_closed();
};

%exception;
