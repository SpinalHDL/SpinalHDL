%module JNISharedMemIface

%include "stdint.i"
%include "std_vector.i"
%include "std_string.i"

%{
 #include "SharedMemIface.hpp"
%}

namespace std {
    %template(VectorUint8) vector<uint8_t>;
};

class SharedMemIface {
public:
    SharedMemIface(const std::string& shmem_name_, size_t shmem_size_);
    ~SharedMemIface();
    std::string print_signals();
    uint64_t get_signal_handle(const std::string& handle_name);
    std::vector<uint8_t> read(uint64_t handle);
    uint64_t read_u64(uint64_t handle);
    uint32_t read_u32(uint64_t handle);
    void write(uint64_t handle, const std::vector<uint8_t>& data);
    void write_u64(uint64_t handle, uint64_t data);
    void write_u32(uint64_t handle, uint32_t data);
    void sleep(uint64_t sleep_cycles);
    void eval();
    bool error_happened();
    std::string error_string();
    void close();
};

