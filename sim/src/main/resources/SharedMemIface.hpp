#pragma once
#include"SharedStruct.hpp"
#include<string>
#include<vector>
#include<atomic>

using namespace std;

class SharedMemIface {

    public:
    SharedMemIface(const string& shmem_name, size_t shmem_size);
    string print_signals();
    int64_t get_signal_handle(const string& handle_name);
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
    bool is_closed(){ return this->closed; };
    void check_ready();
    void set_crashed(int64_t ret_code_){ 
        this->ret_code.store(ret_code_, std::memory_order_relaxed); 
    };
    
    virtual ~SharedMemIface();

    private:

    void read_raw(int64_t handle, std::vector<int8_t>& data, bool is_mem);
    int64_t read64_raw(int64_t handle, bool is_mem);
    int32_t read32_raw(int64_t handle, bool is_mem);
    void write_raw(int64_t handle, const std::vector<int8_t>& data, bool is_mem);
    void write64_raw(int64_t handle, int64_t data, bool is_mem);
    void write32_raw(int64_t handle, int32_t data, bool is_mem);

    bool closed;
    size_t index;
    managed_shared_memory segment;
    SharedStruct* shared_struct; 
    string shmem_name;
    size_t shmem_size;
    std::atomic<int64_t> ret_code;
    std::vector<int8_t> data_buffer;
    std::string error_string;
};
