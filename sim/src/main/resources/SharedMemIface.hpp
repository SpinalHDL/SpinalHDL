#pragma once
#include"SharedStruct.hpp"
#include<string>
#include<vector>

using namespace std;

class SharedMemIface {

    public:
    SharedMemIface(const string& shmem_name_, size_t shmem_size_);
    string print_signals();
    int64_t get_signal_handle(const string& handle_name);
    std::vector<int8_t> read(int64_t handle);
    int64_t read64(int64_t handle);
    int32_t read32(int64_t handle);
    void write(int64_t handle, const std::vector<int8_t>& data);
    void write64(int64_t handle, int64_t data);
    void write32(int64_t handle, int32_t data);
    void sleep(int64_t sleep_cycles);
    void eval();
    void close();
    bool error_happened();
    string error_string();
    virtual ~SharedMemIface();

    private:
    bool wait();
    managed_shared_memory segment;
    SharedStruct* shared_struct; 
    SharedVector* data; 
    string shmem_name;
    size_t shmem_size;
};
