#pragma once
#include"SharedStruct.hpp"
#include<string>
#include<vector>

using namespace std;

class SharedMemIface {

    public:
    SharedMemIface(const string& shmem_name_, size_t shmem_size_);
    string print_signals();
    uint64_t get_signal_handle(const string& handle_name);
    std::vector<uint8_t> read(uint64_t handle);
    uint64_t read_u64(uint64_t handle);
    uint32_t read_u32(uint64_t handle);
    void write(uint64_t handle, const std::vector<uint8_t>& data);
    void write_u64(uint64_t handle, uint64_t data);
    void write_u32(uint64_t handle, uint32_t data);
    void sleep(uint64_t sleep_cycles);
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
