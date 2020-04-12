#pragma once
#include<boost/interprocess/managed_shared_memory.hpp>
#include<boost/interprocess/allocators/allocator.hpp>
#include<boost/interprocess/sync/scoped_lock.hpp>
#include<boost/interprocess/sync/interprocess_condition.hpp>
#include<boost/interprocess/containers/vector.hpp>
#include<exception>
#include<atomic>
#include<iostream>
#include<cstdint>

using namespace boost::interprocess;

typedef allocator<uint8_t, managed_shared_memory::segment_manager>  ShmemAllocator;
typedef vector<uint8_t, ShmemAllocator> SharedVector;

class VpiException: public std::exception
{
    public:
    VpiException(const char* msg_): exception(), msg(msg_) {};
    virtual const char* what() const throw(){
        return msg;
    };

    private:
    const char * msg;
};

enum class ProcStatus : int8_t {
    ready = 0,
    error = -1,
    init = 1,
    print_signals = 2,
    get_signal_handle = 3,
    read = 4,
    write = 5,
    sleep = 6,
    close = 7,
};

class SharedStruct {
    public:
    SharedStruct(const ShmemAllocator alloc_inst_) : 
        alloc_inst(alloc_inst_),
        proc_status(ProcStatus::init),  
        sleep_cycles(0), 
        handle(0),
        data(alloc_inst){}

    virtual ~SharedStruct(){}

    void check_ready(){
        ProcStatus status = this->proc_status.load();
        while(status != ProcStatus::ready) {
            if (status == ProcStatus::error) {
               throw VpiException((const char*) this->data.data()); 
            }
            status = this->proc_status.load();
        }
    }
    
    ProcStatus check_not_ready() {
    ProcStatus status = this->proc_status.load();
    while(status == ProcStatus::ready) {
            status = this->proc_status.load();
        }
        
    return status;
    }

    const ShmemAllocator alloc_inst;
    std::atomic<ProcStatus> proc_status;
    std::atomic<uint64_t> sleep_cycles;
    std::atomic<size_t> handle;
    SharedVector data; 
};
