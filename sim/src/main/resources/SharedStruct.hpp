#pragma once
#include<boost/interprocess/managed_shared_memory.hpp>
#include<boost/interprocess/allocators/allocator.hpp>
#include<boost/interprocess/sync/scoped_lock.hpp>
#include<boost/interprocess/sync/interprocess_condition.hpp>
#include<boost/interprocess/containers/vector.hpp>
#include<cstdint>

using namespace boost::interprocess;

typedef allocator<uint8_t, managed_shared_memory::segment_manager>  ShmemAllocator;
typedef vector<uint8_t, ShmemAllocator> SharedVector;

enum class ProcStatus {
    init,
    ready,
    print_signals,
    get_signal_handle,
    read,
    write,
    sleep,
    close,
    error
};

class SharedStruct {
    public:
    SharedStruct() : 
        sleep_cycles(0), 
        proc_status(ProcStatus::init),  
        handle(0),
        closed(false) {}

    virtual ~SharedStruct(){}

    volatile uint64_t sleep_cycles;
    volatile ProcStatus proc_status;
    volatile size_t handle;
    volatile bool closed;

    class {
        public:
        void wait(){
            scoped_lock<interprocess_mutex> b_lock(b_mutex);
            
            if(blocked){
                blocked = false;
                b_block.notify_one();
            }else{
                blocked = true;
                b_block.wait(b_lock);
            }
        }

        volatile bool blocked = false;
        interprocess_mutex b_mutex;
        interprocess_condition b_block;
    } barrier;
};
