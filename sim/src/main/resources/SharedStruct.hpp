#pragma once
#include<boost/interprocess/managed_shared_memory.hpp>
#include<boost/interprocess/allocators/allocator.hpp>
#include<boost/interprocess/sync/scoped_lock.hpp>
#include<boost/interprocess/sync/interprocess_condition.hpp>
#include<boost/interprocess/containers/vector.hpp>
#if !defined(__ARM_ARCH)
#include<immintrin.h> //_mm_pause 
#endif
#include<exception>
#include<atomic>
#include<thread>
#include<iostream>
#include<string>
#include<cstdint>

#ifndef SPINLOCK_MAX_ACQUIRE_SPINS
#define SPINLOCK_MAX_ACQUIRE_SPINS 500
#endif

using namespace boost::interprocess;

typedef allocator<uint8_t, managed_shared_memory::segment_manager>  ShmemAllocator;
typedef vector<uint8_t, ShmemAllocator> SharedVector;

inline void _spin_pause() {
    #if defined(__ARM_ARCH)
        #if __ARM_ARCH == 8
    __asm__ __volatile__("yield" ::: "memory");
        #else
    __asm__ __volatile__("yield");
        #endif
    #else
    _mm_pause();
    #endif
}

class VpiException: public std::exception
{
    public:
        VpiException(const char* msg_): exception(), msg(msg_) {};
        virtual const char* what() const throw(){
            return msg.c_str();
        };

    private:
        std::string msg;
};

enum class ProcStatus : int8_t {
    ready = 0,
    error = -1,
    init = 1,
    print_signals = 2,
    get_signal_handle = 3,
    read = 4,
    write = 5,
    read_mem = 6,
    write_mem =7,
    sleep = 8,
    close = 9,
    set_seed = 10,
    randomize = 11,
    closed = 12,
};

class SharedStruct {
    public:
        SharedStruct(const ShmemAllocator alloc_inst_) : 
            alloc_inst(alloc_inst_),
            proc_status(ProcStatus::init),  
            sleep_cycles(0), 
            seed(12345),
            handle(0),
            data(alloc_inst){}

        virtual ~SharedStruct(){}

        ProcStatus check_not_ready() {
            ProcStatus status = this->proc_status.load();
            #ifndef NO_SPINLOCK_YIELD_OPTIMIZATION
            for(uint32_t spin_count = 0; status == ProcStatus::ready; ++spin_count) {

                if (spin_count < SPINLOCK_MAX_ACQUIRE_SPINS) {
                    _spin_pause();
                } else {
                    std::this_thread::yield();
                    spin_count = 0;
                }
                status = this->proc_status.load();
            }
            #else
            while(status == ProcStatus::ready) status = this->proc_status.load();
            #endif
            return status;
        }

        const ShmemAllocator alloc_inst;
        std::atomic<ProcStatus> proc_status;
        std::atomic<uint64_t> sleep_cycles;
        std::atomic<uint64_t> seed;
        std::atomic<size_t> index;
        std::atomic<size_t> handle;
        SharedVector data; 
};
