#include"SharedMemIface.hpp"
#include<iostream>
#include<algorithm>
#include<string>
#include<vector>
#include<cassert>
#include<cstring>


SharedMemIface::SharedMemIface(const string& shmem_name_, size_t shmem_size_) :
    closed(false), shmem_name(shmem_name_), shmem_size(shmem_size_), ret_code(0),
    data_buffer(), error_string() {
        shared_memory_object::remove(shmem_name.c_str());
        segment = managed_shared_memory(create_only, shmem_name.c_str(), shmem_size);
        const ShmemAllocator alloc_inst(segment.get_segment_manager());
        shared_struct = segment.construct<SharedStruct>("SharedStruct")(alloc_inst);
        shared_struct->proc_status.store(ProcStatus::init);
    }

string SharedMemIface::print_signals(){
    this->check_ready();
    std::string ret;
    shared_struct->proc_status.store(ProcStatus::print_signals);
    this->check_ready();
    ret = (const char*)shared_struct->data.data();

    return ret;
}

int64_t SharedMemIface::get_signal_handle(const string& handle_name){
    this->check_ready();
    shared_struct->data.resize(handle_name.size());
    std::copy(handle_name.begin(), handle_name.end(), shared_struct->data.begin());
    shared_struct->data.push_back(0);
    shared_struct->proc_status.store(ProcStatus::get_signal_handle);
    this->check_ready();
    return shared_struct->handle;
}

void SharedMemIface::read(int64_t handle, std::vector<int8_t>& data_){
    this->check_ready();
    shared_struct->handle.store(handle);
    shared_struct->proc_status.store(ProcStatus::read);
    this->check_ready();
    data_.resize(shared_struct->data.size());
    std::copy(shared_struct->data.begin(), shared_struct->data.end(), data_.begin());
}

std::vector<int8_t> SharedMemIface::read(int64_t handle){
    this->read(handle, data_buffer);
    return this->data_buffer;
}

int64_t SharedMemIface::read64(int64_t handle){
    int64_t ret = 0;
    this->read(handle, this->data_buffer);
    size_t copy_size = std::min((size_t)8, this->data_buffer.size());
    size_t start_orig = this->data_buffer.size()-1;
    for(uint8_t i = 0; i < copy_size; i++) {
        ((int8_t*) &ret)[i] = this->data_buffer[start_orig - i];
    }
    return ret;
}

int32_t SharedMemIface::read32(int64_t handle){
    int32_t ret = 0;
    this->read(handle, this->data_buffer);
    size_t copy_size = std::min((size_t)4, this->data_buffer.size());
    size_t start_orig = this->data_buffer.size()-1;
    for(uint8_t i = 0; i < copy_size; i++) {
        ((int8_t*) &ret)[i] = this->data_buffer[start_orig - i];
    }
    return ret;
}

void SharedMemIface::write(int64_t handle, const std::vector<int8_t>& data_){
    this->check_ready();
    shared_struct->handle.store(handle);
    shared_struct->data.resize(data_.size());
    std::copy(data_.begin(), data_.end(), shared_struct->data.begin());
    shared_struct->proc_status.store(ProcStatus::write);
}

void SharedMemIface::write64(int64_t handle, int64_t data_){

    this->data_buffer.resize(8);
    for(uint8_t i = 0; i < 8; i++) this->data_buffer[7-i] = (data_ >> 8*i) & 0xFF;
    this->write(handle, this->data_buffer);
}

void SharedMemIface::write32(int64_t handle, int32_t data_){
    this->data_buffer.resize(4);
    for(uint8_t i = 0; i < 4; i++) this->data_buffer[3-i] = (data_ >> 8*i) & 0xFF;
    this->write(handle, this->data_buffer);
}

void SharedMemIface::sleep(int64_t sleep_cycles){
    this->check_ready();
    shared_struct->sleep_cycles.store(sleep_cycles);
    shared_struct->proc_status.store(ProcStatus::sleep);
}

void SharedMemIface::eval(){
    this->sleep(0);
}

void SharedMemIface::set_seed(int64_t seed){
    this->check_ready();
    shared_struct->seed.store(seed);
    shared_struct->proc_status.store(ProcStatus::set_seed);
    this->check_ready();
}

void SharedMemIface::randomize(int64_t seed){
    this->check_ready();
    shared_struct->seed.store(seed);
    shared_struct->proc_status.store(ProcStatus::randomize);
    this->check_ready();
}

void SharedMemIface::close(){
    this->check_ready();
    shared_struct->proc_status.store(ProcStatus::close);
    this->closed = true;
    segment.destroy<SharedStruct>("SharedStruct");
    shared_memory_object::remove(shmem_name.c_str());
}

SharedMemIface::~SharedMemIface(){}

void SharedMemIface::check_ready(){
    if(this->closed) throw VpiException("Attempt to access an already closed simulation");
    ProcStatus status = shared_struct->proc_status.load();
    #ifndef NO_SPINLOCK_YIELD_OPTIMIZATION
    for(uint32_t spin_count = 0; status != ProcStatus::ready; ++spin_count) {
    #else
    while(status != ProcStatus::ready) {
    #endif
        if (status == ProcStatus::error) {
            this->closed = true;
            error_string = (const char*) shared_struct->data.data();
            segment.destroy<SharedStruct>("SharedStruct");
            shared_memory_object::remove(shmem_name.c_str());
            throw VpiException(error_string.c_str()); 
        }

        if (status == ProcStatus::closed) {
            this->closed = true;
            segment.destroy<SharedStruct>("SharedStruct");
            shared_memory_object::remove(shmem_name.c_str());
            throw VpiException("Unexpected closed simulation");
        }

        int64_t retcode = this->ret_code.load(std::memory_order_relaxed);
        if(retcode) {
            this->closed = true;
            error_string = "Simulation crashed with return status ";
            error_string += to_string(retcode);
            segment.destroy<SharedStruct>("SharedStruct");
            shared_memory_object::remove(shmem_name.c_str());
            throw VpiException(error_string.c_str());
        }

        #ifndef NO_SPINLOCK_YIELD_OPTIMIZATION
        if (spin_count < SPINLOCK_MAX_ACQUIRE_SPINS) {
            _mm_pause();
        } else {
            std::this_thread::yield();
            spin_count = 0;
        }
        #endif

        status = shared_struct->proc_status.load();
    }
}
