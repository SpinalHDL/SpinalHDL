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

void SharedMemIface::read_raw(int64_t handle, std::vector<int8_t>& data_, bool is_mem){
    this->check_ready();
    shared_struct->handle.store(handle);
    
    if(is_mem) {
        shared_struct->index.store(this->index);
        shared_struct->proc_status.store(ProcStatus::read_mem);
    } else {
        shared_struct->proc_status.store(ProcStatus::read);
    }
    
    this->check_ready();
    data_.resize(shared_struct->data.size());
    std::copy(shared_struct->data.begin(), shared_struct->data.end(), data_.begin());
}

void SharedMemIface::read(int64_t handle, std::vector<int8_t>& data_){
    read_raw(handle, data_, false);
}

void SharedMemIface::read_mem(int64_t handle, std::vector<int8_t>& data_, int64_t index_){
    this->index = index_;
    read_raw(handle, data_, true);
}

std::vector<int8_t> SharedMemIface::read(int64_t handle){
    this->read(handle, data_buffer);
    return this->data_buffer;
}

std::vector<int8_t> SharedMemIface::read_mem(int64_t handle, int64_t index_){
    this->read_mem(handle, data_buffer, index_);
    return this->data_buffer;
}

int64_t SharedMemIface::read64_raw(int64_t handle, bool is_mem){
    int64_t ret = 0;
    this->read_raw(handle, this->data_buffer, is_mem);
    size_t copy_size = std::min((size_t)8, this->data_buffer.size());
    size_t start_orig = this->data_buffer.size()-1;
    for(uint8_t i = 0; i < copy_size; i++) {
        ((int8_t*) &ret)[i] = this->data_buffer[start_orig - i];
    }
    return ret;
}

int64_t SharedMemIface::read64(int64_t handle){
    return read64_raw(handle, false);
}

int64_t SharedMemIface::read64_mem(int64_t handle, int64_t index_){
    this->index = index_;
    return read64_raw(handle, true);
}

int32_t SharedMemIface::read32_raw(int64_t handle, bool is_mem){
    int32_t ret = 0;
    this->read_raw(handle, this->data_buffer, is_mem);
    size_t copy_size = std::min((size_t)4, this->data_buffer.size());
    size_t start_orig = this->data_buffer.size()-1;
    for(uint8_t i = 0; i < copy_size; i++) {
        ((int8_t*) &ret)[i] = this->data_buffer[start_orig - i];
    }
    return ret;
}

int32_t SharedMemIface::read32(int64_t handle){
    return read32_raw(handle, false);
}

int32_t SharedMemIface::read32_mem(int64_t handle, int64_t index_){
    this->index = index_;
    return read32_raw(handle, true);
}

void SharedMemIface::write_raw(int64_t handle, const std::vector<int8_t>& data_, bool is_mem){
    this->check_ready();
    shared_struct->handle.store(handle);
    shared_struct->data.resize(data_.size());
    std::copy(data_.begin(), data_.end(), shared_struct->data.begin());
    if(is_mem) {
        shared_struct->index.store(this->index);
        shared_struct->proc_status.store(ProcStatus::write_mem);
    } else {
        shared_struct->proc_status.store(ProcStatus::write);
    }
}

void SharedMemIface::write(int64_t handle, const std::vector<int8_t>& data_){
    write_raw(handle, data_, false);
}

void SharedMemIface::write_mem(int64_t handle, const std::vector<int8_t>& data_, int64_t index_){
    this->index = index_;
    write_raw(handle, data_, true);
}

void SharedMemIface::write64_raw(int64_t handle, int64_t data_, bool is_mem){

    this->data_buffer.resize(8);
    for(uint8_t i = 0; i < 8; i++) this->data_buffer[7-i] = (data_ >> 8*i) & 0xFF;
    write_raw(handle, this->data_buffer, is_mem);
}

void SharedMemIface::write64(int64_t handle, int64_t data_){
    write64_raw(handle, data_, false);
}

void SharedMemIface::write64_mem(int64_t handle, int64_t data_, int64_t index_){
    this->index = index_;
    write64_raw(handle, data_, true);
}

void SharedMemIface::write32_raw(int64_t handle, int32_t data_, bool is_mem){
    this->data_buffer.resize(4);
    for(uint8_t i = 0; i < 4; i++) this->data_buffer[3-i] = (data_ >> 8*i) & 0xFF;
    write_raw(handle, this->data_buffer, is_mem);
}

void SharedMemIface::write32(int64_t handle, int32_t data_){
    write32_raw(handle, data_, false);
}

void SharedMemIface::write32_mem(int64_t handle, int32_t data_, int64_t index_){
    this->index = index_;
    write32_raw(handle, data_, true);
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
            _spin_pause();
	} else {
            std::this_thread::yield();
            spin_count = 0;
        }
        #endif

        status = shared_struct->proc_status.load();
    }
}
