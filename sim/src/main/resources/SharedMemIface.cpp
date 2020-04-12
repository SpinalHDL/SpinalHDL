#include"SharedMemIface.hpp"
#include<iostream>
#include<algorithm>
#include<string>
#include<vector>
#include<cassert>
#include<cstring>

const char* closed_access_errstr = "Attempt to access an already closed simulation";

SharedMemIface::SharedMemIface(const string& shmem_name_, size_t shmem_size_) :
    closed(false), shmem_name(shmem_name_), shmem_size(shmem_size_) {
    shared_memory_object::remove(shmem_name.c_str());
    segment = managed_shared_memory(create_only, shmem_name.c_str(), shmem_size);
    const ShmemAllocator alloc_inst(segment.get_segment_manager());
    shared_struct = segment.construct<SharedStruct>("SharedStruct")(alloc_inst);
    shared_struct->proc_status.store(ProcStatus::init);
}

string SharedMemIface::print_signals(){
    if(this->closed) throw VpiException(closed_access_errstr);
    shared_struct->check_ready();
    std::string ret;
    shared_struct->proc_status.store(ProcStatus::print_signals);
    shared_struct->check_ready();
    ret = (const char*)shared_struct->data.data();

    return ret;
}

int64_t SharedMemIface::get_signal_handle(const string& handle_name){
    if(this->closed) throw VpiException(closed_access_errstr);
    shared_struct->check_ready();
    shared_struct->data.resize(handle_name.size());
    std::copy(handle_name.begin(), handle_name.end(), shared_struct->data.begin());
    shared_struct->data.push_back(0);
    shared_struct->proc_status.store(ProcStatus::get_signal_handle);
    shared_struct->check_ready();
    return shared_struct->handle;
}


std::vector<int8_t> SharedMemIface::read(int64_t handle){
    if(this->closed) throw VpiException(closed_access_errstr);
    shared_struct->check_ready();
    std::vector<int8_t> ret_vec;
    shared_struct->handle.store(handle);
    shared_struct->proc_status.store(ProcStatus::read);
    shared_struct->check_ready();
    ret_vec.resize(shared_struct->data.size());
    std::copy(shared_struct->data.begin(), shared_struct->data.end(), ret_vec.begin());

    return ret_vec;
}

int64_t SharedMemIface::read64(int64_t handle){
    int64_t ret = 0ul;
    std::vector<int8_t> read_data = this->read(handle);
    size_t copy_size = std::min(8ul, read_data.size());
    size_t start_orig = read_data.size()-1;
    for(uint8_t i = 0; i < copy_size; i++) {
        ((int8_t*) &ret)[i] = read_data[start_orig - i];
    }
    return ret;
}

int32_t SharedMemIface::read32(int64_t handle){
    int32_t ret = 0ul;
    std::vector<int8_t> read_data = this->read(handle);
    size_t copy_size = std::min(4ul, read_data.size());
    size_t start_orig = read_data.size()-1;
    for(uint8_t i = 0; i < copy_size; i++) {
        ((int8_t*) &ret)[i] = read_data[start_orig - i];
    }
    return ret;
}

void SharedMemIface::write(int64_t handle, const std::vector<int8_t>& data_){
    if(this->closed) throw VpiException(closed_access_errstr);
    shared_struct->check_ready();
    shared_struct->handle.store(handle);
    shared_struct->data.resize(data_.size());
    std::copy(data_.begin(), data_.end(), shared_struct->data.begin());
    shared_struct->proc_status.store(ProcStatus::write);
}

void SharedMemIface::write64(int64_t handle, int64_t data_){
    
    std::vector<int8_t> vdata;
    vdata.resize(8);
    for(uint8_t i = 0; i < 8; i++) vdata[7-i] = (data_ >> 8*i) & 0xFF;
    this->write(handle, vdata);
}

void SharedMemIface::write32(int64_t handle, int32_t data_){
    std::vector<int8_t> vdata;
    vdata.resize(4);
    for(uint8_t i = 0; i < 4; i++) vdata[3-i] = (data_ >> 8*i) & 0xFF;
    this->write(handle, vdata);
}

void SharedMemIface::sleep(int64_t sleep_cycles){
    if(this->closed) throw VpiException(closed_access_errstr);
    shared_struct->check_ready();
    shared_struct->sleep_cycles.store(sleep_cycles);
    shared_struct->proc_status.store(ProcStatus::sleep);
}

void SharedMemIface::eval(){
    this->sleep(0);
}

void SharedMemIface::close(){
    if(this->closed) throw VpiException(closed_access_errstr);
    shared_struct->check_ready();
    shared_struct->proc_status.store(ProcStatus::close);
    this->closed = true;
    segment.destroy<SharedStruct>("SharedStruct");
    shared_memory_object::remove(shmem_name.c_str());
}

SharedMemIface::~SharedMemIface(){}
