#include"SharedMemIface.hpp"
#include<iostream>
#include<algorithm>
#include<string>
#include<vector>
#include<cassert>
#include<cstring>

SharedMemIface::SharedMemIface(const string& shmem_name_, size_t shmem_size_) :
    shmem_name(shmem_name_), shmem_size(shmem_size_) {
    shared_memory_object::remove(shmem_name.c_str());
    segment = managed_shared_memory(create_only, shmem_name.c_str(), shmem_size);
    shared_struct = segment.construct<SharedStruct>("SharedStruct")();
    const ShmemAllocator alloc_inst(segment.get_segment_manager());
    data = segment.construct<SharedVector>("SharedVector")(alloc_inst);
    shared_struct->proc_status = ProcStatus::init;
}

string SharedMemIface::print_signals(){
    while(shared_struct->proc_status == ProcStatus::init);
    assert(shared_struct->proc_status == ProcStatus::ready);
    assert(!shared_struct->closed);
    std::string ret;
    shared_struct->proc_status = ProcStatus::print_signals;
    wait();
    assert(shared_struct->proc_status == ProcStatus::ready);
    ret = (const char*)data->data();

    return ret;
}

int64_t SharedMemIface::get_signal_handle(const string& handle_name){
    while(shared_struct->proc_status == ProcStatus::init);
    assert(shared_struct->proc_status == ProcStatus::ready);
    assert(!shared_struct->closed);
    data->resize(handle_name.size()+1);
    memcpy(data->data(), handle_name.c_str(), handle_name.size());
    data->at(handle_name.size()) = '\0';
    shared_struct->proc_status = ProcStatus::get_signal_handle;
    wait();
    assert(shared_struct->proc_status == ProcStatus::ready);
    return shared_struct->handle;
}


std::vector<int8_t> SharedMemIface::read(int64_t handle){
    while(shared_struct->proc_status == ProcStatus::init);
    assert(shared_struct->proc_status == ProcStatus::ready);
    assert(!shared_struct->closed);
    std::vector<int8_t> ret_vec;
    shared_struct->handle = handle;
    shared_struct->proc_status = ProcStatus::read;
    wait();
    assert(shared_struct->proc_status == ProcStatus::ready);
    ret_vec.resize(data->size());
    memcpy(ret_vec.data(), data->data(), data->size());

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
    while(shared_struct->proc_status == ProcStatus::init);
    assert(shared_struct->proc_status == ProcStatus::ready);
    assert(!shared_struct->closed);
    shared_struct->handle = handle;
    data->resize(data_.size());
    memcpy(data->data(), data_.data(), data_.size());
    shared_struct->proc_status = ProcStatus::write;
    wait();
    assert(shared_struct->proc_status == ProcStatus::ready);
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
    while(shared_struct->proc_status == ProcStatus::init);
    assert(shared_struct->proc_status == ProcStatus::ready);
    assert(!shared_struct->closed);
    shared_struct->sleep_cycles = sleep_cycles;
    shared_struct->proc_status = ProcStatus::sleep;
    wait();
}

void SharedMemIface::eval(){
    this->sleep(0);
}

void SharedMemIface::close(){
    while(shared_struct->proc_status == ProcStatus::init);
    assert(shared_struct->proc_status == ProcStatus::ready);
    assert(!shared_struct->closed);
    shared_struct->proc_status = ProcStatus::close;
    wait();
    
    assert(shared_struct->closed);
}

bool SharedMemIface::error_happened(){
    return shared_struct->proc_status == ProcStatus::error;
}

std::string SharedMemIface::error_string(){
     
    return std::string((const char*)data->data());
}

SharedMemIface::~SharedMemIface(){
    if(!shared_struct->closed){
        close();
    }

    segment.destroy<SharedVector>("SharedVector");
    segment.destroy<SharedStruct>("SharedStruct");
    shared_memory_object::remove(shmem_name.c_str());
}

bool SharedMemIface::wait(){
    
    assert(shared_struct->proc_status != ProcStatus::ready);
    assert(shared_struct->proc_status != ProcStatus::error);
    shared_struct->barrier.wait();
    // Here the VPI side is doing its steps..    
    shared_struct->barrier.wait();
    assert((shared_struct->proc_status == ProcStatus::ready) | 
           (shared_struct->proc_status == ProcStatus::error));
    return error_happened();
}
