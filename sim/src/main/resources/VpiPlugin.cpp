#ifndef SHMEM_FILENAME
#define SHMEM_FILENAME "./shmem_name"
#endif

#include<boost/interprocess/sync/scoped_lock.hpp>
#include<cassert>
#include<iostream>
#include<iomanip>
#include<bitset>
#include<sstream>
#include<fstream>
#include<string>
#include<vector>
#include<algorithm>
#include<iterator>
#include<type_traits>
#include<random>
#include<vpi_user.h>
#include"SharedStruct.hpp"

using namespace std;

uniform_int_distribution<> bin_dis(0, 1);
managed_shared_memory segment;
SharedStruct* shared_struct;
stringstream ss;
string val_str;

PLI_INT32 start_cb(p_cb_data);
PLI_INT32 end_cb(p_cb_data);
PLI_INT32 rw_cb(p_cb_data);
PLI_INT32 ro_cb(p_cb_data);
PLI_INT32 delay_rw_cb(p_cb_data);
PLI_INT32 delay_ro_cb(p_cb_data);

void set_error(string& error_string){
    cout << error_string << endl;
    shared_struct->data.resize(error_string.size());
    std::copy(error_string.begin(),
            error_string.end(),
            shared_struct->data.begin());
    shared_struct->data.push_back('\0');
    shared_struct->proc_status.store(ProcStatus::error);
    vpi_control(vpiFinish, -1);
}

bool check_error(){
    s_vpi_error_info err;
    if (vpi_chk_error(&err)) {
        if(err.level == vpiError) {
            string error_string("VPI error : ");
            error_string += err.message;
            set_error(error_string);
            return true;
        } else {
            cout << "VPI message : " << err.message << endl;
        }
    }

    return false;
}

bool register_cb(PLI_INT32(*f)(p_cb_data),
        PLI_INT32 reason,
        int64_t cycles){

    s_cb_data cbData;
    s_vpi_time simuTime;
    if (cycles < 0){
        cbData.time = NULL;
    } else {
        cbData.time = &simuTime;
        simuTime.type = vpiSimTime;
        simuTime.high = (PLI_INT32) (cycles >> 32);
        simuTime.low = (PLI_INT32) (cycles & 0xFFFFFFFF);
    }

    cbData.reason = reason;
    cbData.cb_rtn = f;
    cbData.user_data = 0;
    cbData.value = 0;

    vpi_register_cb(&cbData);

    return check_error();
}

void entry_point_cb() {

    ifstream shmem_file(SHMEM_FILENAME);
    string shmem_name;
    getline(shmem_file, shmem_name);
    cout << "Shared memory key : " << shmem_name << endl;
    segment = managed_shared_memory(open_only, shmem_name.c_str());
    auto ret_struct = segment.find<SharedStruct>("SharedStruct");
    shared_struct = ret_struct.first; 
    register_cb(start_cb, cbStartOfSimulation, -1);
    if(check_error()) return;
    register_cb(end_cb, cbEndOfSimulation, -1);
    if(check_error()) return;
    register_cb(delay_ro_cb, cbAfterDelay, 0);
    if(check_error()) return;
}

bool print_net_in_module(vpiHandle module_handle, stringstream &msg_ss){
    char* module_name = vpi_get_str(vpiName, module_handle);
    cout << " Signals of " << module_name << endl;
    msg_ss << " Signals of " << module_name << endl;

    vpiHandle net_iterator = vpi_iterate(vpiNet,module_handle);
    if(check_error()) return true;
    if(net_iterator){
        while(vpiHandle net_handle = vpi_scan(net_iterator)){
            if(check_error()) return true;
            string net_full_name = vpi_get_str(vpiFullName, net_handle);
            if(check_error()) return true;

            cout << "    " << net_full_name.c_str() << endl;
            msg_ss << "    " << net_full_name.c_str() << endl;

            vpiHandle net = vpi_handle_by_name(const_cast<char*>(net_full_name.c_str()),
                    (vpiHandle) NULL);

            if(check_error()) return true;

            string net_full_name2 = vpi_get_str(vpiFullName,net);
            if(check_error()) return true;
            if (net_full_name.compare(net_full_name2)){
                assert(0);
            }

            vpi_free_object(net_handle);
        }
    } else {
        cout << "   No handles." << endl;
        msg_ss << "   No handles." << endl;
    }

    cout << endl;
    msg_ss << endl;
    return false;
}

bool print_signals_cmd(){

    vpiHandle mod_iterator;
    vpiHandle mod_handle, child_mod_handle;
    std::vector<vpiHandle> mod_handles;
    stringstream msg_ss;

    mod_iterator = vpi_iterate(vpiModule,NULL);
    if(check_error()) return true;
    if(!mod_iterator){ return false; }
    mod_handle = vpi_scan(mod_iterator);
    if(check_error()) return true;
    while(mod_handle) {
        mod_handles.push_back(mod_handle);
        mod_handle = vpi_scan(mod_iterator);
        if(check_error()) return true;
    }

    while(mod_handles.size() != 0) {
        mod_handle = mod_handles.back();
        mod_handles.pop_back();
        if(print_net_in_module(mod_handle, msg_ss)) return true;
        mod_iterator = vpi_iterate(vpiModule, mod_handle);
        if(check_error()) return true;
        if(mod_iterator){ 
            child_mod_handle = vpi_scan(mod_iterator);
            if(check_error()) return true;
            while(child_mod_handle) {
                mod_handles.push_back(child_mod_handle);
                child_mod_handle = vpi_scan(mod_iterator);
                if(check_error()) return true;
            }
        }
        vpi_free_object(mod_handle);
        if(check_error()) return true;
    }
    
    const string msg_str(msg_ss.str());
    shared_struct->data.resize(msg_str.size());
    std::copy(msg_str.begin(), msg_str.end(), shared_struct->data.begin());
    shared_struct->data.push_back('\0');
    return false;
}

bool randomize_in_module(vpiHandle module_handle, mt19937& mt_rand){

    vpiHandle net_iterator = vpi_iterate(vpiNet, module_handle);
    if(check_error()) return true;
    if(net_iterator){
        while(vpiHandle net_handle = vpi_scan(net_iterator)) {
            if(check_error()) return true;
            
            s_vpi_value value_struct;
            value_struct.format = vpiBinStrVal;
            vpi_get_value(net_handle, &value_struct);
            if(check_error()) return true;
            val_str = (const char*) value_struct.value.str;
            for(char& c : val_str) {
                if ((c != '0') && (c != '1')) { 
                    c = bin_dis(mt_rand) == 0 ? '0' : '1';
                }       
            }

            value_struct.value.str = (PLI_BYTE8*)val_str.c_str();
            vpi_put_value(net_handle, 
                          &value_struct, 
                          NULL, 
                          vpiNoDelay);
            if(check_error()) return true;
            vpi_free_object(net_handle);
        }
    } 

    return false;
}

bool randomize_cmd(){

    vpiHandle mod_iterator;
    vpiHandle mod_handle, child_mod_handle;
    std::vector<vpiHandle> mod_handles;
    mt19937 mt_rand(shared_struct->seed.load());

    mod_iterator = vpi_iterate(vpiModule,NULL);
    if(check_error()) return true;
    if(!mod_iterator){ return false; }
    mod_handle = vpi_scan(mod_iterator);
    if(check_error()) return true;
    while(mod_handle) {
        mod_handles.push_back(mod_handle);
        mod_handle = vpi_scan(mod_iterator);
        if(check_error()) return true;
    }

    while(mod_handles.size() != 0) {
        mod_handle = mod_handles.back();
        mod_handles.pop_back();
        if(randomize_in_module(mod_handle, mt_rand)) return true;
        mod_iterator = vpi_iterate(vpiModule, mod_handle);
        if(check_error()) return true;
        if(mod_iterator){ 
            child_mod_handle = vpi_scan(mod_iterator);
            if(check_error()) return true;
            while(child_mod_handle) {
                mod_handles.push_back(child_mod_handle);
                child_mod_handle = vpi_scan(mod_iterator);
                if(check_error()) return true;
            }
        }
        vpi_free_object(mod_handle);
        if(check_error()) return true;
    }

    return false;
}



bool get_signal_handle_cmd(){

    int64_t handle = (int64_t)vpi_handle_by_name((PLI_BYTE8*) shared_struct->data.data(), 
            NULL);
    if(check_error()) return true; 
    if(!handle) {
        string error_string("vpi_handle_by_name failed with argument: ");
        error_string += (const char*) shared_struct->data.data();
        set_error(error_string);
        return true;
    }

    shared_struct->handle.store(handle);
    return false;
}


void sanitize_byte_str(char* byte_str){
    for(size_t i = 0; i< 8; i++) {
        if ((byte_str[i] != '0') && (byte_str[i] != '1')) { 
            byte_str[i] = '0';
        }
    } 
}

bool read_cmd(){

    s_vpi_value value_struct;
    value_struct.format = vpiBinStrVal;
    shared_struct->data.clear();
    vpi_get_value((vpiHandle)shared_struct->handle.load(), &value_struct);
    if(check_error()) return true;
    size_t valueStrLen = strlen(value_struct.value.str);

    for(size_t i = 0; i < valueStrLen; i++){
        char c = value_struct.value.str[i];
        if ((c != '0') && (c != '1')) {
            cout << "Warning, value '" << c << "' at position "<< i << " is neither '0' or '1'. value set to '0'." << endl;
        }
    }

    size_t valueByteLen = valueStrLen/8;
    size_t bitShift = valueStrLen%8;

    if(bitShift != 0) {
        char accum_string[9] = "00000000";
        accum_string[8] = '\0';
        uint8_t accum = 0;
        memcpy(accum_string+(8-bitShift),
                value_struct.value.str,
                bitShift);

        sanitize_byte_str(accum_string);
        accum = stoul(string(accum_string),
                nullptr,
                2);
        shared_struct->data.push_back(accum);
    }

    for(size_t i = 0; i<valueByteLen; i++){
        char accum_string[9];
        accum_string[8] = '\0';
        uint8_t accum = 0;
        memcpy(accum_string,
                value_struct.value.str+bitShift+i*8,
                8);

        sanitize_byte_str(accum_string);
        accum = stoul(string(accum_string),
                nullptr,
                2);

        shared_struct->data.push_back(accum);
    }

    return false;
}

bool write_cmd(){

    s_vpi_value value_struct;
    ss.str(std::string());
    ss << setw(8);
    ss << setfill('0');

    for(uint8_t& el: shared_struct->data) ss << bitset<8>(el);

    value_struct.format = vpiBinStrVal;
    val_str = ss.str();
    value_struct.value.str = (PLI_BYTE8*)val_str.c_str();
    vpi_put_value((vpiHandle)shared_struct->handle.load(), 
            &value_struct, 
            NULL, 
            vpiNoDelay);

    return check_error();
}

bool sleep_cmd(){

    register_cb(delay_ro_cb, cbAfterDelay, shared_struct->sleep_cycles);
    check_error();
    return true;
}

bool close_cmd(){
    vpi_control(vpiFinish, 0);
    if(!check_error()) shared_struct->proc_status.store(ProcStatus::ready);
    return true;
}

PLI_INT32 start_cb(p_cb_data){
    cout << "Start of simulation" << endl;
    return 0;
}

PLI_INT32 end_cb(p_cb_data){
    cout << "End of simulation" << endl;
    return 0;
}

PLI_INT32 rw_cb(p_cb_data){

    bool run_simulation;
    do {
        run_simulation = false;
        shared_struct->proc_status.store(ProcStatus::ready);
        ProcStatus proc_status = shared_struct->check_not_ready();

        switch(proc_status){
            case ProcStatus::print_signals : run_simulation = print_signals_cmd(); break;
            case ProcStatus::get_signal_handle : run_simulation = get_signal_handle_cmd(); break;
            case ProcStatus::read : run_simulation = read_cmd(); break;
            case ProcStatus::write : run_simulation = write_cmd(); break;
            case ProcStatus::sleep : run_simulation = sleep_cmd(); break;
            case ProcStatus::randomize : run_simulation = randomize_cmd(); break;
            case ProcStatus::close : run_simulation = close_cmd(); break;
            default : {
                          run_simulation = true; 
                          string error_string("Invalid state ");
                          error_string += to_string(static_cast<int8_t>(proc_status));
                          error_string += " detected";
                          set_error(error_string);
                          break;  
                      }
        }
    } while(!run_simulation);

    return 0;
}

PLI_INT32 ro_cb(p_cb_data){
    register_cb(delay_rw_cb, cbAfterDelay, 0);
    check_error();
    return 0;
}

PLI_INT32 delay_rw_cb(p_cb_data){

    register_cb(rw_cb, cbReadWriteSynch, 0);
    check_error();
    return 0;
}

PLI_INT32 delay_ro_cb(p_cb_data){

    register_cb(ro_cb, cbReadOnlySynch, 0);
    check_error();
    return 0;
}

extern "C" {
    void (*vlog_startup_routines[]) () = {
        entry_point_cb,
        0
    };
}
