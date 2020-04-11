#ifndef SHMEM_FILENAME
#define SHMEM_FILENAME "./shmem_name"
#endif

#include <boost/interprocess/sync/scoped_lock.hpp>
#include<cassert>
#include<iostream>
#include<iomanip>
#include<bitset>
#include<sstream>
#include<fstream>
#include<string>
#include<vpi_user.h>
#include"SharedStruct.hpp"

using namespace std;

managed_shared_memory segment;
SharedStruct* shared_struct;
SharedVector* data; 

PLI_INT32 start_cb(p_cb_data);
PLI_INT32 end_cb(p_cb_data);
PLI_INT32 rw_cb(p_cb_data);
PLI_INT32 ro_cb(p_cb_data);
PLI_INT32 delay_rw_cb(p_cb_data);
PLI_INT32 delay_ro_cb(p_cb_data);

void set_error(string& error_string){
    cout << error_string << endl;
    data->clear();
    data->insert(data->end(), 
                error_string.begin(),
                error_string.end());
    data->push_back('\0');
    shared_struct->proc_status = ProcStatus::error;
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
    segment = managed_shared_memory(open_only, shmem_name.c_str());
    auto ret_struct = segment.find<SharedStruct>("SharedStruct");
    shared_struct = ret_struct.first; 
    auto ret_data = segment.find<SharedVector>("SharedVector");
    data = ret_data.first; 
    register_cb(start_cb, cbStartOfSimulation, -1);
    register_cb(end_cb, cbEndOfSimulation, -1);
    register_cb(delay_rw_cb, cbAfterDelay, 0);
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
        
    vpiHandle top_mod_iterator;
    vpiHandle top_mod_handle;
    stringstream msg_ss;

    top_mod_iterator = vpi_iterate(vpiModule,NULL);
    if(check_error()) return true;
    if( !top_mod_iterator ){
        return 0;
    }

    top_mod_handle = vpi_scan(top_mod_iterator);
    if(check_error()) return true;
    while(top_mod_handle) {
        if(print_net_in_module(top_mod_handle, msg_ss)) return true;
        vpiHandle module_iterator = vpi_iterate(vpiModule,top_mod_handle);
        if(check_error()) return true;
        if (module_iterator){
            vpiHandle module_handle;
            module_handle = vpi_scan(module_iterator);
            if(check_error()) return true;
            while (module_handle) {
                if(print_net_in_module(module_handle, msg_ss)) return true;
                module_handle = vpi_scan(module_iterator);
                if(check_error()) return true;
            }
        }

        top_mod_handle = vpi_scan(top_mod_iterator);
    }

    data->clear();
    const string msg_str(msg_ss.str());
    for(uint8_t el: msg_str) data->push_back(el);
    data->push_back('\0');
    return false;
}

bool get_signal_handle_cmd(){
    
    shared_struct->handle = (size_t)vpi_handle_by_name((PLI_BYTE8*) data->data(), 
                                                       NULL);
    if(check_error()) return true; 
    if(!shared_struct->handle) {
        string error_string("vpi_handle_by_name failed with argument ");
        error_string += (char*) data->data();
        set_error(error_string);
        return true;
    }

    return false;
}

bool read_cmd(){
    
    s_vpi_value value_struct;
    value_struct.format = vpiBinStrVal;
    data->clear();
    vpi_get_value((vpiHandle)shared_struct->handle, &value_struct);
    if(check_error()) return true;
    size_t valueStrLen = strlen(value_struct.value.str);
    size_t valueByteLen = valueStrLen/8;
    size_t bitShift = valueStrLen%8;

    if(bitShift != 0) {
        char accum_string[9] = "00000000";
        accum_string[8] = '\0';
        uint8_t accum = 0;
        memcpy(accum_string+(8-bitShift),
                value_struct.value.str,
                bitShift);

        accum = stoul(string(accum_string),
                nullptr,
                2);
        data->push_back(accum);
    }

    for(size_t i = 0; i<valueByteLen; i++){
        char accum_string[9];
        accum_string[8] = '\n';
        uint8_t accum = 0;
        strncpy(accum_string,
                value_struct.value.str+bitShift+i*8,
                8);

        accum = stoul(string(accum_string),
                nullptr,
                2);

        data->push_back(accum);
    }

    return false;
}

bool write_cmd(){

    s_vpi_value value_struct;
    stringstream ss;
    ss << setw(8);
    ss << setfill('0');

    for(uint8_t& el: *data) ss << bitset<8>(el);

    value_struct.format = vpiBinStrVal;
    string val_str = ss.str();
    value_struct.value.str = (PLI_BYTE8*)val_str.c_str();
    vpi_put_value((vpiHandle)shared_struct->handle, &value_struct, NULL, vpiNoDelay);

    return check_error();
}

bool sleep_cmd(){

    register_cb(delay_ro_cb, cbAfterDelay, shared_struct->sleep_cycles-1);
    return true;
}

bool close_cmd(){
    vpi_control(vpiFinish, 0);
    check_error();
    return true;
}

PLI_INT32 start_cb(p_cb_data){
    cout << "Start of simulation" << endl;
    return 0;
}

PLI_INT32 end_cb(p_cb_data){
    cout << "End of simulation" << endl;
    shared_struct->closed = true;
    shared_struct->proc_status = ProcStatus::ready;
    shared_struct->barrier.wait();
    return 0;
}

PLI_INT32 rw_cb(p_cb_data){

    bool run_simulation;
    do {
        run_simulation = false;
        if(shared_struct->proc_status == ProcStatus::init){
            shared_struct->proc_status = ProcStatus::ready;
        } else {
            shared_struct->proc_status = ProcStatus::ready;
            shared_struct->barrier.wait();
        }
        
        shared_struct->barrier.wait();
        
        switch(shared_struct->proc_status){
            case ProcStatus::ready : run_simulation = false; break;
            case ProcStatus::print_signals : run_simulation = print_signals_cmd(); break;
            case ProcStatus::get_signal_handle : run_simulation = get_signal_handle_cmd(); break;
            case ProcStatus::read : run_simulation = read_cmd(); break;
            case ProcStatus::write : run_simulation = write_cmd(); break;
            case ProcStatus::sleep : run_simulation = sleep_cmd(); break;
            case ProcStatus::close : run_simulation = close_cmd(); break;
            default : {
                        run_simulation = true; 
                        string error_string("Invalid state error");
                        set_error(error_string);
                        break;  
                      }
        }
    } while(!run_simulation);

    return 0;
}

PLI_INT32 ro_cb(p_cb_data){
    register_cb(delay_rw_cb, cbAfterDelay, 1);
    return 0;
}

PLI_INT32 delay_rw_cb(p_cb_data){

    register_cb(rw_cb, cbReadWriteSynch, 0);
    return 0;
}

PLI_INT32 delay_ro_cb(p_cb_data){

    register_cb(ro_cb, cbReadOnlySynch, 0);
    return 0;
}

extern "C" {
    void (*vlog_startup_routines[]) () = {
        entry_point_cb,
        0
    };
}

