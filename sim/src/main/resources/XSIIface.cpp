#include <cstdlib>
#include <string>
#include <cstring>
#include <iostream>
#include <algorithm>

#include "spinal_xsim.h"
#include "XSIIface.hpp"
#include "xsi_loader.h"

int64_t pow10(int64_t a) {
    if (a < 0)
        return pow10(-a);
    int64_t ret_val = 1;
    for(int64_t i = a; i > 0; i--) {
        ret_val *= 10;
    }
    return ret_val;
}

int32_t reverse32(int32_t a) {
    return (a & 0xFF000000) >> 24 |
           (a & 0x00FF0000) >>  8 |
           (a & 0x0000FF00) <<  8 |
           (a & 0x000000FF) << 24;
}

void check_vlog_logicval(s_xsi_vlog_logicval *value) {
    if (value->bVal != 0) {
        std::cout << "Unexpected \'X\' of \'Z\', reset to 0" << std::endl;
    }
    uint32_t mask = ~(value->bVal);
    value->aVal &= mask;
    value->bVal = 0;
}

int32_t as_logic_val_width(int32_t width) {
    int64_t result = width >> 5;
    if (width % 32) {
        result += 1;
    }
    return result;
}

XSIIface::XSIIface(): loader{SIM_DESIGN, SIM_KERNEL} {
    std::string sim_design = SIM_DESIGN;
    std::string sim_kernel = SIM_KERNEL;

    std::cout << "Design lib: " << sim_design << std::endl;
    std::cout << "Kernel lib: " << sim_kernel << std::endl;

    s_xsi_setup_info info;
    memset(&info, 0, sizeof(info));
    info.logFileName = NULL;
    char wdbName[] = SIM_WAVE;
    info.wdbFileName = wdbName;
    loader.open(&info);
    if (SIM_HAS_WAVE) {
        loader.trace_all();
    }

    sim_time_precision = loader.get_sim_time_precision();
}

XSIIface::~XSIIface() {}

int32_t XSIIface::get_signal_handle(const std::string& handle_name) {
    int32_t port = (int32_t)loader.get_port_number(handle_name.c_str());
    if (port < 0) {
        throw XSIException("no such port");
    }
    return port;
}

/*
 * Returns the size of a signal in bits
 */
int32_t XSIIface::get_port_width(int32_t handle) {
    return loader.get_int_port(handle, xsiHDLValueSize);
}

/*
 * Verilog signal functions
 */
std::vector<int8_t> XSIIface::read_vlog(int32_t handle) {
    int32_t port_width = get_port_width(handle);
    int32_t buffer_size = as_logic_val_width(port_width);
    s_xsi_vlog_logicval *buffer = (s_xsi_vlog_logicval*) calloc(buffer_size, sizeof(s_xsi_vlog_logicval));

    loader.get_value(handle, buffer);
    std::vector<int8_t> result;
    result.resize(buffer_size*4, 0);

    for (int32_t i = 0; i < buffer_size; i++) {
        check_vlog_logicval(&(buffer[i]));
        reinterpret_cast<int32_t*>(result.data())[i] = buffer[i].aVal;
    }
    free(buffer);
    return result;
}

void XSIIface::write_vlog(int32_t handle, std::vector<int8_t> data) {
    int32_t port_width = get_port_width(handle);
    int32_t buffer_size = as_logic_val_width(port_width);
    s_xsi_vlog_logicval *buffer = (s_xsi_vlog_logicval*) calloc(buffer_size, sizeof(s_xsi_vlog_logicval));

    data.resize(buffer_size*sizeof(int32_t), 0);

    for (int32_t i = 0; i < buffer_size; i++) {
        buffer[i].aVal = reinterpret_cast<int32_t*>(data.data())[i];
    }

    loader.put_value(handle, buffer);
}

/*
 * Interface read functions
 */
int32_t XSIIface::read32(int32_t handle) {
    std::vector<int8_t> vec_bytes = read_vlog(handle);
    vec_bytes.resize(4, 0);
    return *reinterpret_cast<int32_t*>(vec_bytes.data());
}

int64_t XSIIface::read64(int32_t handle) {
    std::vector<int8_t> vec_bytes = read_vlog(handle);
    vec_bytes.resize(8, 0);
    return *reinterpret_cast<int64_t*>(vec_bytes.data());
}

std::vector<int8_t> XSIIface::read(int32_t handle, int32_t width) {
    std::vector<int8_t> vec_bytes = read_vlog(handle);
    vec_bytes.resize(width, 0);
    std::reverse(vec_bytes.begin(), vec_bytes.end());
    return vec_bytes;
}

/*
 * Interface write functions
 */
void XSIIface::write32(int32_t handle, int32_t data) {
    std::vector<int8_t> vec_bytes((int8_t*) &data, (int8_t*) &((&data)[1]));
    write_vlog(handle, vec_bytes);
}

void XSIIface::write64(int32_t handle, int64_t data) {
    std::vector<int8_t> vec_bytes((int8_t*) &data, (int8_t*) &((&data)[1]));
    write_vlog(handle, vec_bytes);
}

void XSIIface::write(int32_t handle, int32_t width, std::vector<int8_t> data) {
    std::reverse(data.begin(), data.end());
    write_vlog(handle, data);
}

void XSIIface::sleep(int64_t sleep_cycles) {
    loader.run(sleep_cycles);
}

void XSIIface::check_status() { }