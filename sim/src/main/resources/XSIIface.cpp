#include <cstdlib>
#include <string>
#include <cstring>
#include <iostream>
#include <algorithm>

#include "spinal_xsim.h"
#include "XSIIface.hpp"
#include "xsi_loader.h"

union byte_union {
    uint32_t word;
    int8_t byte[4]; 
};

void check_vlog_logicval(s_xsi_vlog_logicval *value) {
    if (value->bVal != 0) {
        std::cout << "Unexpected \'X\' of \'Z\', reset to 0" << std::endl;
    }
    uint32_t mask = ~(value->bVal);
    value->aVal &= mask;
    value->bVal = 0;
}

int64_t as_logic_val_width(int32_t width) {
    int64_t result = (int64_t)width / 32;
    if (width % 8 != 0) {
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
}

XSIIface::~XSIIface() {}

int32_t XSIIface::get_signal_handle(const std::string& handle_name) {
    int32_t port = (int32_t)loader.get_port_number(handle_name.c_str());
    if (port < 0) {
        throw XSIException("no such port");
    }
    return port;
}

int32_t XSIIface::read32(int32_t handle) {
    s_xsi_vlog_logicval result = {0x00000000, 0x00000000};
    loader.get_value(handle, &result);
    check_vlog_logicval(&result);
    return result.aVal;
}

int64_t XSIIface::read64(int32_t handle) {
    s_xsi_vlog_logicval result[] = {{0x00000000, 0x00000000}, {0x00000000, 0x00000000}};
    loader.get_value(handle, &(result[0]));
    check_vlog_logicval(&(result[0]));
    check_vlog_logicval(&(result[1]));
    int64_t low = (int64_t)result[0].aVal;
    int64_t hi = (int64_t)result[1].aVal;
    int64_t value = (hi << 32) | low;
    return value;
}

std::vector<int8_t> XSIIface::read(int32_t handle, int32_t width) {
    int64_t buffer_width = as_logic_val_width(width);
    size_t buffer_size = sizeof(s_xsi_vlog_logicval) * buffer_width;
    s_xsi_vlog_logicval *buffer = (s_xsi_vlog_logicval *)malloc(buffer_size);
    memset(buffer, 0, buffer_size);
    loader.get_value(handle, buffer);
    std::vector<int8_t> result;
    for (size_t i = 0; i < buffer_width; i ++) {
        check_vlog_logicval(&(buffer[i]));
        byte_union u;
        u.word = buffer[i].aVal;
        result.push_back(u.byte[0]);
        result.push_back(u.byte[1]);
        result.push_back(u.byte[2]);
        result.push_back(u.byte[3]);
    }
    std::reverse(result.begin(), result.end());
    free(buffer);
    return result;
}

void XSIIface::write32(int32_t handle, int32_t data) {
    s_xsi_vlog_logicval value = {0x00000000, 0x00000000};
    value.aVal = (uint32_t)data;
    loader.put_value(handle, &value);
}

void XSIIface::write64(int32_t handle, int64_t data) {
    s_xsi_vlog_logicval value[] = {{0x00000000, 0x00000000}, {0x00000000, 0x00000000}};
    value[0].aVal = (uint32_t)data;
    value[1].aVal = (uint32_t)(data >> 32);
    loader.put_value(handle, &(value[0]));
}

void XSIIface::write(int32_t handle, int32_t width, const std::vector<int8_t>& data) {
    std::vector<int8_t> raw(data);
    std::reverse(raw.begin(), raw.end());
    int64_t buffer_width = as_logic_val_width(width);
    size_t buffer_size = sizeof(s_xsi_vlog_logicval) * buffer_width;
    s_xsi_vlog_logicval *buffer = (s_xsi_vlog_logicval *)malloc(buffer_size);
    memset(buffer, 0, buffer_size);
    byte_union u;
    for (size_t i = 0; i < raw.size(); i++) {
        size_t index = i % 4;
        u.byte[index] = raw[i];
        if (index == 3) {
            buffer[index / 4].aVal = u.word;
        }
    }
    loader.put_value(handle, buffer);
    free(buffer);
}

void XSIIface::sleep(int64_t sleep_cycles) {
    loader.run(sleep_cycles);
}

void XSIIface::check_status() { }