#ifndef _XSI_IFACE_
#define _XSI_IFACE_

#include <exception>
#include <string>
#include <cstdint>
#include <vector>
#include "xsi_loader.h"

class XSIException: public std::exception {
public:
    XSIException(const char* msg_): exception(), msg(msg_) {};
    virtual const char* what() const throw() {
        return msg.c_str();
    };

private:
    std::string msg;
};

class XSIIface {
public:
    XSIIface();
    ~XSIIface();
    int32_t get_signal_handle(const std::string& handle_name);
    std::vector<int8_t> read(int32_t handle, int32_t width);
    int64_t read64(int32_t handle);
    int32_t read32(int32_t handle);
    void write(int32_t handle, int32_t width, std::vector<int8_t> data);
    void write64(int32_t handle, int64_t data);
    void write32(int32_t handle, int32_t data);
    
    void sleep(int64_t sleep_cycles);
    void check_status();
private:
    Xsi::Loader loader;
    int32_t get_port_width(int32_t handle);
    std::vector<int8_t> read_vlog(int32_t handle);
    void write_vlog(int32_t handle, std::vector<int8_t> data);

    int64_t sim_time_precision;
};

#endif