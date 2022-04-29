#ifndef _XSI_LOADER_H_
#define _XSI_LOADER_H_

#include "xsi.h"
#include "xsi_shared_lib.h"

#include <string>
#include <exception>

namespace Xsi {

class LoaderException : public std::exception {
public:
    LoaderException(const std::string& msg) : _msg("ISim engine error: " + msg) { }

    virtual ~LoaderException() throw() { }

    virtual const char * what() const throw() { return _msg.c_str(); }
private:
    std::string _msg;
};

class Loader {
public:
    Loader(const std::string& dll_name, const std::string& simkernel_libname);
    ~Loader();

    bool isopen() const;
    void open(p_xsi_setup_info setup_info);
    void close();
    void run(XSI_INT64 step);
    void restart();
    int get_value(int port_number, void* value);
    int get_port_number(const char* port_name);
    const char *get_port_name(int port_number);
    void put_value(int port_number, const void* value);
    int get_status();
    const char* get_error_info();
    void trace_all();
    XSI_INT64 get_time();

private:
    bool initialize();

    Xsi::SharedLibrary _design_lib;
    Xsi::SharedLibrary _simkernel_lib;
    std::string _design_libname;
    std::string _simkernel_libname;

    xsiHandle _design_handle;

    t_fp_xsi_open _xsi_open;
    t_fp_xsi_close _xsi_close;
    t_fp_xsi_run _xsi_run;
    t_fp_xsi_get_value _xsi_get_value;
    t_fp_xsi_put_value _xsi_put_value;
    t_fp_xsi_get_status _xsi_get_status;
    t_fp_xsi_get_error_info _xsi_get_error_info;
    t_fp_xsi_restart _xsi_restart;
    t_fp_xsi_get_port_number _xsi_get_port_number;
    t_fp_xsi_get_port_name _xsi_get_port_name;
    t_fp_xsi_trace_all _xsi_trace_all;
    t_fp_xsi_get_time _xsi_get_time;

}; // class Loader

} // namespace Xsi

#endif // _XSI_LOADER_H_


