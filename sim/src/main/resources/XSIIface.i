%module JNIXSIIface

%include "stdint.i"
%include "std_vector.i"
%include "std_string.i"

%{
 #include "XSIIface.hpp"
%}

namespace std {
    %template(VecI8) vector<int8_t>;
};

%exception {
  try {
    $action
  } catch (XSIException &e) {
    jclass clazz = jenv->FindClass("spinal/sim/XSIException");
    jenv->ThrowNew(clazz, e.what());
    return $null;
  } catch (std::exception &e) {
    jclass clazz = jenv->FindClass("java/lang/Exception");
    jenv->ThrowNew(clazz, e.what());
    return $null;
  } catch (...) {
    jclass clazz = jenv->FindClass("java/lang/Exception");
    jenv->ThrowNew(clazz, "unknown exception");
    return $null;
  }
}

class XSIIface {
public:
    XSIIface();
    ~XSIIface();
    int32_t get_signal_handle(const std::string& handle_name);
    std::vector<int8_t> read(int32_t handle, int32_t width);
    int64_t read64(int32_t handle);
    int32_t read32(int32_t handle);
    void write(int32_t handle, int32_t width, const std::vector<int8_t>& data);
    void write64(int32_t handle, int64_t data);
    void write32(int32_t handle, int32_t data);
    
    void sleep(int64_t sleep_cycles);
    void check_status();
};

%exception;