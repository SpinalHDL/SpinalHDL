#include <stdlib.h>
#include <string>
#include <cstring>
#include <iostream>

#include "xsi_loader.h"
#include "spinal_xsim.h"

const char* expected_out[15] = {"0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"};

void append_logic_val_bit_to_string(std::string& retVal, int aVal, int bVal)
{
     if(aVal == 0) {
        if(bVal == 0) {
           retVal +="0";
        } else {
           retVal +="Z";
        }
     } else { // aVal == 1
        if(bVal == 0) {
           retVal +="1";
        } else {
           retVal +="X";
        }
     }
}


void append_logic_val_to_string(std::string& retVal, int aVal, int bVal, int max_bits)
{
   int bit_mask = 0X00000001;
   int aVal_bit, bVal_bit;
   for(int k=max_bits; k>=0; k--) {
      aVal_bit = (aVal >> k ) & bit_mask;
      bVal_bit = (bVal >> k ) & bit_mask;
      append_logic_val_bit_to_string(retVal, aVal_bit, bVal_bit);
   }
}

std::string logic_val_to_string(s_xsi_vlog_logicval* value, int size)
{
   std::string retVal;

   int num_words = size/32 + 1;
   int max_lastword_bit = size %32 - 1;

   // last word may have unfilled bits
   int  aVal = value[num_words -1].aVal;
   int  bVal = value[num_words -1].bVal;
   append_logic_val_to_string(retVal, aVal, bVal, max_lastword_bit);
   
   // this is for fully filled 32 bit aVal/bVal structs
   for(int k = num_words - 2; k>=0; k--) {
      aVal = value[k].aVal;
      bVal = value[k].bVal;
      append_logic_val_to_string(retVal, aVal, bVal, 31);
   }
   return retVal;
}


std::string getcurrentdir()
{
#if defined(_WIN32)
    char buf[MAX_PATH];
    GetCurrentDirectory(sizeof(buf), buf);
    buf[sizeof(buf)-1] = 0;
    return buf;
#else
    char buf[1024];
    //getcwd(buf, sizeof(buf)-1);
    buf[sizeof(buf)-1] = 0;
    return buf;
#endif
}

int main(int argc, char **argv)
{
    std::string cwd = getcurrentdir();
    std::string simengine_libname = SIM_KERNEL;

    //std::string design_libname = getcurrentdir() + "/xsim.dir/counter/xsimk" + lib_extension;
    std::string design_libname = SIM_DESIGN;

    std::cout << "Design DLL     : " << design_libname << std::endl;
    std::cout << "Sim Engine DLL : " << simengine_libname << std::endl;

    // See xsi.h header for more details on how Verilog values are stored as aVal/bVal pairs

    // constants 
    const s_xsi_vlog_logicval one_val  = {0X00000001, 0X00000000};
    const s_xsi_vlog_logicval zero_val = {0X00000000, 0X00000000};

    // Output value (Up to 32 bit requires just one struct
    s_xsi_vlog_logicval count_val = {0X00000000, 0X00000000};

    // Ports
    int reset;
    int clk;
    int enable;
    int count;

    // my variables 
    int count_success = 0;
    int status = 0;

    try {
        Xsi::Loader Xsi_Instance(design_libname, simengine_libname);
        s_xsi_setup_info info;
        memset(&info, 0, sizeof(info));
        info.logFileName = NULL;
        char wdbName[] = "test.wdb";
        info.wdbFileName = wdbName;
        Xsi_Instance.open(&info);
        Xsi_Instance.trace_all();
        reset = Xsi_Instance.get_port_number("reset");
        if(reset <0) {
          std::cerr << "ERROR: reset not found" << std::endl;
          exit(1);
        }
        clk = Xsi_Instance.get_port_number("clk");
        if(clk <0) {
          std::cerr << "ERROR: clk not found" << std::endl;
          exit(1);
        }
        enable = Xsi_Instance.get_port_number("enable");
        if(enable <0) {
          std::cerr << "ERROR: enable not found" << std::endl;
          exit(1);
        }
        count = Xsi_Instance.get_port_number("count");
        if(count <0) {
          std::cerr << "ERROR: count not found" << std::endl;
          exit(1);
        }

        // Start low clock
        Xsi_Instance.put_value(clk, &zero_val);
        Xsi_Instance.run(10);

        // Reset to 1 and clock it
        Xsi_Instance.put_value(reset, &one_val);
        Xsi_Instance.put_value(enable, &zero_val);
        Xsi_Instance.put_value(clk, &one_val);
        Xsi_Instance.run(10);

        // Put clk to 0, reset to 0 and enable to 1
        Xsi_Instance.put_value(clk, &zero_val);
        Xsi_Instance.put_value(reset, &zero_val);
        Xsi_Instance.put_value(enable, &one_val);
        Xsi_Instance.run(10);

        //Test get_time functionality
        int64_t sim_time = Xsi_Instance.get_time();
        std::cout << "Simulation time: " << sim_time << std::endl;
        
        std::string count_val_string;
        // The reset is done. Now start counting
        std::cout << "\n *** starting to count ***\n";
        for (int i=0; i < 15; i++) {
           Xsi_Instance.put_value(clk, &one_val);
           Xsi_Instance.run(10);

           // read the output
           Xsi_Instance.get_value(count, &count_val);
        
           count_val_string = logic_val_to_string(&count_val, 4);
           std::cout << count_val_string << std::endl;
           if( count_val_string.compare(expected_out[i]) == 0) {
              count_success++;
           }
           // Put clk to zero
           Xsi_Instance.put_value(clk, &zero_val);
           Xsi_Instance.run(10);
        }
        std::string errorInfo = Xsi_Instance.get_error_info();
        std::cout << "ERROR info " << errorInfo << std::endl;

        std::cout << "\n *** done counting ***\n";
        
        std::cout << "Total successful checks: " << count_success <<"\n";
        status = (count_success == 15) ? 0:1;

        // Just a check to rewind time to 0
        Xsi_Instance.restart();

    }
    catch (std::exception& e) {
        std::cerr << "ERROR: An exception occurred: " << e.what() << std::endl;
        status = 2;
    }
    catch (...) {
        std::cerr << "ERROR: An unknown exception occurred." << std::endl;
        status = 3;
    }

    if(status == 0) {
      std::cout << "PASSED test\n";
    } else {
      std::cout << "FAILED test\n";
    }

    exit(status);
}


