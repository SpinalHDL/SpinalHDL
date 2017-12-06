package spinal.sim

import jnr.ffi.LibraryLoader

import scala.collection.mutable.ArrayBuffer

import sys.process._

class BackendConfig {
  val rtlSourcesPaths = ArrayBuffer[String]()
  var toplevelName: String = null
  var workspacePath: String = null
  var withWave = true
}
case class VerilatorSignal(path : Seq[String], dataType : String){

}

class VerilatorBackendConfig{
  var signals = ArrayBuffer[VerilatorSignal]()
}

class VerilatorBackend(config: BackendConfig, vConfig : VerilatorBackendConfig) {

  def wrapperCppPath = s"${config.workspacePath}/V${config.toplevelName}__spinalWrapper.cpp"

  def clean(): Unit ={
    s"rm -rf ${config.workspacePath}".!
//    s"rm ${config.workspacePath}/libV${config.toplevelName}.so".!
  }

  def genWrapperCpp(): Unit = {
    val wrapperString = s"""
#include <stdint.h>

#include "V${config.toplevelName}.h"
#include "verilated_vcd_c.h"

class ISignalAccess{
public:
 // virtual uint64_t getU64() = 0;
  //virtual void setU64(uint64_t value) = 0;

  virtual uint64_t getU64() = 0;
  virtual void setU64(uint64_t value) = 0;
};

class  CDataSignalAccess : public ISignalAccess{
public:
    CData *raw;
    CDataSignalAccess(CData *raw) : raw(raw){

    }
    uint64_t getU64() {return *raw;}
    void setU64(uint64_t value)  {*raw = value; }
};

class Wrapper{
public:
    uint64_t time;
    V${config.toplevelName} top;
    ISignalAccess *signalAccess[${vConfig.signals.length}];
    #ifdef TRACE
	  VerilatedVcdC tfp;
	  #endif

    Wrapper(){
      time = 0;
${val signalInits = for((signal, id) <- vConfig.signals.zipWithIndex)
      yield s"        signalAccess[$id] = new ${signal.dataType}SignalAccess(&top.${signal.path.mkString(".")});\n"
  signalInits.mkString("")}
      #ifdef TRACE
      Verilated::traceEverOn(true);
      top.trace(&tfp, 99);
      tfp.open("${config.workspacePath}/V${config.toplevelName}.vcd");
      #endif
    }

    virtual ~Wrapper(){
      for(int idx = 0;idx < ${vConfig.signals.length};idx++){
          delete signalAccess[idx];
      }

      #ifdef TRACE
      tfp.close();
      tfp.dump(time);
      #endif
    }

};


#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <stdint.h>
Wrapper* wrapperNewHandle(){
    Wrapper *handle = new Wrapper;
    return handle;
}
void wrapperDeleteHandle(Wrapper * handle){
    delete handle;
}

void wrapperEval(Wrapper *handle){
    handle->top.eval();
}

uint64_t wrapperGetU64(Wrapper *handle, int id){
  return handle->signalAccess[id]->getU64();
}
void wrapperSetU64(Wrapper *handle, int id, uint64_t value){
  handle->signalAccess[id]->setU64(value);
}

void wrapperSleep(Wrapper *handle, uint64_t cycles){
  #ifdef TRACE
  handle->tfp.dump(handle->time);
  #endif
  handle->time += cycles;
}

uint64_t miaou[] = {11,22,33,44};
void wrapperTest(uint32_t *arg){
  arg[0] = 55;
  arg[1] = 66;
  //return miaou;
}


void wrapperTest2(uint32_t *arg){
  arg[0] = 111;
  //return miaou;
}

#ifdef __cplusplus
}
#endif
     """
    val outFile = new java.io.FileWriter(wrapperCppPath)
    outFile.write(wrapperString)
    outFile.flush()
    outFile.close()
  }



  def compile(): Unit = {
    s"""verilator
       |-CFLAGS -fPIC -CFLAGS -m64 -CFLAGS -shared
       |-LDFLAGS -fPIC -LDFLAGS -m64 -LDFLAGS -shared
       |${if(config.withWave) "-CFLAGS -DTRACE --trace" else ""}
       |--Mdir ${config.workspacePath}
       |--top-module ${config.toplevelName}
       |-cc ${config.rtlSourcesPaths.mkString(" ")}
       |--exe $wrapperCppPath""".stripMargin.!

    genWrapperCpp()
    s"make -j -C ${config.workspacePath} -f V${config.toplevelName}.mk V${config.toplevelName}".!
    s"cp ${config.workspacePath}/V${config.toplevelName} ${config.workspacePath}/libV${config.toplevelName}.so".!
  }



  clean()
  compile()
  val native = LibraryLoader.create(classOf[IVerilatorNative]).load(s"${config.workspacePath}/V${config.toplevelName}")
  def instanciate() = native.wrapperNewHandle()
}
