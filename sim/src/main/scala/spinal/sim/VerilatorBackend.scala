package spinal.sim

import jnr.ffi.LibraryLoader

import scala.collection.mutable.ArrayBuffer

import sys.process._

class BackendConfig {
  val rtlSourcesPaths = ArrayBuffer[String]()
  var toplevelName: String = null
  var workspacePath: String = null
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
    s"rm ${config.workspacePath}/libV${config.toplevelName}.so".!
  }

  def genWrapperCpp(): Unit = {
    val wrapperString = s"""
#include <stdint.h>

#include "V${config.toplevelName}.h"

class ISignalAccess{
public:
    virtual CData getCData() = 0;
    virtual void setCData(CData value) = 0;
};

class  CDataSignalAccess : public ISignalAccess{
public:
    CData *raw;
    CDataSignalAccess(CData *raw) : raw(raw){

    }
    CData getCData() {return *raw;}
    void setCData(CData value)  {*raw = value; }
};

class Wrapper{
public:
    V${config.toplevelName} top;
    ISignalAccess *signalAccess[3];

    Wrapper(){
${val signalInits = for((signal, id) <- vConfig.signals.zipWithIndex)
      yield s"        signalAccess[$id] = new ${signal.dataType}SignalAccess(&top.${signal.path.mkString(".")});\n"
      signalInits.mkString("")}
    }

    virtual ~Wrapper(){
        for(int idx = 0;idx < 3;idx++){
            delete signalAccess[idx];
        }
    }
};


#ifdef __cplusplus
extern "C" {
#endif

Wrapper* wrapperNewHandle(){
    return new Wrapper;
}

void wrapperEval(Wrapper *handle){
    handle->top.eval();
}

CData wrapperGetCData(Wrapper *handle, int id){
    return handle->signalAccess[id]->getCData();
}
void wrapperSetCData(Wrapper *handle, int id, CData value){
    handle->signalAccess[id]->setCData(value);
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
