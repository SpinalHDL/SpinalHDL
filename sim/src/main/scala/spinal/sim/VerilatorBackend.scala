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

class VerilatorBackendConfig{
  var signals = ArrayBuffer[Signal]()
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
  virtual void getAU8(uint8_t *value) {}
  virtual void setAU8(uint8_t *value, int length) {}

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


class  SDataSignalAccess : public ISignalAccess{
public:
    SData *raw;
    SDataSignalAccess(SData *raw) : raw(raw){

    }
    uint64_t getU64() {return *raw;}
    void setU64(uint64_t value)  {*raw = value; }
};


class  IDataSignalAccess : public ISignalAccess{
public:
    IData *raw;
    IDataSignalAccess(IData *raw) : raw(raw){

    }
    uint64_t getU64() {return *raw;}
    void setU64(uint64_t value)  {*raw = value; }
};


class  QDataSignalAccess : public ISignalAccess{
public:
    QData *raw;
    QDataSignalAccess(QData *raw) : raw(raw){

    }
    uint64_t getU64() {return *raw;}
    void setU64(uint64_t value)  {*raw = value; }
};



class  WDataSignalAccess : public ISignalAccess{
public:
    WData *raw;
    uint32_t width;
    bool sint;

    WDataSignalAccess(WData *raw, uint32_t width, bool sint) : raw(raw), width(width), sint(sint){

    }

    uint64_t getU64() {return raw[0] + (((uint64_t)raw[1]) << 32);}
    void setU64(uint64_t value)  {
      uint32_t wordsCount = (width+31)/32;
      raw[0] = value;
      raw[1] = value >> 32;
      uint32_t padding = (value & 0x8000000000000000) && sint ? 0xFFFFFFFFFFFFFFFF : 0;
      for(uint32_t idx = 2;idx < wordsCount;idx++){
        raw[idx] = padding;
      }

      if(width%32 != 0) raw[wordsCount-1] &= (1l << width%32)-1;
    }

    void getAU8(uint8_t *value) {
      uint32_t wordsCount = (width+31)/32;
      uint32_t byteCount = wordsCount*4;
      uint32_t shift = 32-(width % 32);
      uint32_t backup = raw[wordsCount-1];
      if(sint && shift != 32) raw[wordsCount-1] = (((int32_t)backup) << shift) >> shift;
      for(uint32_t idx = 0;idx < byteCount;idx++){
        value[idx + !sint] = ((uint8_t*)raw)[byteCount-idx-1];
      }
      raw[wordsCount-1] = backup;
    }

    void setAU8(uint8_t *value, int length) {
      uint32_t wordsCount = (width+31)/32;
      uint32_t padding = (value[0] & 0x80 && sint) != 0 ? 0xFFFFFFFF : 0;
      for(uint32_t idx = 0;idx < wordsCount;idx++){
        raw[idx] = padding;
      }
      uint32_t capedLength = length > 4*wordsCount ? 4*wordsCount : length;
      for(uint32_t idx = 0;idx < capedLength;idx++){
        ((uint8_t*)raw)[idx] = value[length-idx-1];
      }
      if(width%32 != 0) raw[wordsCount-1] &= (1l << width%32)-1;
    }
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
      yield s"      signalAccess[$id] = new ${if(signal.dataType.width <= 8) "CData"
      else if(signal.dataType.width <= 16) "SData"
      else if(signal.dataType.width <= 32) "IData"
      else if(signal.dataType.width <= 64) "QData"
      else "WData"}SignalAccess(${if(signal.dataType.width <= 64)"&" else ""}top.${signal.path.mkString(".")}${if(signal.dataType.width > 64) s", ${signal.dataType.width}, ${if(signal.dataType.isInstanceOf[SIntDataType]) "true" else "false"}" else ""});\n"
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
      tfp.dump(time);
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

void wrapperGetAU8(Wrapper *handle, int id, uint8_t *value){
  handle->signalAccess[id]->getAU8(value);
}
void wrapperSetAU8(Wrapper *handle, int id, uint8_t *value, int length){
  handle->signalAccess[id]->setAU8(value, length);
}


void wrapperSleep(Wrapper *handle, uint64_t cycles){
  #ifdef TRACE
  handle->tfp.dump(handle->time);
  #endif
  handle->time += cycles;
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

  class Logger extends ProcessLogger {override def err(s: => String): Unit = {println(s)}
    override def out(s: => String): Unit = {}
    override def buffer[T](f: => T) = f
  }

  def compile(): Unit = {
    s"""verilator
       |-CFLAGS -fPIC -CFLAGS -m64 -CFLAGS -shared
       |-LDFLAGS -fPIC -LDFLAGS -m64 -LDFLAGS -shared
       |${if(config.withWave) "-CFLAGS -DTRACE --trace" else ""}
       |--Mdir ${config.workspacePath}
       |--top-module ${config.toplevelName}
       |-cc ${config.rtlSourcesPaths.mkString(" ")}
       |--exe $wrapperCppPath""".stripMargin.!(new Logger())

    genWrapperCpp()
    s"make -j -C ${config.workspacePath} -f V${config.toplevelName}.mk V${config.toplevelName}".!(new Logger())
    s"cp ${config.workspacePath}/V${config.toplevelName} ${config.workspacePath}/libV${config.toplevelName}.so".!(new Logger())
  }


  clean()
  compile()
  val native = LibraryLoader.create(classOf[IVerilatorNative]).load(s"${config.workspacePath}/V${config.toplevelName}")
  def instanciate() = native.wrapperNewHandle()
}
