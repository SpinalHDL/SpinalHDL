package spinal.sim

import javax.tools.JavaFileObject

import jnr.ffi.LibraryLoader

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import sys.process._



class VerilatorBackendConfig{
  var signals = ArrayBuffer[Signal]()
  var optimisationLevel : Int = 2
  val rtlSourcesPaths = ArrayBuffer[String]()
  var toplevelName: String = null
  var workspacePath: String = null
  var withWave = true
  var waveDepth = 1 // 0 => all
}

object VerilatorBackend{
  private var uniqueId = 0
  def allocateUniqueId() : Int = {
    this.synchronized {
      uniqueId = uniqueId + 1
      uniqueId
    }
  }
}

class VerilatorBackend(val config : VerilatorBackendConfig) {
  val uniqueId = VerilatorBackend.allocateUniqueId()

  def wrapperCppPath = s"${config.workspacePath}/V${config.toplevelName}__spinalWrapper.cpp"

  def clean(): Unit ={
    s"rm -rf ${config.workspacePath}".!
//    s"rm ${config.workspacePath}/libV${config.toplevelName}.so".!
  }

  def genWrapperCpp(): Unit = {
    val wrapperString = s"""
#include <stdint.h>
#include <string>

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

class Wrapper_${uniqueId}{
public:
    uint64_t time;
    V${config.toplevelName} top;
    ISignalAccess *signalAccess[${config.signals.length}];
    #ifdef TRACE
	  VerilatedVcdC tfp;
	  #endif

    Wrapper_${uniqueId}(const char * name){
      time = 0;
${val signalInits = for((signal, id) <- config.signals.zipWithIndex)
      yield s"      signalAccess[$id] = new ${if(signal.dataType.width <= 8) "CData"
      else if(signal.dataType.width <= 16) "SData"
      else if(signal.dataType.width <= 32) "IData"
      else if(signal.dataType.width <= 64) "QData"
      else "WData"}SignalAccess(${if(signal.dataType.width <= 64)"&" else ""}top.${signal.path.mkString(".")}${if(signal.dataType.width > 64) s", ${signal.dataType.width}, ${if(signal.dataType.isInstanceOf[SIntDataType]) "true" else "false"}" else ""});\n"
  signalInits.mkString("")}
      #ifdef TRACE
      Verilated::traceEverOn(true);
      top.trace(&tfp, 99);
      tfp.open((std::string("${config.workspacePath}/V${config.toplevelName}_") + name + ".vcd").c_str());
      #endif
    }

    virtual ~Wrapper_${uniqueId}(){
      for(int idx = 0;idx < ${config.signals.length};idx++){
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
Wrapper_${uniqueId}* wrapperSpinalNewHandle_${uniqueId}(const char * name, uint32_t seedValue){
    srand48(seedValue);
    Verilated::randReset(2);
    Wrapper_${uniqueId} *handle = new Wrapper_${uniqueId}(name);
    return handle;
}
void wrapperSpinalDeleteHandle_${uniqueId}(Wrapper_${uniqueId} * handle){
    delete handle;
}

void wrapperSpinalEval_${uniqueId}(Wrapper_${uniqueId} *handle){
    handle->top.eval();
}

uint64_t wrapperSpinalGetU64_${uniqueId}(Wrapper_${uniqueId} *handle, int id){
  return handle->signalAccess[id]->getU64();
}
void wrapperSpinalSetU64_${uniqueId}(Wrapper_${uniqueId} *handle, int id, uint64_t value){
  handle->signalAccess[id]->setU64(value);
}

void wrapperSpinalGetAU8_${uniqueId}(Wrapper_${uniqueId} *handle, int id, uint8_t *value){
  handle->signalAccess[id]->getAU8(value);
}
void wrapperSpinalSetAU8_${uniqueId}(Wrapper_${uniqueId} *handle, int id, uint8_t *value, int length){
  handle->signalAccess[id]->setAU8(value, length);
}


void wrapperSpinalSleep_${uniqueId}(Wrapper_${uniqueId} *handle, uint64_t cycles){
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

    val exportMapString =
      s"""CODEABI_1.0 {
         |    global: wrapperSpinal*;
         |    local: *;
         |};""".stripMargin

    val exportmapFile = new java.io.FileWriter(s"${config.workspacePath}/libcode.version")
    exportmapFile.write(exportMapString)
    exportmapFile.flush()
    exportmapFile.close()
  }

  class Logger extends ProcessLogger {override def err(s: => String): Unit = {println(s)}
    override def out(s: => String): Unit = {}
    override def buffer[T](f: => T) = f
  }

  def compileVerilator(): Unit = {
    // VL_THREADED
    val flags = List("-fPIC", "-m64", "-shared")
    s"""verilator
       | ${flags.map("-CFLAGS " + _).mkString(" ")}
       | ${flags.map("-LDFLAGS " + _).mkString(" ")}
       | -LDFLAGS '-Wl,--version-script=libcode.version'
       | -Wno-WIDTH -Wno-UNOPTFLAT
       | --x-assign unique
       | --trace-depth ${config.waveDepth}
       | -CFLAGS -O${config.optimisationLevel}
       | ${if(config.withWave) "-CFLAGS -DTRACE --trace" else ""}
       | --Mdir ${config.workspacePath}
       | --top-module ${config.toplevelName}
       | -cc ${config.rtlSourcesPaths.mkString(" ")}
       | --exe $wrapperCppPath""".stripMargin.!(new Logger())

    genWrapperCpp()
    s"make -j -C ${config.workspacePath} -f V${config.toplevelName}.mk V${config.toplevelName}".! (new Logger())
    s"cp ${config.workspacePath}/V${config.toplevelName} ${config.workspacePath}/libV${config.toplevelName}.so".!(new Logger())
  }

  def compileJava() : Unit = {
    val verilatorNativeCode =
      s"""package wrapper_${config.workspacePath};
         |import jnr.ffi.Pointer;
         |import jnr.ffi.annotations.IgnoreError;
         |import jnr.ffi.annotations.In;
         |import jnr.ffi.annotations.Out;
         |
         |public interface VerilatorNative {
         |    public long wrapperSpinalNewHandle_${uniqueId}(@In String name, int seed);
         |    @IgnoreError public void wrapperSpinalEval_${uniqueId}(long handle);
         |    @IgnoreError public void wrapperSpinalSleep_${uniqueId}(long handle, long cycles);
         |    @IgnoreError public long wrapperSpinalGetU64_${uniqueId}(long handle, int id);
         |    @IgnoreError public void wrapperSpinalSetU64_${uniqueId}(long handle, int id, long value);
         |    @IgnoreError public void wrapperSpinalGetAU8_${uniqueId}(long handle, int id,@Out byte[] value);
         |    @IgnoreError public void wrapperSpinalSetAU8_${uniqueId}(long handle, int id,@In byte[] value, int length);
         |    public void wrapperSpinalDeleteHandle_${uniqueId}(long handle);
         |}
       """.stripMargin
    val verilatorNativeFile = new DynamicCompiler.InMemoryJavaFileObject(s"wrapper_${config.workspacePath}.VerilatorNative", verilatorNativeCode)

    val verilatorNativeImplCode =
      s"""package wrapper_${config.workspacePath};
         |import spinal.sim.IVerilatorNative;
         |import jnr.ffi.LibraryLoader;
         |
         |public class VerilatorNativeImpl implements IVerilatorNative {
         |    public VerilatorNative jnr;
         |    public VerilatorNativeImpl(){
         |      this.jnr = LibraryLoader.create(VerilatorNative.class).load("${config.workspacePath}/V${config.toplevelName}");
         |    }
         |    public long wrapperNewHandle(String name, int seed) { return jnr.wrapperSpinalNewHandle_${uniqueId}(name, seed);}
         |    public void wrapperEval(long handle) { jnr.wrapperSpinalEval_${uniqueId}(handle);}
         |    public void wrapperSleep(long handle, long cycles) { jnr.wrapperSpinalSleep_${uniqueId}(handle, cycles);}
         |    public long wrapperGetU64(long handle, int id) { return jnr.wrapperSpinalGetU64_${uniqueId}(handle, id);}
         |    public void wrapperSetU64(long handle, int id, long value) { jnr.wrapperSpinalSetU64_${uniqueId}(handle, id, value);}
         |    public void wrapperGetAU8(long handle, int id, byte[] value) { jnr.wrapperSpinalGetAU8_${uniqueId}(handle, id, value);}
         |    public void wrapperSetAU8(long handle, int id, byte[] value, int length) { jnr.wrapperSpinalSetAU8_${uniqueId}(handle, id, value, length);}
         |    public void wrapperDeleteHandle(long handle) { jnr.wrapperSpinalDeleteHandle_${uniqueId}(handle);}
         |}
       """.stripMargin
    val verilatorNativeImplFile = new DynamicCompiler.InMemoryJavaFileObject(s"wrapper_${config.workspacePath}.VerilatorNativeImpl", verilatorNativeImplCode)
    import collection.JavaConverters._
    DynamicCompiler.compile(List[JavaFileObject](verilatorNativeFile, verilatorNativeImplFile).asJava, s"${config.workspacePath}")
  }

  clean()
  compileVerilator()
  compileJava()

  val nativeImpl = DynamicCompiler.getClass(s"wrapper_${config.workspacePath}.VerilatorNativeImpl", s"${config.workspacePath}")
  val nativeInstance : IVerilatorNative = nativeImpl.newInstance().asInstanceOf[IVerilatorNative]
//  val jnrClass = DynamicCompiler.getClass(s"wrapper_${config.workspacePath}.VerilatorNative", s"${config.workspacePath}")
//  val jnr = LibraryLoader.create(jnrClass).load("${config.workspacePath}/V${config.toplevelName}");
//  nativeInstance.getClass().getField("jnr").set(nativeInstance, jnr)
  val a = 2
  //Little memory leak (300KB)
//  System.gc()
//  println(s"Free memory => ${Runtime.getRuntime.freeMemory()/1024}/${Runtime.getRuntime.totalMemory()/1024}")
  def instanciate(name : String, seed : Int) = nativeInstance.wrapperNewHandle(name, seed)
}

