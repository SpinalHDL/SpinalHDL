package spinal.sim

import java.io.{File, PrintWriter}
import javax.tools.JavaFileObject
import net.openhft.affinity.impl.VanillaCpuLayout
import org.apache.commons.io.FileUtils
import spinal.SpinalEnv

import java.security.MessageDigest
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import sys.process._
import scala.io.Source
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.FileFilter

class VerilatorBackendConfig{
  var signals                = ArrayBuffer[Signal]()
  var optimisationLevel: Int = 2
  val rtlSourcesPaths        = ArrayBuffer[String]()
  val rtlIncludeDirs         = ArrayBuffer[String]()
  var toplevelName: String   = null
  var maxCacheEntries: Int   = 100
  var cachePath: String      = null
  var workspacePath: String  = null
  var workspaceName: String  = null
  var vcdPath: String        = null
  var vcdPrefix: String      = null
  var waveFormat             : WaveFormat = WaveFormat.NONE
  var waveDepth:Int          = 1 // 0 => all
  var simulatorFlags         = ArrayBuffer[String]()
  var withCoverage           = false
  var timePrecision: String  = null
}


object VerilatorBackend {
  private val cacheGlobalLock = new Object()
  private val cachePathLockMap = mutable.HashMap[String, Object]()
}

class VerilatorBackend(val config: VerilatorBackendConfig) extends Backend {
  import Backend._

  val cachePath      = config.cachePath
  val cacheEnabled   = cachePath != null
  val maxCacheEntries = config.maxCacheEntries
  val workspaceName  = config.workspaceName
  val workspacePath  = config.workspacePath
  val wrapperCppName = s"V${config.toplevelName}__spinalWrapper.cpp"
  val wrapperCppPath = new File(s"${workspacePath}/${workspaceName}/$wrapperCppName").getAbsolutePath

  def cacheGlobalSynchronized(function: => Unit) = {
    if (cacheEnabled) {
      VerilatorBackend.cacheGlobalLock.synchronized {
        function
      }
    } else {
      function
    }
  }

  def cacheSynchronized(cacheFile: File)(function: => Unit): Unit = {
    if (cacheEnabled) {
      var lock: Object = null
      VerilatorBackend.cachePathLockMap.synchronized {
        lock = VerilatorBackend.cachePathLockMap.getOrElseUpdate(cacheFile.getCanonicalPath(), new Object())
      }

      lock.synchronized {
        function
      }
    } else {
      function
    }
  }

  def clean(): Unit = {
    FileUtils.deleteQuietly(new File(s"${workspacePath}/${workspaceName}"))
  }

  val availableFormats = Array(WaveFormat.VCD, WaveFormat.FST, 
                               WaveFormat.DEFAULT, WaveFormat.NONE)

  val format = if(availableFormats contains config.waveFormat){
                config.waveFormat  
              } else {
                println("Wave format " + config.waveFormat + " not supported by Verilator")
                WaveFormat.NONE
              }

  def genWrapperCpp(useTimePrecision: Boolean = true): Unit = {
    val jniPrefix = "Java_" + s"wrapper_${workspaceName}".replace("_", "_1") + "_VerilatorNative_"
    val wrapperString = s"""
#include <stdint.h>
#include <string>
#include <memory>
#include <jni.h>
#include <iostream>

#include "V${config.toplevelName}.h"
#ifdef TRACE
#include "verilated_${format.ext}_c.h"
#endif
#include "V${config.toplevelName}__Syms.h"

using namespace std;

class ISignalAccess{
public:
  virtual ~ISignalAccess() {}

  virtual void getAU8(JNIEnv *env, jbyteArray value) {}
  virtual void getAU8_mem(JNIEnv *env, jbyteArray value, size_t index) {}
  virtual void setAU8(JNIEnv *env, jbyteArray value, int length) {}
  virtual void setAU8_mem(JNIEnv *env, jbyteArray value, int length, size_t index) {}

  virtual uint64_t getU64() = 0;
  virtual uint64_t getU64_mem(size_t index) = 0;
  virtual void setU64(uint64_t value) = 0;
  virtual void setU64_mem(uint64_t value, size_t index) = 0;
};

class  CDataSignalAccess : public ISignalAccess{
public:
    CData *raw;
    CDataSignalAccess(CData *raw) : raw(raw){}
    CDataSignalAccess(CData &raw) : raw(addressof(raw)){}
    uint64_t getU64() {return *raw;}
    uint64_t getU64_mem(size_t index) {return raw[index];}
    void setU64(uint64_t value)  {*raw = value; }
    void setU64_mem(uint64_t value, size_t index){raw[index] = value; }
};


class  SDataSignalAccess : public ISignalAccess{
public:
    SData *raw;
    SDataSignalAccess(SData *raw) : raw(raw){}
    SDataSignalAccess(SData &raw) : raw(addressof(raw)){}
    uint64_t getU64() {return *raw;}
    uint64_t getU64_mem(size_t index) {return raw[index];}
    void setU64(uint64_t value)  {*raw = value; }
    void setU64_mem(uint64_t value, size_t index){raw[index] = value; }
};


class  IDataSignalAccess : public ISignalAccess{
public:
    IData *raw;
    IDataSignalAccess(IData *raw) : raw(raw){}
    IDataSignalAccess(IData &raw) : raw(addressof(raw)){}
    uint64_t getU64() {return *raw;}
    uint64_t getU64_mem(size_t index) {return raw[index];}
    void setU64(uint64_t value)  {*raw = value; }
    void setU64_mem(uint64_t value, size_t index){raw[index] = value; }
};


class  QDataSignalAccess : public ISignalAccess{
public:
    QData *raw;
    QDataSignalAccess(QData *raw) : raw(raw){}
    QDataSignalAccess(QData &raw) : raw(addressof(raw)){}
    uint64_t getU64() {return *raw;}
    uint64_t getU64_mem(size_t index) {return raw[index];}
    void setU64(uint64_t value)  {*raw = value; }
    void setU64_mem(uint64_t value, size_t index){raw[index] = value; }
};

class  WDataSignalAccess : public ISignalAccess{
public:
    WData *raw;
    uint32_t width;
    uint32_t wordsCount;
    bool sint;

    WDataSignalAccess(WData *raw, uint32_t width, bool sint) : 
      raw(raw), width(width), wordsCount((width+31)/32), sint(sint) {}

    uint64_t getU64_mem(size_t index) {
      WData *mem_el = &(raw[index*wordsCount]);
      return mem_el[0] + (((uint64_t)mem_el[1]) << 32);
    }

    uint64_t getU64() { return getU64_mem(0); }

    void setU64_mem(uint64_t value, size_t index)  {
      WData *mem_el = &(raw[index*wordsCount]);
      mem_el[0] = value;
      mem_el[1] = value >> 32;
      uint32_t padding = ((value & 0x8000000000000000l) && sint) ? 0xFFFFFFFF : 0;
      for(uint32_t idx = 2;idx < wordsCount;idx++){
        mem_el[idx] = padding;
      }

      if(width%32 != 0) mem_el[wordsCount-1] &= (1l << width%32)-1;
    }

    void setU64(uint64_t value)  {
      setU64_mem(value, 0);
    }
    
    void getAU8_mem(JNIEnv *env, jbyteArray value, size_t index) {
      WData *mem_el = &(raw[index*wordsCount]);
      uint32_t byteCount = wordsCount*4;
      uint32_t shift = 32-(width % 32);
      uint32_t backup = mem_el[wordsCount-1];
      uint8_t values[byteCount + !sint];
      if(sint && shift != 32) mem_el[wordsCount-1] = (((int32_t)backup) << shift) >> shift;
      for(uint32_t idx = 0;idx < byteCount;idx++){
        values[idx + !sint] = ((uint8_t*)mem_el)[byteCount-idx-1];
      }
      (env)->SetByteArrayRegion ( value, 0, byteCount + !sint, reinterpret_cast<jbyte*>(values));
      mem_el[wordsCount-1] = backup;
    }
  
    void getAU8(JNIEnv *env, jbyteArray value) {
      getAU8_mem(env, value, 0);
    }

    void setAU8_mem(JNIEnv *env, jbyteArray jvalue, int length, size_t index) {
      WData *mem_el = &(raw[index*wordsCount]);
      jbyte value[length];
      (env)->GetByteArrayRegion( jvalue, 0, length, value);
      uint32_t padding = (value[0] & 0x80 && sint) != 0 ? 0xFFFFFFFF : 0;
      for(uint32_t idx = 0;idx < wordsCount;idx++){
        mem_el[idx] = padding;
      }
      uint32_t capedLength = length > 4*wordsCount ? 4*wordsCount : length;
      for(uint32_t idx = 0;idx < capedLength;idx++){
        ((uint8_t*)mem_el)[idx] = value[length-idx-1];
      }
      if(width%32 != 0) mem_el[wordsCount-1] &= (1l << width%32)-1;
    }

    void setAU8(JNIEnv *env, jbyteArray jvalue, int length) {
      setAU8_mem(env, jvalue, length, 0);
    }
};

class Wrapper_${uniqueId};
thread_local Wrapper_${uniqueId} *simHandle${uniqueId};

#include <chrono>
using namespace std::chrono;

class Wrapper_${uniqueId}{
public:
    uint64_t time;
    high_resolution_clock::time_point lastFlushAt;
    uint32_t timeCheck;
    bool waveEnabled;
    //VerilatedContext* contextp; //Buggy in multi threaded spinalsim
    V${config.toplevelName} *top;
    ISignalAccess *signalAccess[${config.signals.length}];
    #ifdef TRACE
	  Verilated${format.ext.capitalize}C tfp;
	  #endif
    string name;
    int32_t time_precision;

    Wrapper_${uniqueId}(const char * name, const char * wavePath, int seed){
      //contextp = new VerilatedContext;
      Verilated::randReset(2);
      Verilated::randSeed(seed);
      top = new V${config.toplevelName}();

      simHandle${uniqueId} = this;
      time = 0;
      timeCheck = 0;
      lastFlushAt = high_resolution_clock::now();
      waveEnabled = true;
${    val signalInits = for((signal, id) <- config.signals.zipWithIndex) yield {
      val typePrefix = if(signal.dataType.width <= 8) "CData"
      else if(signal.dataType.width <= 16) "SData"
      else if(signal.dataType.width <= 32) "IData"
      else if(signal.dataType.width <= 64) "QData"
      else "WData"
      val enforcedCast = if(signal.dataType.width > 64) "(WData*)" else ""
      val signalReference = s"top->${signal.path.map(_.replace("$", "__024")).mkString("->")}"
      val memPatch = if(signal.dataType.isMem) "[0]" else ""

      s"      signalAccess[$id] = new ${typePrefix}SignalAccess($enforcedCast $signalReference$memPatch ${if(signal.dataType.width > 64) s" , ${signal.dataType.width}, ${if(signal.dataType.isInstanceOf[SIntDataType]) "true" else "false"}" else ""});\n"

    }

      signalInits.mkString("")
    }
      #ifdef TRACE
      Verilated::traceEverOn(true);
      top->trace(&tfp, 99);
      tfp.open((std::string(wavePath) + name + ".${format.ext}").c_str());
      #endif
      this->name = name;
      this->time_precision = ${if (useTimePrecision) "Verilated::timeprecision()" else "VL_TIME_PRECISION" };
    }

    virtual ~Wrapper_${uniqueId}(){
      for(int idx = 0;idx < ${config.signals.length};idx++){
          delete signalAccess[idx];
      }

      #ifdef TRACE
      if(waveEnabled) tfp.dump((vluint64_t)time);
      tfp.flush();
      tfp.close();
      #endif
      #ifdef COVERAGE
      VerilatedCov::write((("${new File(config.vcdPath).getAbsolutePath.replace("\\","\\\\")}/${if(config.vcdPrefix != null) config.vcdPrefix + "_" else ""}") + name + ".dat").c_str());
      #endif

      // Verilated::runFlushCallbacks();
      // Verilated::runExitCallbacks();

      //contextp->gotFinish(true);
      top->final();
      delete top;
      //delete contextp;
    }

};

double sc_time_stamp () {
  return simHandle${uniqueId}->time;
}


void vl_finish(const char* filename, int linenum, const char* hier) VL_MT_UNSAFE {
    if (false && hier) {}
    VL_PRINTF(  // Not VL_PRINTF_MT, already on main thread
        "- %s:%d: Verilog $$finish\\n", filename, linenum);
   /*if (Verilated::threadContextp()->gotFinish()) {
        VL_PRINTF(  // Not VL_PRINTF_MT, already on main thread
            "- %s:%d: Second verilog $$finish, exiting\\n", filename, linenum);
        Verilated::runFlushCallbacks();
        Verilated::runExitCallbacks();
        std::exit(0);
    }*/
    Verilated::threadContextp()->gotFinish(true);
}

#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <stdint.h>

#define API __attribute__((visibility("default")))


JNIEXPORT Wrapper_${uniqueId} * API JNICALL ${jniPrefix}newHandle_1${uniqueId}
  (JNIEnv * env, jobject obj, jstring name, jstring wavePath, jint seedValue){
    #if defined(_WIN32) && !defined(__CYGWIN__)
    srand(seedValue);
    #else
    srand48(seedValue);
    #endif
    const char* ch = env->GetStringUTFChars(name, 0);
    const char* wavePathCh = env->GetStringUTFChars(wavePath, 0);
    Wrapper_${uniqueId} *handle = new Wrapper_${uniqueId}(ch, wavePathCh, seedValue);
    env->ReleaseStringUTFChars(name, ch);
    env->ReleaseStringUTFChars(wavePath, wavePathCh);
    return handle;
}

JNIEXPORT jboolean API JNICALL ${jniPrefix}eval_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle){
   handle->top->eval();
   return Verilated::gotFinish();
}

JNIEXPORT jint API JNICALL ${jniPrefix}getTimePrecision_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle){
  return handle->time_precision;
}

JNIEXPORT void API JNICALL ${jniPrefix}sleep_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle, uint64_t cycles){
  #ifdef TRACE
  if(handle->waveEnabled) {
    handle->tfp.dump((vluint64_t)handle->time);
  }
  handle->timeCheck++;
  if(handle->timeCheck > 10000){
    handle->timeCheck = 0;
    high_resolution_clock::time_point timeNow = high_resolution_clock::now();
    duration<double, std::milli> time_span = timeNow - handle->lastFlushAt;
    if(time_span.count() > 1e3){
      handle->lastFlushAt = timeNow;
      handle->tfp.flush();
    }
  }
  #endif
  handle->time += cycles;
}

JNIEXPORT jlong API JNICALL ${jniPrefix}getU64_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle, int id){
  return handle->signalAccess[id]->getU64();
}

JNIEXPORT jlong API JNICALL ${jniPrefix}getU64mem_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle, int id, uint64_t index){
  return handle->signalAccess[id]->getU64_mem(index);
}

JNIEXPORT void API JNICALL ${jniPrefix}setU64_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle, int id, uint64_t value){
  handle->signalAccess[id]->setU64(value);
}

JNIEXPORT void API JNICALL ${jniPrefix}setU64mem_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle, int id, uint64_t value, uint64_t index){
  handle->signalAccess[id]->setU64_mem(value, index);
}

JNIEXPORT void API JNICALL ${jniPrefix}deleteHandle_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} * handle){
  delete handle;
}

JNIEXPORT void API JNICALL ${jniPrefix}getAU8_1${uniqueId}
  (JNIEnv * env, jobject obj, Wrapper_${uniqueId} * handle, jint id, jbyteArray value){
  handle->signalAccess[id]->getAU8(env, value);
}

JNIEXPORT void API JNICALL ${jniPrefix}getAU8mem_1${uniqueId}
  (JNIEnv * env, jobject obj, Wrapper_${uniqueId} * handle, jint id, jbyteArray value, uint64_t index){
  handle->signalAccess[id]->getAU8_mem(env, value, index);
}

JNIEXPORT void API JNICALL ${jniPrefix}setAU8_1${uniqueId}
  (JNIEnv * env, jobject obj, Wrapper_${uniqueId} * handle, jint id, jbyteArray value, jint length){
  handle->signalAccess[id]->setAU8(env, value, length);
}

JNIEXPORT void API JNICALL ${jniPrefix}setAU8mem_1${uniqueId}
  (JNIEnv * env, jobject obj, Wrapper_${uniqueId} * handle, jint id, jbyteArray value, jint length, uint64_t index){
  handle->signalAccess[id]->setAU8_mem(env, value, length, index);
}

JNIEXPORT void API JNICALL ${jniPrefix}enableWave_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} * handle){
  handle->waveEnabled = true;
}

JNIEXPORT void API JNICALL ${jniPrefix}disableWave_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} * handle){
  handle->waveEnabled = false;
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
         |    global: $jniPrefix*;
         |    local: *;
         |};""".stripMargin

    val exportmapFile = new java.io.FileWriter(s"${workspacePath}/${workspaceName}/libcode.version")
    exportmapFile.write(exportMapString)
    exportmapFile.flush()
    exportmapFile.close()
  }

  class Logger extends ProcessLogger {
    var outStr = new StringBuilder()
    override def err(s: => String): Unit = { if(!s.startsWith("ar: creating ")) println(s) }
    override def out(s: => String): Unit = { outStr ++= s; outStr ++= "\n" }
    override def buffer[T](f: => T) = f
  }

//     VL_THREADED
  def compileVerilator(): Unit = {
    val java_home = System.getProperty("java.home")
    assert(java_home != "" && java_home != null, "JAVA_HOME need to be set")
    val jdk = java_home.replace("/jre","").replace("\\jre","")
    val jdkIncludes = if(isWindows){
      new File(s"${workspacePath}\\${workspaceName}").mkdirs()
      FileUtils.copyDirectory(new File(s"$jdk\\include"), new File(s"${workspacePath}\\${workspaceName}\\jniIncludes"))
      s"jniIncludes"
    }else{
      jdk + "/include"
    }

    val arch = System.getProperty("os.arch")
    val flags   = if(isMac) List("-dynamiclib") else (if(arch == "arm" || arch == "aarch64" || arch == "loongarch64") List("-fPIC", "-shared", "-Wno-attributes") else List("-fPIC", "-m64", "-shared", "-Wno-attributes"))

    val waveArgs = format match {
      case WaveFormat.FST =>  "-CFLAGS -DTRACE --trace-fst"
      case WaveFormat.VCD =>  "-CFLAGS -DTRACE --trace"
      case WaveFormat.NONE => ""
      // Other formats are not supported by Verilator
      case _ => ???
    }

    val covArgs = config.withCoverage match {
      case true =>  "-CFLAGS -DCOVERAGE --coverage"
      case false => ""
    }

    val timeScaleArgs = config.timePrecision match {
      case null => ""
      case _ => s"--timescale-override /${config.timePrecision.replace(" ", "")}"
    }

    val rtlIncludeDirsArgs = config.rtlIncludeDirs.map(e => s"-I${new File(e).getAbsolutePath}")
      .map('"' + _.replace("\\","/") + '"').mkString(" ")

    val verilatorBinFilename = if(isWindows) "verilator_bin.exe" else "verilator"

    // allow a user to overwrite/add verilator flags, e.g. C++ version
    // if the default is too old (see e.g. #278)
    val envFlags = sys.env.getOrElse("SPINAL_VERILATOR_FLAGS", "")

    // when changing the verilator script, the hash generation (below) must also be updated
    val verilatorScript = s""" set -e ;
       | ${verilatorBinFilename}
       | ${flags.map("-CFLAGS " + _).mkString(" ")}
       | ${flags.map("-LDFLAGS " + _).mkString(" ")}
       | -CFLAGS -I"$jdkIncludes" -CFLAGS -I"$jdkIncludes/${if(isWindows)"win32" else (if(isMac) "darwin" else (if(isFreeBsd) "freebsd" else "linux"))}"
       | -CFLAGS -fvisibility=hidden
       | -LDFLAGS -fvisibility=hidden
       | -CFLAGS -DVL_USER_FINISH=1
       | --autoflush  
       | --output-split 5000
       | --output-split-cfuncs 500
       | --output-split-ctrace 500
       | -Wno-WIDTH -Wno-UNOPTFLAT -Wno-CMPCONST -Wno-UNSIGNED
       | --x-assign unique
       | --x-initial-edge
       | --trace-depth ${config.waveDepth}
       | -O3
       | -CFLAGS -O${config.optimisationLevel}
       | $waveArgs
       | $covArgs
       | $timeScaleArgs
       | --Mdir ${workspaceName}
       | --top-module ${config.toplevelName}
       | $rtlIncludeDirsArgs
       | -cc ${config.rtlSourcesPaths.filter(e => e.endsWith(".v") || 
                                                  e.endsWith(".sv") || 
                                                  e.endsWith(".h"))
                                     .map(new File(_).getAbsolutePath)
                                     .map('"' + _.replace("\\","/") + '"')
                                     .mkString(" ")}
       | --exe $workspaceName/$wrapperCppName
       | $envFlags
       | ${config.simulatorFlags.mkString(" ")}""".stripMargin.replace("\n", "")


    val workspaceDir = new File(s"${workspacePath}/${workspaceName}")
    var workspaceCacheDir: File = null
    var hashCacheDir: File = null

    val verilatorVersionProcess = Process(Seq(verilatorBinFilename, "--version"), new File(workspacePath))
    val verilatorVersion = verilatorVersionProcess.lineStream.mkString("\n") // blocks and throws an exception if exit status != 0
    val verilatorVersionDeci = BigDecimal(verilatorVersion.split(" ")(1))

    if (cacheEnabled) {
      // calculate hash of verilator version+options and source file contents

      val md = MessageDigest.getInstance("SHA-1")

      md.update(cachePath.getBytes())
      md.update(0.toByte)
      md.update(flags.mkString(" ").getBytes())
      md.update(0.toByte)
      md.update(config.waveDepth.toString().getBytes())
      md.update(0.toByte)
      md.update(config.optimisationLevel.toString().getBytes())
      md.update(0.toByte)
      md.update(waveArgs.getBytes())
      md.update(0.toByte)
      md.update(covArgs.getBytes())
      md.update(0.toByte)
      md.update(config.toplevelName.getBytes())
      md.update(0.toByte)
      md.update(config.simulatorFlags.mkString(" ").getBytes())
      md.update(0.toByte)
      md.update(verilatorVersion.getBytes())


      def hashFile(md: MessageDigest, file: File) = {
        val bis = new BufferedInputStream(new FileInputStream(file))
        val buf = new Array[Byte](1024)

        Iterator.continually(bis.read(buf, 0, buf.length))
          .takeWhile(_ >= 0)
          .foreach(md.update(buf, 0, _))

        bis.close()
      }

      config.rtlIncludeDirs.foreach { dirname =>
        FileUtils.listFiles(new File(dirname), null, true).asScala.foreach { file =>
          hashFile(md, file)
          md.update(0.toByte)
        }

        md.update(0.toByte)
      }

      config.rtlSourcesPaths.foreach { filename =>
        hashFile(md, new File(filename))
        md.update(0.toByte)
      }

      val digest = md.digest()
      val hash = digest.map(x => (x & 0xFF).toHexString.padTo(2, '0')).mkString("")
      workspaceCacheDir = new File(s"${cachePath}/${hash}/${workspaceName}")
      hashCacheDir = new File(s"${cachePath}/${hash}")
      uniqueId = BigInt(digest).toLong.abs

      cacheGlobalSynchronized {
        // remove old cache entries

        val cacheDir = new File(cachePath)
        if (cacheDir.isDirectory()) {
          if (maxCacheEntries > 0) {
            val cacheEntriesArr = cacheDir.listFiles()
              .filter(_.isDirectory())
              .sortWith(_.lastModified() < _.lastModified())

            val cacheEntries = cacheEntriesArr.toBuffer
            val cacheEntryFound = workspaceCacheDir.isDirectory()

            while (cacheEntries.length > maxCacheEntries || (!cacheEntryFound && cacheEntries.length >= maxCacheEntries)) {
              if (cacheEntries(0).getCanonicalPath() != hashCacheDir.getCanonicalPath()) {
                cacheSynchronized(cacheEntries(0)) {
                  FileUtils.deleteQuietly(cacheEntries(0))
                }
              }

              cacheEntries.remove(0)
            }
          }
        }
      }
    }

    cacheSynchronized(hashCacheDir) {
      var useCache = false

      if (cacheEnabled) {
        if (workspaceCacheDir.isDirectory()) {
          println("[info] Found cached verilator binaries")
          useCache = true
        }
      }

      var lastTime = System.currentTimeMillis()

      def bench(msg : String): Unit ={
        val newTime = System.currentTimeMillis()
        val sec = (newTime-lastTime)*1e-3
        println(msg + " " + sec)
        lastTime = newTime
      }

      val verilatorScriptFile = new PrintWriter(new File(workspacePath + "/verilatorScript.sh"))
      verilatorScriptFile.write(verilatorScript)
      verilatorScriptFile.close

      // invoke verilator or copy cached files depending on whether cache is not used or used
      val libExt = if(isWindows) "dll" else (if(isMac) "dylib" else "so")
      if (!useCache) {
        val shCommand = if(isWindows) "sh.exe" else "sh"
        val logger = new Logger()
        assert(Process(Seq(shCommand, "verilatorScript.sh"),
                       new File(workspacePath)).! (logger) == 0, "Verilator invocation failed\n" + logger.outStr.toString())
        val threadCount = SimManager.cpuCount
        genWrapperCpp(verilatorVersionDeci >= BigDecimal("4.034"))
        assert(s"${SpinalEnv.makeCmd} -j$threadCount VM_PARALLEL_BUILDS=1 -C ${workspacePath}/${workspaceName} -f V${config.toplevelName}.mk V${config.toplevelName} CURDIR=${workspacePath}/${workspaceName}".!  (logger) == 0, "Verilator C++ model compilation failed\n" + logger.outStr.toString())
        FileUtils.copyFile(new File(s"${workspacePath}/${workspaceName}/V${config.toplevelName}${if(isWindows) ".exe" else ""}") , new File(s"${workspacePath}/${workspaceName}/${workspaceName}_$uniqueId.${libExt}"))
      } else {
        FileUtils.copyDirectory(workspaceCacheDir, workspaceDir)
      }


      if (cacheEnabled) {
        // update cache

        if (!useCache) {
          FileUtils.deleteQuietly(workspaceCacheDir)

          // copy only needed files to save disk space
          FileUtils.copyDirectory(workspaceDir, workspaceCacheDir, new FileFilter() {
            def accept(file: File): Boolean = file.getName().endsWith("." + libExt)
          })
        }

        FileUtils.touch(hashCacheDir)
      }
    }
  }

  def compileJava(): Unit = {
    val verilatorNativeImplCode =
      s"""package wrapper_${workspaceName};
         |import spinal.sim.IVerilatorNative;
         |
         |public class VerilatorNative implements IVerilatorNative {
         |    public long newHandle(String name, String wavePath, int seed) { return newHandle_${uniqueId}(name, wavePath, seed);}
         |    public boolean eval(long handle) { return eval_${uniqueId}(handle);}
         |    public int get_time_precision(long handle) { return getTimePrecision_${uniqueId}(handle);}
         |    public void sleep(long handle, long cycles) { sleep_${uniqueId}(handle, cycles);}
         |    public long getU64(long handle, int id) { return getU64_${uniqueId}(handle, id);}
         |    public long getU64_mem(long handle, int id, long index) { return getU64mem_${uniqueId}(handle, id, index);}
         |    public void setU64(long handle, int id, long value) { setU64_${uniqueId}(handle, id, value);}
         |    public void setU64_mem(long handle, int id, long value, long index) { setU64mem_${uniqueId}(handle, id, value, index);}
         |    public void getAU8(long handle, int id, byte[] value) { getAU8_${uniqueId}(handle, id, value);}
         |    public void getAU8_mem(long handle, int id, byte[] value, long index) { getAU8mem_${uniqueId}(handle, id, value, index);}
         |    public void setAU8(long handle, int id, byte[] value, int length) { setAU8_${uniqueId}(handle, id, value, length);}
         |    public void setAU8_mem(long handle, int id, byte[] value, int length, long index) { setAU8mem_${uniqueId}(handle, id, value, length, index);}
         |    public void deleteHandle(long handle) { deleteHandle_${uniqueId}(handle);}
         |    public void enableWave(long handle) { enableWave_${uniqueId}(handle);}
         |    public void disableWave(long handle) { disableWave_${uniqueId}(handle);}
         |
         |
         |    public native long newHandle_${uniqueId}(String name, String wavePath, int seed);
         |    public native boolean eval_${uniqueId}(long handle);
         |    public native int getTimePrecision_${uniqueId}(long handle);
         |    public native void sleep_${uniqueId}(long handle, long cycles);
         |    public native long getU64_${uniqueId}(long handle, int id);
         |    public native long getU64mem_${uniqueId}(long handle, int id, long index);
         |    public native void setU64_${uniqueId}(long handle, int id, long value);
         |    public native void setU64mem_${uniqueId}(long handle, int id, long value, long index);
         |    public native void getAU8_${uniqueId}(long handle, int id, byte[] value);
         |    public native void getAU8mem_${uniqueId}(long handle, int id, byte[] value, long index);
         |    public native void setAU8_${uniqueId}(long handle, int id, byte[] value, int length);
         |    public native void setAU8mem_${uniqueId}(long handle, int id, byte[] value, int length, long index);
         |    public native void deleteHandle_${uniqueId}(long handle);
         |    public native void enableWave_${uniqueId}(long handle);
         |    public native void disableWave_${uniqueId}(long handle);
         |
         |    static{
         |      System.load("${new File(s"${workspacePath}/${workspaceName}").getAbsolutePath.replace("\\","\\\\")}/${workspaceName}_$uniqueId.${if(isWindows) "dll" else (if(isMac) "dylib" else "so")}");
         |    }
         |}
       """.stripMargin

    val verilatorNativeImplFile = new DynamicCompiler.InMemoryJavaFileObject(s"wrapper_${workspaceName}.VerilatorNative", verilatorNativeImplCode)
    import collection.JavaConverters._
    DynamicCompiler.compile(List[JavaFileObject](verilatorNativeImplFile).asJava, s"${workspacePath}/${workspaceName}")
  }

  def checks(): Unit ={
    if(System.getProperty("java.class.path").contains("sbt-launch.jar")){
      System.err.println("""[Error] It look like you are running the simulation with SBT without having the SBT 'fork := true' configuration.\n  Add it in the build.sbt file to fix this issue, see https://github.com/SpinalHDL/SpinalTemplateSbt/blob/master/build.sbt""")
      throw new Exception()
    }
  }

  clean()
  checks()
  compileVerilator()
  compileJava()

  val nativeImpl = DynamicCompiler.getClass(s"wrapper_${workspaceName}.VerilatorNative", s"${workspacePath}/${workspaceName}")
  val nativeInstance: IVerilatorNative = nativeImpl.getConstructor().newInstance().asInstanceOf[IVerilatorNative]

  def instanciate(name: String, seed: Int) = nativeInstance.synchronized{ //synchronized is used as a Verilator isn't thread safe on construction (bug ?)
    val patched = config.vcdPath.replace("$TEST", name)
    val wavePath = s"${new File(patched).getAbsolutePath.replace("\\","\\\\")}/${if(config.vcdPrefix != null) config.vcdPrefix + "_" else ""}"
    FileUtils.forceMkdirParent(new File(wavePath, "."))
    nativeInstance.newHandle(name, wavePath, seed)
  }

  override def isBufferedWrite: Boolean = false
}

