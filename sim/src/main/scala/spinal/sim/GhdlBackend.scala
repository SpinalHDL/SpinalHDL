package spinal.sim

import java.io.{File, PrintWriter}

import javax.tools.JavaFileObject
import net.openhft.affinity.impl.VanillaCpuLayout
import org.apache.commons.io.FileUtils

import scala.collection.mutable.ArrayBuffer
import scala.sys.process._


class BackendConfig{
  var signals                = ArrayBuffer[Signal]()
  var optimisationLevel: Int = 2
  val rtlSourcesPaths        = ArrayBuffer[String]()
  var toplevelName: String   = null
  var workspacePath: String  = null
  var workspaceName: String  = null
  var wavePath: String        = null
  var wavePrefix: String      = null
  var waveFormat             : WaveFormat = WaveFormat.NONE
  var waveDepth:Int          = 1 // 0 => all
  var simulatorFlags         = ArrayBuffer[String]()
}


class GhdlBackend(val config: BackendConfig) extends Backend {
  import Backend._

  val workspaceName  = config.workspaceName
  val workspacePath  = config.workspacePath
  val wrapperCppName = s"V${config.toplevelName}__spinalWrapper.cpp"
  val wrapperCppPath = new File(s"${workspacePath}/${workspaceName}/$wrapperCppName").getAbsolutePath


  def clean(): Unit = {
    FileUtils.deleteQuietly(new File(s"${workspacePath}/${workspaceName}"))
  }

  def genWrapperCpp(): Unit = {
    val jniPrefix = "Java_" + s"wrapper_${workspaceName}".replace("_", "_1") + "_GhdlNative_"
    val wrapperString = s"""
#ifdef __cplusplus
extern "C" {
#endif
#include <cstdint>
#include <string>
#include <vector>
#include "ghdlIface.h"

#define API __attribute__((visibility("default")))


JNIEXPORT HdlSimulation * API JNICALL ${jniPrefix}newSimulation${uniqueId}
  (JNIEnv* env, jobject obj, jstring simuPath, jint waveFormat, jstring wavePath){
    
    (void) obj;
    
    char* simuPath_ = env->GetStringChars(simuPath, 0);
    WaveFormat waveFormat = static_cast<WaveFormat>(waveFormat);
    char* wavePath_ = env->GetStringChars(wavePath, 0);

    std::string std_simuPath(simuPath_);
    std::string std_wavePath(wavePath_);

    HdlSimulation *simu = newHdlSimulation(std_simuPath, waveFormat, std_wavePath);
    env->ReleaseStringChars(simuPath, simuPath_);
    env->ReleaseStringChars(wavePath, wavePath_);
    return simu;
}


JNIEXPORT jlong API JNICALL ${jniPrefix}getHandle_1${uniqueId}
  (JNIEnv* env, jobject obj, HdlSimulation* simu, jstring handleName){
  (void) obj;

  char* handleName_ = env->GetStringChars(handleName, 0);
  
  std::string std_handleName(handleName_);
  uint64_t handle = reinterpret_cast<uint64_t>(simu->getHandle(std_handleName));
  env->ReleaseStringChars(handlName, handleName_);
  return (jlong) handle;
}

JNIEXPORT jint API JNICALL ${jniPrefix}getU32_1${uniqueId}
  (JNIEnv* env, jobject obj, HdlSimulation* simu, jlong handle){
    (void) env;
    (void) obj;
    vpiHandle handle_ = reinterpret_cast<vpiHandle>(handle);
    return (jint) simu->getInt(handle_);
}

JNIEXPORT void API JNICALL ${jniPrefix}setU32_1${uniqueId}
  (JNIEnv *, jobject obj, HdlSimulation* simu, jlong handle, jint value){
  (void) env;
  (void) obj;
  vpiHandle handle_ = reinterpret_cast<vpiHandle>(handle);
  simu->setInt(handle_, (uint32_t) value);
}

JNIEXPORT jlong API JNICALL ${jniPrefix}getU64_1${uniqueId}
  (JNIEnv* env, jobject obj, HdlSimulation* simu, jlong handle){
    (void) env;
    (void) obj;
    vpiHandle handle_ = reinterpret_cast<vpiHandle>(handle);
    return (jlong) simu->getLong(handle_);
}

JNIEXPORT void API JNICALL ${jniPrefix}setU64_1${uniqueId}
  (JNIEnv*, jobject obj, HdlSimulation* simu, jlong handle, jlong value){
  (void) env;
  (void) obj;
  vpiHandle handle_ = reinterpret_cast<vpiHandle>(handle);
  simu->setLong(handle_, (uint64_t) value);
}

JNIEXPORT jbyteArray API JNICALL ${jniPrefix}getAU8_1${uniqueId}
  (JNIEnv* env, jobject obj, HdlSimulation* simu, jlong handle){
  (void) obj;
  vpiHandle handle_ = reinterpret_cast<vpiHandle>(handle);
  std::vector<uint8_t> vec = simu->getBigInt(handle_);
  jbyteArray arr = env->NewByteArray(vec.size());
  env->SetByteArrayRegion(arr, 0, vec.size(), reinterpret_cast<jbyte*>(vec.data));
  return arr;
}

JNIEXPORT void API JNICALL ${jniPrefix}setAU8_1${uniqueId}
  (JNIEnv* env, jobject obj, HdlSimulator* simu, jlong handle, jbyteArray value){
  (void) obj;
  vpiHandle handle_ = reinterpret_cast<vpiHandle>(handle);
  std::vector<uint8_t> vec;
  vec.resize((size_t) env->GetArrayLength(), 0);
  env->GetByteArrayRegion(arr, 0, vec.size(), reinterpret_cast<jbyte*>(vec.data));
  simu->setBigInt(handle_, vec);
}

JNIEXPORT void API JNICALL ${jniPrefix}eval_1${uniqueId}
  (JNIEnv*, jobject obj, HdlSimulation* simu){
    (void) obj;
    (void) env;
    simu->eval();
}

JNIEXPORT void API JNICALL ${jniPrefix}sleep_1${uniqueId}
  (JNIEnv*, jobject obj, HdlSimulation* simu, uint64_t cycles){
  (void) obj;
  (void) env;
  simu->sleep(cycles);
}

JNIEXPORT void API JNICALL ${jniPrefix}end_1${uniqueId}
  (JNIEnv*, jobject obj, HdlSimulation* simu){
  (void) obj;
  (void) env;
  simu->end();
  delete simu;
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

  class Logger extends ProcessLogger {override def err(s: => String): Unit = {if(!s.startsWith("ar: creating ")) println(s)}
    override def out(s: => String): Unit = {}
    override def buffer[T](f: => T) = f
  }


  def compileGhdl(): Unit = {
    val jdk = System.getProperty("java.home").replace("/jre","").replace("\\jre","")
    val jdkIncludes = if(isWindows){
      new File(s"${workspacePath}\\${workspaceName}").mkdirs()
      FileUtils.copyDirectory(new File(s"$jdk\\include"), new File(s"${workspacePath}\\${workspaceName}\\jniIncludes"))
      s"jniIncludes"
    }else{
      jdk + "/include"
    }

    val flags   = if(isMac) List("-dynamiclib") else List("-fPIC", "-m64", "-shared", "-Wno-attributes")

    val waveArgs = config.waveFormat match {
      case WaveFormat.NONE => ""
    }

    val fileList = config.rtlSourcesPaths
      .map(new File(_).getAbsolutePath)
      .map('"' + _.replace("\\","/") + '"')
      .mkString(" ")

    val cflags = "-fPIC -std=c++11 -pedantic -Wall -Wextra -O2"
    val lflags = "-lboost_fiber -lboost_context -lpthread -no-pie"

    val GhdlScript =
      s"""
         |ghdl -a ${fileList}
         |ghdl --bind ${config.toplevelName}
         |g++ -c ghdlIface.cpp -o ghdlIface.o ${cflags}
         |g++ -c ${wrapperCppName} -o ${wrapperCppName}.o ${cflags}
         |g++ ghdlIface.o ${wrapperCppName}.o -Wl,`ghdl --list-link ${config.toplevelName}` ${cflags} ${ldflags}
         |""".stripMargin

    var lastTime = System.currentTimeMillis()
    
    def bench(msg : String): Unit ={
      val newTime = System.currentTimeMillis()
      val sec = (newTime-lastTime)*1e-3
      println(msg + " " + sec)
      lastTime = newTime
    }
    
    val GhdlScriptFile = new PrintWriter(new File(workspacePath + "/GhdlScript.sh"))
    GhdlScriptFile.write(GhdlScript)
    GhdlScriptFile.close

    val shCommand = if(isWindows) "sh.exe" else "sh"
    assert(Process(Seq(shCommand, "GhdlScript.sh"), 
                   new File(workspacePath)).! (new Logger()) == 0, "Ghdl invocation failed")
    
    genWrapperCpp()

    val threadCount = if(isWindows || isMac) Runtime.getRuntime().availableProcessors() else VanillaCpuLayout.fromCpuInfo().cpus()
    assert(s"make -j$threadCount VM_PARALLEL_BUILDS=1 -C ${workspacePath}/${workspaceName} -f V${config.toplevelName}.mk V${config.toplevelName} CURDIR=${workspacePath}/${workspaceName}".!  (new Logger()) == 0, "Ghdl C++ model compilation failed")

    FileUtils.copyFile(new File(s"${workspacePath}/${workspaceName}/V${config.toplevelName}${if(isWindows) ".exe" else ""}") , new File(s"${workspacePath}/${workspaceName}/${workspaceName}_$uniqueId.${if(isWindows) "dll" else (if(isMac) "dylib" else "so")}"))
  }

  def compileJava(): Unit = {
    val GhdlNativeImplCode =
      s"""package wrapper_${workspaceName};
         |import spinal.sim.IGhdlNative;
         |
         |public class GhdlNative implements IGhdlNative {
         |    public long newHandle(String name, int seed) { return newHandle_${uniqueId}(name, seed);}
         |    public boolean eval(long handle) { return eval_${uniqueId}(handle);}
         |    public void sleep(long handle, long cycles) { sleep_${uniqueId}(handle, cycles);}
         |    public long getU64(long handle, int id) { return getU64_${uniqueId}(handle, id);}
         |    public void setU64(long handle, int id, long value) { setU64_${uniqueId}(handle, id, value);}
         |    public void getAU8(long handle, int id, byte[] value) { getAU8_${uniqueId}(handle, id, value);}
         |    public void setAU8(long handle, int id, byte[] value, int length) { setAU8_${uniqueId}(handle, id, value, length);}
         |    public void deleteHandle(long handle) { deleteHandle_${uniqueId}(handle);}
         |    public void enableWave(long handle) { enableWave_${uniqueId}(handle);}
         |    public void disableWave(long handle) { disableWave_${uniqueId}(handle);}
         |
         |
         |    public native long newHandle_${uniqueId}(String name, int seed);
         |    public native boolean eval_${uniqueId}(long handle);
         |    public native void sleep_${uniqueId}(long handle, long cycles);
         |    public native long getU64_${uniqueId}(long handle, int id);
         |    public native void setU64_${uniqueId}(long handle, int id, long value);
         |    public native void getAU8_${uniqueId}(long handle, int id, byte[] value);
         |    public native void setAU8_${uniqueId}(long handle, int id, byte[] value, int length);
         |    public native void deleteHandle_${uniqueId}(long handle);
         |    public native void enableWave_${uniqueId}(long handle);
         |    public native void disableWave_${uniqueId}(long handle);
         |
         |    static{
         |      System.load("${new File(s"${workspacePath}/${workspaceName}").getAbsolutePath.replace("\\","\\\\")}/${workspaceName}_$uniqueId.${if(isWindows) "dll" else (if(isMac) "dylib" else "so")}");
         |    }
         |}
       """.stripMargin

    val GhdlNativeImplFile = new DynamicCompiler.InMemoryJavaFileObject(s"wrapper_${workspaceName}.GhdlNative", GhdlNativeImplCode)
    import collection.JavaConverters._
    DynamicCompiler.compile(List[JavaFileObject](GhdlNativeImplFile).asJava, s"${workspacePath}/${workspaceName}")
  }

  def checks(): Unit ={
    if(System.getProperty("java.class.path").contains("sbt-launch.jar")){
      System.err.println("""[Error] It look like you are running the simulation with SBT without having the SBT 'fork := true' configuration.\n  Add it in the build.sbt file to fix this issue, see https://github.com/SpinalHDL/SpinalTemplateSbt/blob/master/build.sbt""")
      throw new Exception()
    }
  }

//  clean()
  checks()
  compileGhdl()
  compileJava()

  val nativeImpl = DynamicCompiler.getClass(s"wrapper_${workspaceName}.GhdlNative", s"${workspacePath}/${workspaceName}")
  val nativeInstance: IGhdlNative = nativeImpl.newInstance().asInstanceOf[IGhdlNative]

  def instanciate(name: String, seed: Int) = nativeInstance.newHandle(name, seed)
}

