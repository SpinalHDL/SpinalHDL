/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core.internals

import spinal.core._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class PhaseVhdl(pc: PhaseContext, report: SpinalReport[_]) extends PhaseMisc with VhdlBase {
  import pc._

  var outFile: java.io.FileWriter = null

  override def impl(pc: PhaseContext): Unit = {
    packageName     = pc.privateNamespaceName + packageName
    enumPackageName = pc.privateNamespaceName + enumPackageName
    report.toplevelName = pc.topLevel.definitionName
    var targetFilePath = ""

    if (pc.config.oneFilePerComponent) {
      val fileList = new java.io.FileWriter(pc.config.targetDirectory + "/" + topLevel.definitionName + ".lst")

      // Emit enums
      targetFilePath = pc.config.targetDirectory + "/" + "pkg_enum.vhd"
      outFile = new java.io.FileWriter(targetFilePath)
      outFile.write(VhdlVerilogBase.getHeader("--", pc.config.rtlHeader, topLevel, config.headerWithDate, config.headerWithRepoHash))
      emitEnumPackage(outFile)
      outFile.flush()
      outFile.close()
      report.generatedSourcesPaths += targetFilePath
      fileList.write(targetFilePath + "\n")

      // Emit utility functions
      if (pc.config.genVhdlPkg) {
        targetFilePath = pc.config.targetDirectory + "/" + "pkg_scala2hdl.vhd"
        outFile = new java.io.FileWriter(targetFilePath)
        outFile.write(VhdlVerilogBase.getHeader("--", pc.config.rtlHeader, topLevel, config.headerWithDate, config.headerWithRepoHash))
        emitPackage(outFile)
        outFile.flush()
        outFile.close()
        report.generatedSourcesPaths += targetFilePath
        fileList.write(targetFilePath + "\n")
      }

      // Emit each component
      for (c <- sortedComponents) {
        val componentContent = compile(c)

        if (!componentContent.contains("replaced by")) {
          targetFilePath = pc.config.targetDirectory + "/" + (c.definitionName + ".vhd")

          if (!c.isInBlackBoxTree) {
            outFile = new java.io.FileWriter(targetFilePath)
            outFile.write(VhdlVerilogBase.getHeader("--", pc.config.rtlHeader, topLevel, config.headerWithDate, config.headerWithRepoHash))
            outFile.write(componentContent)
            outFile.flush()
            outFile.close()
            report.generatedSourcesPaths += targetFilePath
            fileList.write(targetFilePath + "\n")
          }
        }
      }
      fileList.flush()
      fileList.close()
    } else {
      // All in one
      targetFilePath = pc.config.targetDirectory + "/" +  (if(pc.config.netlistFileName == null)(topLevel.definitionName + ".vhd") else pc.config.netlistFileName)
      report.generatedSourcesPaths += targetFilePath
      report.toplevelName = pc.topLevel.definitionName
      outFile = new java.io.FileWriter(targetFilePath)
      outFile.write(VhdlVerilogBase.getHeader("--", pc.config.rtlHeader, topLevel, config.headerWithDate, config.headerWithRepoHash))
      emitEnumPackage(outFile)

      if(pc.config.genVhdlPkg)
        emitPackage(outFile)

      for (c <- sortedComponents) {
        if (!c.isInBlackBoxTree) {
          outFile.write(compile(c))
          outFile.flush()
        }
      }

      outFile.flush()
      outFile.close()
    }
  }

  val allocateAlgoIncrementaleBase = globalData.allocateAlgoIncrementale()

  def compile(component: Component): String = {
    val componentBuilderVhdl = new ComponentEmitterVhdl(
      c                         = component,
      vhdlBase                  = this,
      algoIdIncrementalBase     = allocateAlgoIncrementaleBase,
      mergeAsyncProcess         = config.mergeAsyncProcess,
      asyncResetCombSensitivity = config.asyncResetCombSensitivity,
      anonymSignalPrefix        = if(pc.config.anonymSignalUniqueness) globalData.anonymSignalPrefix + "_" + component.definitionName else globalData.anonymSignalPrefix,
      emitedComponentRef        = emitedComponentRef,
      pc                        = pc
    )

    val trace = componentBuilderVhdl.getTrace()
    val oldComponent = emitedComponent.getOrElse(trace, null)

    val text = if (oldComponent == null) {
      emitedComponent += (trace -> component)
      componentBuilderVhdl.result
    } else {
      emitedComponentRef.put(component, oldComponent)
      val str =  s"\n--${component.definitionName} replaced by ${oldComponent.definitionName}\n\n"
      component.definitionName = oldComponent.definitionName
      str
    }

    text
  }

  val emitedComponent    = mutable.Map[ComponentEmitterTrace, Component]()
  val emitedComponentRef = new java.util.concurrent.ConcurrentHashMap[Component, Component]()


  def emitEnumPackage(out: java.io.FileWriter): Unit = {
    val ret = new StringBuilder()
    ret ++=
      s"""library IEEE;
         |use IEEE.STD_LOGIC_1164.ALL;
         |use IEEE.NUMERIC_STD.all;
         |
         |package $enumPackageName is
                                    |""".stripMargin

    for (enumDef <- enums.keys) {
      ret ++= s"  type ${enumDef.getName()} is (${enumDef.elements.map(_.getName()).mkString(",")});\n"
      //ret ++= s"  type ${getEnumDebugType(enumDef)} is (${enumDef.elements.foldLeft("XXX")((str, e) => str + "," + e.getName())});\n"
    }

    ret ++= "\n"

    for ((enumDef, encodings) <- enums) {
      val enumName = enumDef.getName()
      ret ++= s"  function pkg_mux (sel : std_logic; one : $enumName; zero : $enumName) return $enumName;\n"


      for (encoding <- encodings if !encoding.isNative) {
        val encodingName = encoding.getName()
        val bitCount     = encoding.getWidth(enumDef)
        val vhdlEnumType = emitEnumType(enumDef, encoding)

        ret ++= s"  subtype $vhdlEnumType is std_logic_vector(${bitCount - 1} downto 0);\n"

        for (element <- enumDef.elements) {
          ret ++= s"  constant ${emitEnumLiteral(element, encoding)} : $vhdlEnumType := ${idToBits(element, encoding)};\n"
        }

        ret ++= "\n"
        //ret ++= s"  function pkg_to${enumName}_debug (value : std_logic_vector) return $enumName;\n"
      }

      for (encoding <- encodings) {
        if (!encoding.isNative){}
         // ret ++= s"  function ${getEnumToDebugFuntion(enumDef, encoding)} (value : ${emitEnumType(enumDef, encoding)}) return ${getEnumDebugType(enumDef)};\n"
        else {
          ret ++= s"  function pkg_toStdLogicVector_${encoding.getName()} (value : $enumName) return std_logic_vector;\n"
          ret ++= s"  function pkg_to${enumName}_${encoding.getName()} (value : std_logic_vector(${encoding.getWidth(enumDef) - 1} downto 0)) return $enumName;\n"
        }

        for (targetEncoding <- encodings if targetEncoding != encoding && !(targetEncoding.isNative && encoding.isNative)) {
          ret ++= s"  function ${getReEncodingFuntion(enumDef, encoding, targetEncoding)} (that : ${emitEnumType(enumDef, encoding)}) return ${emitEnumType(enumDef, targetEncoding)};\n"
        }
      }
    }

    def idToBits[T <: SpinalEnum](enum: SpinalEnumElement[T], encoding: SpinalEnumEncoding): String = {
      val str = encoding.getValue(enum).toString(2)
      "\"" + ("0" * (encoding.getWidth(enum.spinalEnum) - str.length)) + str + "\""
    }

    ret ++= s"end $enumPackageName;\n\n"

    if (enums.nonEmpty) {
      ret ++= s"package body $enumPackageName is\n"
      for ((enumDef, encodings) <- enums) {
        val enumName = enumDef.getName()
        ret ++= s"  function pkg_mux (sel : std_logic; one : $enumName; zero : $enumName) return $enumName is\n"
        ret ++= "  begin\n"
        ret ++= "    if sel = '1' then\n"
        ret ++= "      return one;\n"
        ret ++= "    else\n"
        ret ++= "      return zero;\n"
        ret ++= "    end if;\n"
        ret ++= "  end pkg_mux;\n\n"


        for (encoding <- encodings) {
          if (encoding.isNative){
            ret ++=
              s"""  function pkg_to${enumName}_${encoding.getName()} (value : std_logic_vector(${encoding.getWidth(enumDef) - 1} downto 0)) return $enumName is
                                                                                                                                                              |  begin
                                                                                                                                                              |    case value is
                                                                                                                                                              |${
                {
                  for (e <- enumDef.elements) yield s"      when ${idToBits(e, encoding)} => return ${e.getName()};"
                }.mkString("\n")
              }
                  |      when others => return ${enumDef.elements.head.getName()};
                                                                                 |    end case;
                                                                                 |  end;
                                                                                 |""".stripMargin

            ret ++=
              s"""  function pkg_toStdLogicVector_${encoding.getName()} (value : $enumName) return std_logic_vector is
                                                                                            |  begin
                                                                                            |    case value is
                                                                                            |${
                {
                  for (e <- enumDef.elements) yield s"      when ${e.getName()} => return ${idToBits(e, encoding)};"
                }.mkString("\n")
              }
                  |      when others => return ${idToBits(enumDef.elements.head, encoding)};
                                                                                           |    end case;
                                                                                           |  end;
                                                                                           |""".stripMargin
          }


          for (targetEncoding <- encodings if targetEncoding != encoding) {
            ret ++= s"  function ${getReEncodingFuntion(enumDef, encoding, targetEncoding)} (that : ${emitEnumType(enumDef, encoding)}) return ${emitEnumType(enumDef, targetEncoding)} is\n"
            ret ++= "  begin\n"
            ret ++= "    case that is \n"
            for (e <- enumDef.elements) {
              ret ++= s"      when ${emitEnumLiteral(e, encoding)} => return ${emitEnumLiteral(e, targetEncoding)};\n"
            }
            ret ++= s"      when others => return ${emitEnumLiteral(enumDef.elements.head, targetEncoding)};\n"
            ret ++= "    end case;\n"
            ret ++= "  end;\n\n"
          }
        }
      }
      ret ++= s"end $enumPackageName;\n\n\n"
    }
    out.write(ret.result())
  }

  def emitPackage(out: java.io.FileWriter): Unit = {

    def pkgExtractBool(kind: String): (String, String) = {
      val ret = new StringBuilder()
      (s"function pkg_extract (that : $kind; bitId : integer) return std_logic", {
        ret ++= "  begin\n"
        ret ++= "    return that(bitId);\n"
        ret ++= "  end pkg_extract;\n\n"
        ret.result()
      })
    }

    def pkgExtract(kind: String): (String, String) = {
      val ret = new StringBuilder()

      (s"function pkg_extract (that : $kind; base : unsigned; size : integer) return $kind", {
        ret ++= "   constant elementCount : integer := (that'length-size)+1;\n"
        ret ++= s"   type tableType is array (0 to elementCount-1) of $kind(size-1 downto 0);\n"
        ret ++= "   variable table : tableType;\n"
        ret ++= "  begin\n"
        ret ++= "    for i in 0 to elementCount-1 loop\n"
        ret ++= "      table(i) := that(i + size - 1 downto i);\n"
        ret ++= "    end loop;\n"
        ret ++= "    return table(to_integer(base));\n"
        ret ++= "  end pkg_extract;\n\n"
        ret.result()
      })
    }


    def pkgCat(kind: String): (String, String) = {
      val ret = new StringBuilder()

      (s"function pkg_cat (a : $kind; b : $kind) return $kind", {
        ret ++= s"    variable cat : $kind(a'length + b'length-1 downto 0);\n"
        ret ++= s"  begin\n"
        ret ++= s"    cat := a & b;\n"
        ret ++= s"    return cat;\n"
        ret ++= s"  end pkg_cat;\n\n"
        ret.result()
      })
    }

    def pkgNot(kind: String): (String, String) = {
      val ret = new StringBuilder()

      (s"function pkg_not (value : $kind) return $kind", {
        ret ++= s"    variable ret : $kind(value'length-1 downto 0);\n"
        ret ++= s"  begin\n"
        ret ++= s"    ret := not value;\n"
        ret ++= s"    return ret;\n"
        ret ++= s"  end pkg_not;\n\n"
        ret.result()
      })
    }

    val vectorTypes = "std_logic_vector" :: "unsigned" :: "signed" :: Nil

    val funcs = ArrayBuffer[(String, String)]()

    vectorTypes.foreach(kind => {
      funcs += pkgExtractBool(kind)
      funcs += pkgExtract(kind)
      funcs += pkgCat(kind)
      funcs += pkgNot(kind)
    })

    val ret = new StringBuilder()
    ret ++= "library IEEE;\n"
    ret ++= "use ieee.std_logic_1164.all;\n"
    ret ++= "use ieee.numeric_std.all;\n"
    ret ++= "use ieee.math_real.all;\n"
    ret ++= "\n"
    ret ++= s"package $packageName is\n"
    ret ++= funcs.map("  " + _._1 + ";\n").mkString
    ret ++= "\n"
    ret ++= "  function pkg_mux (sel : std_logic; one : std_logic; zero : std_logic) return std_logic;\n"
    ret ++= "  function pkg_mux (sel : std_logic; one : std_logic_vector; zero : std_logic_vector) return std_logic_vector;\n"
    ret ++= "  function pkg_mux (sel : std_logic; one : unsigned; zero : unsigned) return unsigned;\n"
    ret ++= "  function pkg_mux (sel : std_logic; one : signed; zero : signed) return signed;\n"
    ret ++= "\n"
    ret ++= "  function pkg_toStdLogic (value : boolean) return std_logic;\n"
    ret ++= "  function pkg_toStdLogicVector (value : std_logic) return std_logic_vector;\n"
    ret ++= "  function pkg_toUnsigned (value : std_logic) return unsigned;\n"
    ret ++= "  function pkg_toSigned (value : std_logic) return signed;\n"
    ret ++= "  function pkg_stdLogicVector (lit : std_logic_vector) return std_logic_vector;\n"
    ret ++= "  function pkg_unsigned (lit : unsigned) return unsigned;\n"
    ret ++= "  function pkg_signed (lit : signed) return signed;\n"
    ret ++= "\n"
    ret ++= "  function pkg_resize (that : std_logic_vector; width : integer) return std_logic_vector;\n"
    ret ++= "  function pkg_resize (that : unsigned; width : integer) return unsigned;\n"
    ret ++= "  function pkg_resize (that : signed; width : integer) return signed;\n"
    ret ++= "\n"
    ret ++= "  function pkg_extract (that : std_logic_vector; high : integer; low : integer) return std_logic_vector;\n"
    ret ++= "  function pkg_extract (that : unsigned; high : integer; low : integer) return unsigned;\n"
    ret ++= "  function pkg_extract (that : signed; high : integer; low : integer) return signed;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftRight (that : std_logic_vector; size : natural) return std_logic_vector;\n"
    ret ++= "  function pkg_shiftRight (that : std_logic_vector; size : unsigned) return std_logic_vector;\n"
    ret ++= "  function pkg_shiftLeft (that : std_logic_vector; size : natural) return std_logic_vector;\n"
    ret ++= "  function pkg_shiftLeft (that : std_logic_vector; size : unsigned) return std_logic_vector;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftRight (that : unsigned; size : natural) return unsigned;\n"
    ret ++= "  function pkg_shiftRight (that : unsigned; size : unsigned) return unsigned;\n"
    ret ++= "  function pkg_shiftLeft (that : unsigned; size : natural) return unsigned;\n"
    ret ++= "  function pkg_shiftLeft (that : unsigned; size : unsigned) return unsigned;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftRight (that : signed; size : natural) return signed;\n"
    ret ++= "  function pkg_shiftRight (that : signed; size : unsigned) return signed;\n"
    ret ++= "  function pkg_shiftLeft (that : signed; size : natural) return signed;\n"
    ret ++= "  function pkg_shiftLeft (that : signed; size : unsigned; w : integer) return signed;\n"
    ret ++= "\n"
    ret ++= "  function pkg_rotateLeft (that : std_logic_vector; size : unsigned) return std_logic_vector;\n"
    ret ++= s"end  $packageName;\n"
    ret ++= "\n"
    ret ++= s"package body $packageName is\n"
    ret ++= funcs.map(f => "  " + f._1 + " is\n" + f._2).mkString
    ret ++= "\n"
    ret ++= "  -- unsigned shifts\n"
    ret ++= "  function pkg_shiftRight (that : unsigned; size : natural) return unsigned is\n"
    ret ++= "  begin\n"
    ret ++= "    if size >= that'length then\n"
    ret ++= "      return \"\";\n"
    ret ++= "    else\n"
    ret ++= "      return shift_right(that,size)(that'length-1-size downto 0);\n"
    ret ++= "    end if;\n"
    ret ++= "  end pkg_shiftRight;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftRight (that : unsigned; size : unsigned) return unsigned is\n"
    ret ++= "  begin\n"
    ret ++= "    return shift_right(that,to_integer(size));\n"
    ret ++= "  end pkg_shiftRight;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftLeft (that : unsigned; size : natural) return unsigned is\n"
    ret ++= "  begin\n"
    ret ++= "    return shift_left(resize(that,that'length + size),size);\n"
    ret ++= "  end pkg_shiftLeft;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftLeft (that : unsigned; size : unsigned) return unsigned is\n"
    ret ++= "  begin\n"
    ret ++= "    return shift_left(resize(that,that'length + 2**size'length - 1),to_integer(size));\n"
    ret ++= "  end pkg_shiftLeft;\n"
    ret ++= "\n"
    ret ++= "  -- std_logic_vector shifts\n"
    ret ++= "  function pkg_shiftRight (that : std_logic_vector; size : natural) return std_logic_vector is\n"
    ret ++= "  begin\n"
    ret ++= "    return std_logic_vector(pkg_shiftRight(unsigned(that),size));\n"
    ret ++= "  end pkg_shiftRight;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftRight (that : std_logic_vector; size : unsigned) return std_logic_vector is\n"
    ret ++= "  begin\n"
    ret ++= "    return std_logic_vector(pkg_shiftRight(unsigned(that),size));\n"
    ret ++= "  end pkg_shiftRight;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftLeft (that : std_logic_vector; size : natural) return std_logic_vector is\n"
    ret ++= "  begin\n"
    ret ++= "    return std_logic_vector(pkg_shiftLeft(unsigned(that),size));\n"
    ret ++= "  end pkg_shiftLeft;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftLeft (that : std_logic_vector; size : unsigned) return std_logic_vector is\n"
    ret ++= "  begin\n"
    ret ++= "    return std_logic_vector(pkg_shiftLeft(unsigned(that),size));\n"
    ret ++= "  end pkg_shiftLeft;\n"
    ret ++= "\n"
    ret ++= "  -- signed shifts\n"
    ret ++= "  function pkg_shiftRight (that : signed; size : natural) return signed is\n"
    ret ++= "  begin\n"
    ret ++= "    return signed(pkg_shiftRight(unsigned(that),size));\n"
    ret ++= "  end pkg_shiftRight;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftRight (that : signed; size : unsigned) return signed is\n"
    ret ++= "  begin\n"
    ret ++= "    return shift_right(that,to_integer(size));\n"
    ret ++= "  end pkg_shiftRight;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftLeft (that : signed; size : natural) return signed is\n"
    ret ++= "  begin\n"
    ret ++= "    return signed(pkg_shiftLeft(unsigned(that),size));\n"
    ret ++= "  end pkg_shiftLeft;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftLeft (that : signed; size : unsigned; w : integer) return signed is\n"
    ret ++= "  begin\n"
    ret ++= "    return shift_left(resize(that,w),to_integer(size));\n"
    ret ++= "  end pkg_shiftLeft;\n"
    ret ++= "\n"
    ret ++= "  function pkg_rotateLeft (that : std_logic_vector; size : unsigned) return std_logic_vector is\n"
    ret ++= "  begin\n"
    ret ++= "    return std_logic_vector(rotate_left(unsigned(that),to_integer(size)));\n"
    ret ++= "  end pkg_rotateLeft;\n"
    ret ++= "\n"
    ret ++= "  function pkg_extract (that : std_logic_vector; high : integer; low : integer) return std_logic_vector is\n"
    ret ++= "    variable temp : std_logic_vector(high-low downto 0);\n"
    ret ++= "  begin\n"
    ret ++= "    temp := that(high downto low);\n"
    ret ++= "    return temp;\n"
    ret ++= "  end pkg_extract;\n"
    ret ++= "\n"
    ret ++= "  function pkg_extract (that : unsigned; high : integer; low : integer) return unsigned is\n"
    ret ++= "    variable temp : unsigned(high-low downto 0);\n"
    ret ++= "  begin\n"
    ret ++= "    temp := that(high downto low);\n"
    ret ++= "    return temp;\n"
    ret ++= "  end pkg_extract;\n"
    ret ++= "\n"
    ret ++= "  function pkg_extract (that : signed; high : integer; low : integer) return signed is\n"
    ret ++= "    variable temp : signed(high-low downto 0);\n"
    ret ++= "  begin\n"
    ret ++= "    temp := that(high downto low);\n"
    ret ++= "    return temp;\n"
    ret ++= "  end pkg_extract;\n"
    ret ++= "\n"
    ret ++= "  function pkg_mux (sel : std_logic; one : std_logic; zero : std_logic) return std_logic is\n"
    ret ++= "  begin\n"
    ret ++= "    if sel = '1' then\n"
    ret ++= "      return one;\n"
    ret ++= "    else\n"
    ret ++= "      return zero;\n"
    ret ++= "    end if;\n"
    ret ++= "  end pkg_mux;\n"
    ret ++= "\n"
    ret ++= "  function pkg_mux (sel : std_logic; one : std_logic_vector; zero : std_logic_vector) return std_logic_vector is\n"
    ret ++= "    variable ret : std_logic_vector(zero'range);\n"
    ret ++= "  begin\n"
    ret ++= "    if sel = '1' then\n"
    ret ++= "      ret := one;\n"
    ret ++= "    else\n"
    ret ++= "      ret := zero;\n"
    ret ++= "    end if;\n"
    ret ++= "    return ret;\n"
    ret ++= "  end pkg_mux;\n"
    ret ++= "\n"
    ret ++= "  function pkg_mux (sel : std_logic; one : unsigned; zero : unsigned) return unsigned is\n"
    ret ++= "    variable ret : unsigned(zero'range);\n"
    ret ++= "  begin\n"
    ret ++= "    if sel = '1' then\n"
    ret ++= "      ret := one;\n"
    ret ++= "    else\n"
    ret ++= "      ret := zero;\n"
    ret ++= "    end if;\n"
    ret ++= "    return ret;\n"
    ret ++= "  end pkg_mux;\n"
    ret ++= "\n"
    ret ++= "  function pkg_mux (sel : std_logic; one : signed; zero : signed) return signed is\n"
    ret ++= "    variable ret : signed(zero'range);\n"
    ret ++= "  begin\n"
    ret ++= "    if sel = '1' then\n"
    ret ++= "      ret := one;\n"
    ret ++= "    else\n"
    ret ++= "      ret := zero;\n"
    ret ++= "    end if;\n"
    ret ++= "    return ret;\n"
    ret ++= "  end pkg_mux;\n"
    ret ++= "\n"
    ret ++= "  function pkg_toStdLogic (value : boolean) return std_logic is\n"
    ret ++= "  begin\n"
    ret ++= "    if value = true then\n"
    ret ++= "      return '1';\n"
    ret ++= "    else\n"
    ret ++= "      return '0';\n"
    ret ++= "    end if;\n"
    ret ++= "  end pkg_toStdLogic;\n"
    ret ++= "\n"
    ret ++= "  function pkg_toStdLogicVector (value : std_logic) return std_logic_vector is\n"
    ret ++= "    variable ret : std_logic_vector(0 downto 0);\n"
    ret ++= "  begin\n"
    ret ++= "    ret(0) := value;\n"
    ret ++= "    return ret;\n"
    ret ++= "  end pkg_toStdLogicVector;\n"
    ret ++= "\n"
    ret ++= "  function pkg_toUnsigned (value : std_logic) return unsigned is\n"
    ret ++= "    variable ret : unsigned(0 downto 0);\n"
    ret ++= "  begin\n"
    ret ++= "    ret(0) := value;\n"
    ret ++= "    return ret;\n"
    ret ++= "  end pkg_toUnsigned;\n"
    ret ++= "\n"
    ret ++= "  function pkg_toSigned (value : std_logic) return signed is\n"
    ret ++= "    variable ret : signed(0 downto 0);\n"
    ret ++= "  begin\n"
    ret ++= "    ret(0) := value;\n"
    ret ++= "    return ret;\n"
    ret ++= "  end pkg_toSigned;\n"
    ret ++= "\n"
    ret ++= "  function pkg_stdLogicVector (lit : std_logic_vector) return std_logic_vector is\n"
    ret ++= "    variable ret : std_logic_vector(lit'length-1 downto 0);\n"
    ret ++= "  begin\n"
    ret ++= "    ret := lit;\n"
    ret ++= "    return ret;\n"
    ret ++= "  end pkg_stdLogicVector;\n"
    ret ++= "\n"
    ret ++= "  function pkg_unsigned (lit : unsigned) return unsigned is\n"
    ret ++= "    variable ret : unsigned(lit'length-1 downto 0);\n"
    ret ++= "  begin\n"
    ret ++= "    ret := lit;\n"
    ret ++= "    return ret;\n"
    ret ++= "  end pkg_unsigned;\n"
    ret ++= "\n"
    ret ++= "  function pkg_signed (lit : signed) return signed is\n"
    ret ++= "    variable ret : signed(lit'length-1 downto 0);\n"
    ret ++= "  begin\n"
    ret ++= "    ret := lit;\n"
    ret ++= "    return ret;\n"
    ret ++= "  end pkg_signed;\n"
    ret ++= "\n"
    ret ++= "  function pkg_resize (that : std_logic_vector; width : integer) return std_logic_vector is\n"
    ret ++= "  begin\n"
    ret ++= "    return std_logic_vector(resize(unsigned(that),width));\n"
    ret ++= "  end pkg_resize;\n"
    ret ++= "\n"
    ret ++=
     """|  function pkg_resize (that : unsigned; width : integer) return unsigned is
        |    variable ret : unsigned(width-1 downto 0);
        |  begin
        |    if that'length = 0 then
        |       ret := (others => '0');
        |    else
        |       ret := resize(that,width);
        |    end if;
        |    return ret;
        |  end pkg_resize;
        |""".stripMargin


    ret ++=
     """|  function pkg_resize (that : signed; width : integer) return signed is
        |    variable ret : signed(width-1 downto 0);
        |  begin
        |    if that'length = 0 then
        |       ret := (others => '0');
        |    elsif that'length >= width then
        |       ret := that(width-1 downto 0);
        |    else
        |       ret := resize(that,width);
        |    end if;
        |    return ret;
        |  end pkg_resize;
        |""".stripMargin
    ret ++= s"end $packageName;\n"
    ret ++= "\n"
    ret ++= "\n"

    out.write(ret.result())
  }
}
