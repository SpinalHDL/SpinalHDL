/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal


import scala.collection.mutable
import scala.collection.mutable.{StringBuilder, ArrayBuffer}


/**
 * Created by PIC18F on 07.01.2015.
 */
class VhdlBackend extends Backend with VhdlBase {
  var out: java.io.FileWriter = null
  var library = "work"
  var enumPackageName = "pkg_enum"
  var packageName = "pkg_scala2hdl"
  var outputFile: String = null

  val emitedComponent = mutable.Map[ComponentBuilder, ComponentBuilder]()
  val emitedComponentRef = mutable.Map[Component, Component]()

  reservedKeyWords ++= vhdlKeyWords


  override protected def elaborate[T <: Component](topLevel: T): BackendReport[T] = {
    val report = super.elaborate(topLevel)
    SpinalInfoPhase("Write VHDL")

    if (outputFile == null) outputFile = topLevel.definitionName + ".vhd"

    out = new java.io.FileWriter(outputFile)
    emitEnumPackage(out)
    emitPackage(out)

    for (c <- sortedComponents) {
      SpinalInfoPhase(s"${"  " * (1 + c.level)}emit ${c.definitionName}")
      compile(c)
    }

    out.flush();
    out.close();

    //  emitTestBench(topLevel :: Nil,topLevel.definitionName + "_tb")

    report
  }


  def compile(component: Component): Unit = {
    val text = emit(component)
    out.write(text)
  }

  class ComponentBuilder(val component: Component) {
    val parts = ArrayBuffer[(StringBuilder, Boolean)]()

    def newPart(mustMatch: Boolean): StringBuilder = {
      val builder = new mutable.StringBuilder
      parts += (builder -> mustMatch)
      builder
    }

    def result: String = {
      val ret = new mutable.StringBuilder
      parts.foreach(ret ++= _._1)
      ret.result()
    }
    var hash: Integer = null
    override def hashCode(): Int = {
      if (hash == null) {
        hash = parts.filter(_._2).foldLeft(0)(_ + _._1.result().hashCode())
      }
      hash
    }
    override def equals(obj: scala.Any): Boolean = {
      if (this.hashCode() != obj.hashCode()) return false //Colision into hashmap implementation don't check it XD
      obj match {
        case cb: ComponentBuilder => {
          for ((a, b) <- (parts, cb.parts).zipped) {
            if (a._2 || b._2) {
              if (a._1.result() != b._1.result()) {
                return false
              }
            }
          }
          return true;
        }
        case _ => return ???
      }
    }
  }

  def emit(component: Component): String = {
    val ret = new StringBuilder()
    val builder = new ComponentBuilder(component)

    emitLibrary(builder)
    emitEntity(component, builder)
    emitArchitecture(component, builder)

    val oldBuilder = emitedComponent.getOrElse(builder, null)
    if (oldBuilder == null) {
      emitedComponent += (builder -> builder)
      return builder.result
    } else {
      emitedComponentRef += (component -> oldBuilder.component)
      return s"\n--${component.definitionName} remplaced by ${oldBuilder.component.definitionName}\n\n"
    }
  }


  def emitEnumPackage(out: java.io.FileWriter): Unit = {
    val ret = new StringBuilder();
    ret ++= s"""library IEEE;
               |use IEEE.STD_LOGIC_1164.ALL;
               |use IEEE.NUMERIC_STD.all;
               |
               |package $enumPackageName is
                                          |""".stripMargin
    for (enumDef <- enums) {
      ret ++= s"  type ${enumDef.getName()} is (${enumDef.values.toList.sortWith(_.id < _.id).map(_.getName()).reduceLeft(_ + "," + _)});\n"
    }

    for (enumDef <- enums) {
      val enumName = enumDef.getName()
      ret ++= s"  function pkg_mux (sel : std_logic;one : $enumName;zero : $enumName) return $enumName;\n"
      ret ++= s"  function pkg_toStdLogicVector (value : $enumName) return std_logic_vector;\n"
      ret ++= s"  function pkg_to$enumName (value : std_logic_vector) return $enumName;\n"
    }

    ret ++= s"end $enumPackageName;\n\n"
    if (enums.size != 0) {
      ret ++= s"package body $enumPackageName is\n"
      for (enumDef <- enums) {
        val enumName = enumDef.getName()
        ret ++= s"  function pkg_mux (sel : std_logic;one : $enumName;zero : $enumName) return $enumName is\n"
        ret ++= "  begin\n"
        ret ++= "    if sel = '1' then\n"
        ret ++= "      return one;\n"
        ret ++= "    else\n"
        ret ++= "      return zero;\n"
        ret ++= "    end if;\n"
        ret ++= "  end pkg_mux;\n\n"


        ret ++= s"  function pkg_toStdLogicVector (value : $enumName) return std_logic_vector is\n"
        ret ++= "  begin\n"
        ret ++= "    case value is \n"
        for (e <- enumDef.values) {
          ret ++= s"      when ${e.getName()} => return ${idToBits(e)};\n"
        }
        ret ++= s"      when others => return ${idToBits(enumDef.values.head)};\n"
        ret ++= "    end case;\n"
        ret ++= "  end pkg_toStdLogicVector;\n\n"

        ret ++= s"  function pkg_to$enumName (value : std_logic_vector) return $enumName is\n"
        ret ++= "  begin\n"
        ret ++= "    case to_integer(unsigned(value)) is \n"
        for (e <- enumDef.values) {
          ret ++= s"      when ${e.id} => return ${e.getName()};\n"
        }
        ret ++= s"      when others => return ${enumDef.values.head.getName()};\n"
        ret ++= "    end case;\n"
        ret ++= s"  end pkg_to$enumName;\n\n"


        def idToBits(enum: SpinalEnumElement[_]): String = {
          val str = enum.id.toString(2)
          "\"" + ("0" * (enum.getWidth - str.length)) + str + "\""

        }
      }
      ret ++= s"end $enumPackageName;\n\n\n"
    }
    out.write(ret.result())
  }

  def emitPackage(out: java.io.FileWriter): Unit = {

    def pkgExtractBool(kind: String): Tuple2[String, String] = {
      val ret = new StringBuilder();
      (s"function pkg_extract (that : $kind; bitId : integer) return std_logic", {
        ret ++= "  begin\n"
        ret ++= "    return that(bitId);\n"
        ret ++= "  end pkg_extract;\n\n"
        ret.result()
      })
    }
    //    def pkgExtractOffsetedBitCound(kind: String): Tuple2[String, String] = {
    //      val ret = new StringBuilder();
    //      (s"function pkg_extract (that : $kind;offset : unsigned; bitCount : natural) return $kind", {
    //        ret ++= s"    variable temp : $kind(bitCount-1 downto 0);\n"
    //        ret ++= "  begin\n"
    //        ret ++= "    temp := pkg_resize(pkg_shiftRight(that,offset),bitCount);\n"
    //        ret ++= "    return temp;\n"
    //        ret ++= "  end pkg_extract;\n\n"
    //        ret.result()
    //      })
    //    }
    def pkgCat(kind: String): Tuple2[String, String] = {
      val ret = new StringBuilder();
      (s"function pkg_cat (a : $kind; b : $kind) return $kind", {
        ret ++= s"    variable cat : $kind(a'length + b'length-1 downto 0);\n"
        ret ++= s"  begin\n"
        ret ++= s"    cat := a & b;\n"
        ret ++= s"    return cat;\n"
        ret ++= s"  end pkg_cat;\n\n"
        ret.result()
      })
    }

    def pkgDummy(kind: String): Tuple2[String, String] = {
      val ret = new StringBuilder();
      (s"function pkg_dummy (that : $kind) return $kind", {
        ret ++= s"    variable dummy : $kind(that'high downto 0);\n"
        ret ++= s"  begin\n"
        ret ++= s"    dummy := that;\n"
        ret ++= s"    return dummy;\n"
        ret ++= s"  end pkg_dummy;\n\n"
        ret.result()
      })
    }

    /*def pkgResize(kind: String): Tuple2[String, String] = {
      (s"function pkg_extract (that : $kind; width : integer) return std_logic",
        s"""|  begin
            |    return that(bitId);
            |  end pkg_extract;
            |  """.stripMargin)
    }
*/
    val vectorTypes = "std_logic_vector" :: "unsigned" :: "signed" :: Nil
    val funcs = ArrayBuffer[Tuple2[String, String]]()
    vectorTypes.foreach(kind => {
      funcs += pkgExtractBool(kind)
      funcs += pkgDummy(kind)
      funcs += pkgCat(kind)
    })



    val ret = new StringBuilder();
    ret ++= s"library IEEE;\n"
    ret ++= "use IEEE.STD_LOGIC_1164.ALL;\n"
    ret ++= "use IEEE.NUMERIC_STD.all;\n"
    ret ++= "\n"
    ret ++= s"package $packageName is\n"
    ret ++= s"${funcs.map("  " + _._1 + ";\n").reduce(_ + _)}\n"
    ret ++= "\n"
    ret ++= "  function pkg_mux (sel : std_logic;one : std_logic;zero : std_logic) return std_logic;\n"
    ret ++= "  function pkg_mux (sel : std_logic;one : std_logic_vector;zero : std_logic_vector) return std_logic_vector;\n"
    ret ++= "  function pkg_mux (sel : std_logic;one : unsigned;zero : unsigned) return unsigned;\n"
    ret ++= "  function pkg_mux (sel : std_logic;one : signed;zero : signed) return signed;\n"
    ret ++= s"\n"
    ret ++= "  function pkg_toStdLogic (value : boolean) return std_logic;\n"
    ret ++= "  function pkg_toStdLogicVector (value : std_logic) return std_logic_vector;\n"
    ret ++= "  function pkg_toUnsigned(value : std_logic) return unsigned;\n"
    ret ++= "  function pkg_toSigned (value : std_logic) return signed;\n"
    ret ++= "  function pkg_stdLogicVector (lit : std_logic_vector; bitCount : integer) return std_logic_vector;\n"
    ret ++= "  function pkg_unsigned (lit : unsigned; bitCount : integer) return unsigned;\n"
    ret ++= "  function pkg_signed (lit : signed; bitCount : integer) return signed;\n"
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
    ret ++= "  function pkg_shiftLeft (that : signed; size : unsigned) return signed;\n"
    ret ++= s"end  $packageName;\n"
    ret ++= "\n"
    ret ++= s"package body $packageName is\n"
    ret ++= s"${funcs.map(f => "  " + f._1 + " is\n" + f._2 + "\n").reduce(_ + _)}"
    ret ++= "\n"
    ret ++= "  -- unsigned shifts\n"
    ret ++= "  function pkg_shiftRight (that : unsigned; size : natural) return unsigned is\n"
    ret ++= "  begin\n"
    ret ++= "    if size >= that then\n"
    ret ++= "      return \"\";\n"
    ret ++= "    else\n"
    ret ++= "      return shift_right(that,size)(that'high-size downto 0);\n"
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
    ret ++= "    return signed(pkg_shiftRight(unsigned(that),size));\n"
    ret ++= "  end pkg_shiftRight;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftLeft (that : signed; size : natural) return signed is\n"
    ret ++= "  begin\n"
    ret ++= "    return signed(pkg_shiftLeft(unsigned(that),size));\n"
    ret ++= "  end pkg_shiftLeft;\n"
    ret ++= "\n"
    ret ++= "  function pkg_shiftLeft (that : signed; size : unsigned) return signed is\n"
    ret ++= "  begin\n"
    ret ++= "    return signed(pkg_shiftLeft(unsigned(that),size));\n"
    ret ++= "  end pkg_shiftLeft;\n"
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
    ret ++= "  function pkg_mux (sel : std_logic;one : std_logic;zero : std_logic) return std_logic is\n"
    ret ++= "  begin\n"
    ret ++= "    if sel = '1' then\n"
    ret ++= "      return one;\n"
    ret ++= "    else\n"
    ret ++= "      return zero;\n"
    ret ++= "    end if;\n"
    ret ++= "  end pkg_mux;\n"
    ret ++= "\n"
    ret ++= "  function pkg_mux (sel : std_logic;one : std_logic_vector;zero : std_logic_vector) return std_logic_vector is\n"
    ret ++= "  begin\n"
    ret ++= "    if sel = '1' then\n"
    ret ++= "      return one;\n"
    ret ++= "    else\n"
    ret ++= "      return zero;\n"
    ret ++= "    end if;\n"
    ret ++= "  end pkg_mux;\n"
    ret ++= "\n"
    ret ++= "  function pkg_mux (sel : std_logic;one : unsigned;zero : unsigned) return unsigned is\n"
    ret ++= "  begin\n"
    ret ++= "    if sel = '1' then\n"
    ret ++= "      return one;\n"
    ret ++= "    else\n"
    ret ++= "      return zero;\n"
    ret ++= "    end if;\n"
    ret ++= "  end pkg_mux;\n"
    ret ++= "\n"
    ret ++= "  function pkg_mux (sel : std_logic;one : signed;zero : signed) return signed is\n"
    ret ++= "  begin\n"
    ret ++= "    if sel = '1' then\n"
    ret ++= "      return one;\n"
    ret ++= "    else\n"
    ret ++= "      return zero;\n"
    ret ++= "    end if;\n"
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
    ret ++= "  function pkg_stdLogicVector (lit : std_logic_vector; bitCount : integer) return std_logic_vector is\n"
    ret ++= "  begin\n"
    ret ++= "    return pkg_resize(lit,bitCount);\n"
    ret ++= "  end pkg_stdLogicVector;\n"
    ret ++= "\n"
    ret ++= "  function pkg_unsigned (lit : unsigned; bitCount : integer) return unsigned is\n"
    ret ++= "  begin\n"
    ret ++= "    return pkg_resize(lit,bitCount);\n"
    ret ++= "  end pkg_unsigned;\n"
    ret ++= "\n"
    ret ++= "  function pkg_signed (lit : signed; bitCount : integer) return signed is\n"
    ret ++= "  begin\n"
    ret ++= "    return pkg_resize(lit,bitCount);\n"
    ret ++= "  end pkg_signed;\n"
    ret ++= "\n"
    ret ++= "  function pkg_resize (that : std_logic_vector; width : integer) return std_logic_vector is\n"
    ret ++= "  begin\n"
    ret ++= "    return std_logic_vector(resize(unsigned(that),width));\n"
    ret ++= "  end pkg_resize;\n"
    ret ++= "\n"
    ret ++= "  function pkg_resize (that : unsigned; width : integer) return unsigned is\n"
    ret ++= "  begin\n"
    ret ++= "    return resize(that,width);\n"
    ret ++= "  end pkg_resize;\n"
    ret ++= "\n"
    ret ++= "  function pkg_resize (that : signed; width : integer) return signed is\n"
    ret ++= "  begin\n"
    ret ++= "    return resize(that,width);\n"
    ret ++= "  end pkg_resize;\n"
    ret ++= s"end $packageName;\n"
    ret ++= "\n"
    ret ++= "\n"

    out.write(ret.result())
  }

  def emitLibrary(builder: ComponentBuilder): Unit = {
    val ret = builder.newPart(true)
    emitLibrary(ret)
  }
  def emitLibrary(ret: StringBuilder): Unit = {
    ret ++= "library IEEE;\n"
    ret ++= "use IEEE.STD_LOGIC_1164.ALL;\n"
    ret ++= "use IEEE.NUMERIC_STD.all;\n"
    ret ++= "\n"
    ret ++= s"library $library;\n"
    ret ++= s"use $library.$packageName.all;\n"
    ret ++= s"use $library.$enumPackageName.all;\n\n"
  }

  def emitEntityName(component: Component): Unit = {

  }

  def emitEntity(component: Component, builder: ComponentBuilder): Unit = {
    var ret = builder.newPart(false)
    ret ++= s"\nentity ${component.definitionName} is\n"
    ret = builder.newPart(true)
    ret ++= s"  port(\n"
    component.nodes.foreach(_ match {
      case baseType: BaseType => {
        if (baseType.isIo) {
          ret ++= s"    ${baseType.getName()} : ${emitDirection(baseType)} ${emitDataType(baseType)};\n"
        }
      }
      case _ =>
    })
    /*component.getOrdredNodeIo.foreach(baseType =>
      ret ++= s"    ${baseType.getName()} : ${emitDirection(baseType)} ${emitDataType(baseType)};\n"
    )*/
    ret.setCharAt(ret.size - 2, ' ')
    ret ++= s"  );\n"
    ret = builder.newPart(false)
    ret ++= s"end ${component.definitionName};\n"
    ret ++= s"\n"

  }


  def emitArchitecture(component: Component, builder: ComponentBuilder): Unit = {
    var ret = builder.newPart(false)
    ret ++= s"architecture arch of ${component.definitionName} is\n"
    ret = builder.newPart(true)
    emitBlackBoxComponents(component, ret)
    emitAttributesDef(component, ret)
    emitSignals(component, ret)
    ret ++= s"begin\n"
    emitComponentInstances(component, ret)
    emitAsyncronous(component, ret)
    emitSyncronous(component, ret)
    ret ++= s"end arch;\n"
    ret ++= s"\n"


  }

  def emitBlackBoxComponents(component: Component, ret: StringBuilder): Unit = {
    val emited = mutable.Set[String]()
    for (c <- component.kinds) c match {
      case blackBox: BlackBox => {
        if (!emited.contains(blackBox.definitionName)) {
          emited += blackBox.definitionName
          emitBlackBoxComponent(blackBox, ret)
        }
      }
      case _ =>
    }
  }

  def emitBlackBoxComponent(component: BlackBox, ret: StringBuilder): Unit = {
    ret ++= s"\n  component ${component.definitionName} is\n"
    val genericFlat = component.generic.flatten
    if (genericFlat.size != 0) {
      ret ++= s"    generic(\n"
      for ((name, e) <- genericFlat) {
        e match {
          case baseType: BaseType => ret ++= s"      ${emitReference(baseType)} : ${emitDataType(baseType, false)};\n"
          case s: String => ret ++= s"      $name : string;\n"
          case i: Int => ret ++= s"      $name : integer;\n"
          case d: Double => ret ++= s"      $name : real;\n"
          case b: Boolean => ret ++= s"      $name : boolean;\n"
        }
      }

      //      genericFlat.foreach(_._2 match {
      //        case baseType: BaseType => ret ++= s"      ${baseType.getName()} : ${emitDataType(baseType, false)};\n"
      //        case string : String =>ret ++= s"      ${string.getName()} : ${emitDataType(baseType, false)};\n"
      //      })
      ret.setCharAt(ret.size - 2, ' ')
      ret ++= s"    );\n"
    }
    ret ++= s"    port(\n"
    component.nodes.foreach(_ match {
      case baseType: BaseType => {
        if (baseType.isIo) {
          ret ++= s"      ${baseType.getName()} : ${emitDirection(baseType)} ${emitDataType(baseType, false)};\n"
        }
      }
      case _ =>
    })
    ret.setCharAt(ret.size - 2, ' ')
    ret ++= s"    );\n"
    ret ++= s"  end component;\n"
    ret ++= s"  \n"
  }

  def emitAttributesDef(component: Component, ret: StringBuilder): Unit = {
    val map = mutable.Map[String, Attribute]()

    for (node <- component.nodes) {
      node match {
        case attributeReady: AttributeReady => {
          for (attribute <- attributeReady.attributes) {
            val mAttribute = map.getOrElseUpdate(attribute.getName, attribute)
            if (!mAttribute.sameType(attribute)) SpinalError(s"There is some attributes with different nature (${attribute} and ${mAttribute} at ${node.component}})")
          }
        }
        case _ =>
      }
    }

    for (attribute <- map.values) {
      val typeString = attribute match {
        case _: AttributeString => "string"
        case _: AttributeFlag => "boolean"
      }
      ret ++= s"  attribute ${attribute.getName} : $typeString;\n"
    }

    ret ++= "\n"
  }

  def emitSignals(component: Component, ret: StringBuilder): Unit = {
    for (node <- component.nodes) {
      node match {
        case signal: BaseType => {
          if (!signal.isIo)
            ret ++= emitSignal(signal, signal);

          emitAttributes(signal, "signal", ret)
        }
        //        case outBinding: OutBinding => {
        //          ret ++= emitSignal(outBinding, outBinding.out);
        //          emitAttributes(outBinding, "signal", ret)
        //        }
        case mem: Mem[_] => {
          ret ++= s"  type ${emitReference(mem)}_type is array (0 to ${mem.wordCount - 1}) of std_logic_vector(${mem.getWidth - 1} downto 0);\n"
          ret ++= emitSignal(mem, mem);
          emitAttributes(mem, "signal", ret)
        }
        case _ =>
      }


    }
  }

  def emitAttributes(node: Node, vhdlType: String, ret: StringBuilder): Unit = {
    if (!node.isInstanceOf[AttributeReady]) return
    val attributeReady = node.asInstanceOf[AttributeReady]

    for (attribute <- attributeReady.attributes) {
      val value = attribute match {
        case attribute: AttributeString => "\"" + attribute.value + "\""
        case attribute: AttributeFlag => "true"
      }

      ret ++= s"  attribute ${attribute.getName} of ${emitReference(node)}: signal is $value;\n"
    }
  }


  def getSensitivity(nodes: Iterable[Node], includeNodes: Boolean): mutable.Set[Node] = {
    val sensitivity = mutable.Set[Node]()

    if (includeNodes)
      nodes.foreach(walk(_))
    else
      nodes.foreach(_.inputs.foreach(walk(_)))

    def walk(node: Node): Unit = {
      if (isReferenceable(node))
        sensitivity += node
      else
        node.inputs.foreach(walk(_))
    }

    sensitivity
  }
  def emitAsyncronous(component: Component, ret: StringBuilder): Unit = {

    var processCounter = 0
    class Process(val order: Int) {
      var sensitivity: mutable.Set[Node] = null
      val nodes = ArrayBuffer[Node]()
      val whens = ArrayBuffer[when]()
      var hasMultipleAssignment = false

      def genSensitivity: Unit = sensitivity = getSensitivity(nodes, false)


      def needProcessDef : Boolean = {
        if (!whens.isEmpty || nodes.size > 1) return true
        if (hasMultipleAssignment) {
          val ma: MultipleAssignmentNode = nodes(0).inputs(0).asInstanceOf[MultipleAssignmentNode]
          val assignedBits = AssignedBits()
          for (input <- ma.inputs) input match{
            case assign : AssignementNode => {
              val scope = assign.getScopeBits
              if(!AssignedBits.intersect(scope,assignedBits).isEmpty) return true
              assignedBits.add(scope)
            }
            case _ => return true
          }
        }
        return false
      }
    }

    val processSet = mutable.Set[Process]()
    val whenToProcess = mutable.Map[when, Process]()

    def move(to: Process, from: Process): Unit = {
      to.nodes ++= from.nodes
      to.whens ++= from.whens
      to.hasMultipleAssignment |= from.hasMultipleAssignment
      from.whens.foreach(whenToProcess(_) = to)
      processSet.remove(from)
    }

    val asyncSignals = component.nodes.filter(_ match {
      case signal: BaseType => (!signal.isDelay) && (!((signal.isIo && signal.isInput) || component.kindsOutputsBindings.contains(signal)))
      case _ => false
    })

    for (signal <- asyncSignals) {
      var process: Process = null
      var hasMultipleAssignment = false
      walk(signal.inputs(0))
      def walk(that: Node): Unit = {
        that match {
          case wn: WhenNode => {
            if (whenToProcess.contains(wn.w)) {
              val otherProcess = whenToProcess.get(wn.w).get
              if (process == null) {
                process = otherProcess
                otherProcess.nodes += signal
              } else if (process != otherProcess) {
                move(otherProcess, process)
                process = otherProcess
              }
            } else {
              if (process == null) {
                process = new Process(processCounter);
                processCounter += 1
                process.nodes += signal
                processSet += process
              }
              process.whens += wn.w
              whenToProcess += (wn.w -> process)
            }

            walk(wn.whenTrue)
            walk(wn.whenFalse)
          }
          case man: MultipleAssignmentNode => {
            man.inputs.foreach(walk(_))
            hasMultipleAssignment = true
          }
          case that => {
            if (process == null) {
              process = new Process(processCounter);
              processCounter += 1
              process.nodes += signal
              processSet += process
            }
          }
        }
      }

      process.hasMultipleAssignment |= hasMultipleAssignment
    }

    val processList = processSet.toList.sortWith(_.order < _.order)


    for (process <- processList if !process.needProcessDef) {
      for (node <- process.nodes) {
        emitAssignement(node, node.inputs(0), ret, "  ")
        //ret ++= s"  ${emitReference(node)} <= ${emitLogic(node.inputs(0))};\n"
      }
    }

    for (process <- processList if process.needProcessDef) {
      process.genSensitivity

      if (process.sensitivity.size != 0) {
        val context = new AssignementLevel
        for (node <- process.nodes) {
          context.walkWhenTree(node, node.inputs(0))
        }

        ret ++= s"  process(${process.sensitivity.toList.sortWith(_.instanceCounter < _.instanceCounter).map(emitReference(_)).reduceLeft(_ + "," + _)})\n"
        ret ++= "  begin\n"
        context.emitContext(ret, "    ")
        ret ++= "  end process;\n\n"
      } else {
        for (node <- process.nodes) {
          ret ++= s"  ${emitReference(node)} <= ${emitLogic(node.inputs(0))};\n"
        }
      }
    }

  }


  def operatorImplAsOperator(vhd: String)(op: Modifier): String = {
    op.inputs.size match {
      case 1 => s"($vhd ${emitLogic(op.inputs(0))})"
      case 2 => {
        val temp = s"(${emitLogic(op.inputs(0))} $vhd ${emitLogic(op.inputs(1))})"
        if (opThatNeedBoolCast.contains(op.opName))
          return s"pkg_toStdLogic$temp"
        else
          return temp
      }
    }
  }

  def operatorImplAsFunction(vhd: String)(func: Modifier): String = {
    s"$vhd(${func.inputs.map(emitLogic(_)).reduce(_ + "," + _)})"
  }

  def operatorImplAsBitsToEnum(func: Modifier): String = {
    val enumCast = func.asInstanceOf[EnumCast]
    s"pkg_to${enumCast.enum.getParentName}(${func.inputs.map(emitLogic(_)).reduce(_ + "," + _)})"
  }

  val modifierImplMap = mutable.Map[String, Modifier => String]()


  //unsigned
  modifierImplMap.put("u+u", operatorImplAsOperator("+"))
  modifierImplMap.put("u-u", operatorImplAsOperator("-"))
  modifierImplMap.put("u*u", operatorImplAsOperator("*"))

  modifierImplMap.put("u|u", operatorImplAsOperator("or"))
  modifierImplMap.put("u&u", operatorImplAsOperator("and"))
  modifierImplMap.put("u^u", operatorImplAsOperator("xor"))
  modifierImplMap.put("~u", operatorImplAsOperator("not"))

  modifierImplMap.put("u==u", operatorImplAsOperator("="))
  modifierImplMap.put("u!=u", operatorImplAsOperator("/="))
  modifierImplMap.put("u<u", operatorImplAsOperator("<"))
  modifierImplMap.put("u<=u", operatorImplAsOperator("<="))


  modifierImplMap.put("u>>i", operatorImplAsFunction("pkg_shiftRight"))
  modifierImplMap.put("u<<i", operatorImplAsFunction("pkg_shiftLeft"))
  modifierImplMap.put("u>>u", operatorImplAsFunction("pkg_shiftRight"))
  modifierImplMap.put("u<<u", operatorImplAsFunction("pkg_shiftLeft"))


  //signed
  modifierImplMap.put("s+s", operatorImplAsOperator("+"))
  modifierImplMap.put("s-s", operatorImplAsOperator("-"))
  modifierImplMap.put("s*s", operatorImplAsOperator("*"))

  modifierImplMap.put("s|s", operatorImplAsOperator("or"))
  modifierImplMap.put("s&s", operatorImplAsOperator("and"))
  modifierImplMap.put("s^s", operatorImplAsOperator("xor"))
  modifierImplMap.put("~s", operatorImplAsOperator("not"))
  modifierImplMap.put("-s", operatorImplAsOperator("-"))

  modifierImplMap.put("s==s", operatorImplAsOperator("="))
  modifierImplMap.put("s!=s", operatorImplAsOperator("/="))
  modifierImplMap.put("s<s", operatorImplAsOperator("<"))
  modifierImplMap.put("s<=s", operatorImplAsOperator("<="))


  modifierImplMap.put("s>>i", operatorImplAsFunction("pkg_shiftRight"))
  modifierImplMap.put("s<<i", operatorImplAsFunction("pkg_shiftLeft"))
  modifierImplMap.put("s>>u", operatorImplAsFunction("pkg_shiftRight"))
  modifierImplMap.put("s<<u", operatorImplAsFunction("pkg_shiftLeft"))



  //bits
  modifierImplMap.put("b##b", operatorImplAsFunction("pkg_cat"))

  modifierImplMap.put("b|b", operatorImplAsOperator("or"))
  modifierImplMap.put("b&b", operatorImplAsOperator("and"))
  modifierImplMap.put("b^b", operatorImplAsOperator("xor"))
  modifierImplMap.put("~b", operatorImplAsOperator("not"))

  modifierImplMap.put("b==b", operatorImplAsOperator("="))
  modifierImplMap.put("b!=b", operatorImplAsOperator("/="))

  modifierImplMap.put("b>>i", operatorImplAsFunction("pkg_shiftRight"))
  modifierImplMap.put("b<<i", operatorImplAsFunction("pkg_shiftLeft"))
  modifierImplMap.put("b>>u", operatorImplAsFunction("pkg_shiftRight"))
  modifierImplMap.put("b<<u", operatorImplAsFunction("pkg_shiftLeft"))




  //bool
  modifierImplMap.put("B==B", operatorImplAsOperator("="))
  modifierImplMap.put("B!=B", operatorImplAsOperator("/="))


  modifierImplMap.put("!", operatorImplAsOperator("not"))
  modifierImplMap.put("&&", operatorImplAsOperator("and"))
  modifierImplMap.put("||", operatorImplAsOperator("or"))
  modifierImplMap.put("B^B", operatorImplAsOperator("xor"))


  //enum
  modifierImplMap.put("e==e", operatorImplAsOperator("="))
  modifierImplMap.put("e!=e", operatorImplAsOperator("/="))

  //cast
  modifierImplMap.put("s->b", operatorImplAsFunction("std_logic_vector"))
  modifierImplMap.put("u->b", operatorImplAsFunction("std_logic_vector"))
  modifierImplMap.put("B->b", operatorImplAsFunction("pkg_toStdLogicVector"))
  modifierImplMap.put("e->b", operatorImplAsFunction("pkg_toStdLogicVector"))

  modifierImplMap.put("b->s", operatorImplAsFunction("signed"))
  modifierImplMap.put("u->s", operatorImplAsFunction("signed"))

  modifierImplMap.put("b->u", operatorImplAsFunction("unsigned"))
  modifierImplMap.put("s->u", operatorImplAsFunction("unsigned"))

  modifierImplMap.put("b->e", operatorImplAsBitsToEnum)


  //misc
  modifierImplMap.put("resize(s,i)", operatorImplAsFunction("pkg_resize"))
  modifierImplMap.put("resize(u,i)", operatorImplAsFunction("pkg_resize"))
  modifierImplMap.put("resize(b,i)", operatorImplAsFunction("pkg_resize"))

  //Memo whenNode hardcode emitlogic
  modifierImplMap.put("mux(B,B,B)", operatorImplAsFunction("pkg_mux"))
  modifierImplMap.put("mux(B,b,b)", operatorImplAsFunction("pkg_mux"))
  modifierImplMap.put("mux(B,u,u)", operatorImplAsFunction("pkg_mux"))
  modifierImplMap.put("mux(B,s,s)", operatorImplAsFunction("pkg_mux"))
  modifierImplMap.put("mux(B,e,e)", operatorImplAsFunction("pkg_mux"))

  modifierImplMap.put("extract(b,i)", extractBoolFixed)
  modifierImplMap.put("extract(u,i)", extractBoolFixed)
  modifierImplMap.put("extract(s,i)", extractBoolFixed)

  modifierImplMap.put("extract(b,u)", extractBoolFloating)
  modifierImplMap.put("extract(u,u)", extractBoolFloating)
  modifierImplMap.put("extract(s,u)", extractBoolFloating)

  modifierImplMap.put("extract(b,i,i)", extractBitVectorFixed)
  modifierImplMap.put("extract(u,i,i)", extractBitVectorFixed)
  modifierImplMap.put("extract(s,i,i)", extractBitVectorFixed)

  modifierImplMap.put("extract(b,u,w)", extractBitVectorFloating)
  modifierImplMap.put("extract(u,u,w)", extractBitVectorFloating)
  modifierImplMap.put("extract(s,u,w)", extractBitVectorFloating)


  def extractBoolFixed(func: Modifier): String = {
    val that = func.asInstanceOf[ExtractBoolFixed]
    s"pkg_extract(${emitLogic(that.getBitVector)},${that.getBitId})"
  }

  def extractBoolFloating(func: Modifier): String = {
    val that = func.asInstanceOf[ExtractBoolFloating]
    s"pkg_extract(${emitLogic(that.getBitVector)},to_integer(${emitLogic(that.getBitId)}))"
  }

  def extractBitVectorFixed(func: Modifier): String = {
    val that = func.asInstanceOf[ExtractBitsVectorFixed]
    s"pkg_extract(${emitLogic(that.getBitVector)},${that.getHi},${that.getLo})"
  }
  def extractBitVectorFloating(func: Modifier): String = {
    val that = func.asInstanceOf[ExtractBitsVectorFloating]
    s"pkg_dummy(${emitLogic(that.getBitVector)}(to_integer(${emitLogic(that.getOffset)}) + ${that.getBitCount.value - 1}  downto to_integer(${emitLogic(that.getOffset)})))"
  }


  def opThatNeedBoolCastGen(a: String, b: String): List[String] = {
    ("==" :: "!=" :: "<" :: "<=" :: Nil).map(a + _ + b)
  }

  val opThatNeedBoolCast = mutable.Set[String]()
  opThatNeedBoolCast ++= opThatNeedBoolCastGen("B", "B")
  opThatNeedBoolCast ++= opThatNeedBoolCastGen("b", "b")
  opThatNeedBoolCast ++= opThatNeedBoolCastGen("u", "u")
  opThatNeedBoolCast ++= opThatNeedBoolCastGen("s", "s")
  opThatNeedBoolCast ++= opThatNeedBoolCastGen("e", "e")


  def emitLogic(node: Node): String = node match {
    case baseType: BaseType => emitReference(baseType)
    case node: Modifier => modifierImplMap.getOrElse(node.opName, throw new Exception("can't find " + node.opName))(node)
    case lit: BitsLiteral => lit.kind match {
      case _: Bits => s"pkg_stdLogicVector(X${'\"'}${lit.value.toString(16)}${'\"'},${lit.getWidth})"
      case _: UInt => s"pkg_unsigned(X${'\"'}${lit.value.toString(16)}${'\"'},${lit.getWidth})"
      case _: SInt => s"pkg_signed(X${'\"'}${lit.value.toString(16)}${'\"'},${lit.getWidth})"
    }
    case lit: IntLiteral => lit.value.toString(10)
    case lit: BoolLiteral => "\'" + (if (lit.value) "1" else "0") + "\'"
    case lit: EnumLiteral[_] => lit.enum.getName()
    case memRead: MemReadAsync => {
      if (memRead.writeToReadKind == dontCare) SpinalWarning(s"memReadAsync with dontCare is as writeFirst into VHDL")
      s"${emitReference(memRead.getMem)}(to_integer(${emitReference(memRead.getAddress)}))"
    }
    case whenNode: WhenNode => s"pkg_mux(${whenNode.inputs.map(emitLogic(_)).reduce(_ + "," + _)})" //Exeptional case with asyncrouns of literal
    case o => throw new Exception("Don't know how emit logic of " + o.getClass.getSimpleName)
  }


  def emitSyncronous(component: Component, ret: StringBuilder): Unit = {
    // ret ++= "  -- synchronous\n"

    val syncNodes = component.getDelays

    val clockDomainMap = mutable.Map[ClockDomain, ArrayBuffer[SyncNode]]()

    for (syncNode <- syncNodes) {
      clockDomainMap.getOrElseUpdate(syncNode.getClockDomain, new ArrayBuffer[SyncNode]()) += syncNode
    }

    for ((clockDomain, array) <- clockDomainMap) {
      val arrayWithReset = ArrayBuffer[SyncNode]()
      val arrayWithoutReset = ArrayBuffer[SyncNode]()

      for (syncNode <- array) {
        if (syncNode.isUsingReset) arrayWithReset += syncNode else arrayWithoutReset += syncNode
      }

      emitClockDomain(true)
      emitClockDomain(false)



      def emitClockDomain(withReset: Boolean): Unit = {
        val activeArray = if (withReset) arrayWithReset else arrayWithoutReset
        if (activeArray.size == 0) return;
        val clock = component.pulledDataCache.getOrElse(clockDomain.clock, throw new Exception("???")).asInstanceOf[Bool]
        val reset = if (null == clockDomain.reset || !withReset) null else component.pulledDataCache.getOrElse(clockDomain.reset, throw new Exception("???")).asInstanceOf[Bool]
        val clockEnable = if (null == clockDomain.clockEnable) null else component.pulledDataCache.getOrElse(clockDomain.clockEnable, throw new Exception("???")).asInstanceOf[Bool]
        val asyncReset = (null != reset) && clockDomain.resetKind == ASYNC
        val syncReset = (null != reset) && clockDomain.resetKind == SYNC
        var tabLevel = 1
        def tabStr = "  " * tabLevel
        def inc = {
          tabLevel = tabLevel + 1
        }
        def dec = {
          tabLevel = tabLevel - 1
        }

        val initialValueAssignement = new AssignementLevel
        val initialValues = ArrayBuffer[Node]()
        for (syncNode <- activeArray) syncNode match {
          case reg: Reg => {
            if (reg.hasInitialValue) {
              initialValueAssignement.walkWhenTree(reg.getOutputByConsumers, reg.getInitialValue)
              initialValues += reg.getInitialValue
            }
          }
          case _ =>
        }


        if (asyncReset) {
          val sensitivity = getSensitivity(initialValues, true)
          ret ++= s"${tabStr}process(${emitReference(clock)},${emitReference(reset)}${sensitivity.foldLeft("")(_ + "," + emitReference(_))})\n"
        } else {
          ret ++= s"${tabStr}process(${emitReference(clock)})\n"
        }

        ret ++= s"${tabStr}begin\n"
        inc
        if (asyncReset) {
          ret ++= s"${tabStr}if ${emitReference(reset)} = \'${if (clockDomain.resetActiveHigh) 1 else 0}\' then\n";
          inc
          emitRegsInitialValue(initialValueAssignement, tabStr)
          dec
          ret ++= s"${tabStr}elsif ${emitClockEdge(clock, clockDomain.edge)}"
          inc
        } else {
          ret ++= s"${tabStr}if ${emitClockEdge(clock, clockDomain.edge)}"
          inc
        }
        if (clockEnable != null) {
          ret ++= s"${tabStr}if ${emitReference(clockEnable)} = \'${if (clockDomain.clockEnableActiveHigh) 1 else 0}\' then\n"
          inc
        }
        if (syncReset) {
          ret ++= s"${tabStr}if ${emitReference(reset)} = \'${if (clockDomain.resetActiveHigh) 1 else 0}\' then\n"
          inc
          emitRegsInitialValue(initialValueAssignement, tabStr)
          dec
          ret ++= s"${tabStr}else\n"
          inc
          emitRegsLogic(tabStr)
          dec
          ret ++= s"${tabStr}end if;\n"
          dec
        } else {
          emitRegsLogic(tabStr)
          dec
        }

        while (tabLevel != 1) {
          ret ++= s"${tabStr}end if;\n"
          dec
        }
        ret ++= s"${tabStr}end process;\n"
        dec
        ret ++= s"${tabStr}\n"


        def emitRegsInitialValue(assignementLevel: AssignementLevel, tab: String): Unit = {
          assignementLevel.emitContext(ret, tab)
        }


        def emitRegsLogic(tab: String): Unit = {


          val rootContext = new AssignementLevel

          for (syncNode <- activeArray) syncNode match {
            case reg: Reg => {
              val regSignal = reg.getOutputByConsumers
              if (!regSignal.isIo || !regSignal.isInput) {
                val in = reg.getDataInput
                if (in != reg)
                  rootContext.walkWhenTree(regSignal, in)
              }
            }
            case memWrite: MemWrite => {
              if (memWrite.useWriteEnable) {
                ret ++= s"${tab}if ${emitReference(memWrite.getEnable)} = '1' then\n"
                emitWrite(tab + "  ")
                ret ++= s"${tab}end if;\n"
              } else {
                emitWrite(tab)
              }

              def emitWrite(tab: String) = ret ++= s"$tab${emitReference(memWrite.getMem)}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)};\n"
            }
            case memReadSync: MemReadSync => {
              //readFirst
              if (memReadSync.writeToReadKind == writeFirst) SpinalError(s"Can't translate a memReadSync with writeFirst into VHDL $memReadSync")
              if (memReadSync.writeToReadKind == dontCare) SpinalWarning(s"memReadSync with dontCare is as readFirst into VHDL $memReadSync")
              if (memReadSync.useReadEnable) {
                ret ++= s"${tab}if ${emitReference(memReadSync.getEnable)} = '1' then\n"
                emitRead(tab + "  ")
                ret ++= s"${tab}end if;\n"
              } else {
                emitRead(tab)
              }
              def emitRead(tab: String) = ret ++= s"$tab${emitReference(memReadSync.consumers(0))} <= ${emitReference(memReadSync.getMem)}(to_integer(${emitReference(memReadSync.getAddress)}));\n"

            }
          }
          rootContext.emitContext(ret, tab)
        }
      }
    }
  }

  def emitAssignement(to: Node, from: Node, ret: StringBuilder, tab: String): Unit = {
    from match {
      case from: AssignementNode => {
        from match {
          case assign: BitAssignmentFixed => ret ++= s"$tab${emitReference(to)}(${assign.getBitId}) <= ${emitLogic(assign.getInput)};\n"
          case assign: BitAssignmentFloating => ret ++= s"$tab${emitReference(to)}(to_integer(${emitLogic(assign.getBitId)})) <= ${emitLogic(assign.getInput)};\n"
          case assign: RangedAssignmentFixed => ret ++= s"$tab${emitReference(to)}(${assign.getHi} downto ${assign.getLo}) <= ${emitLogic(assign.getInput)};\n"
          case assign: RangedAssignmentFloating => ret ++= s"$tab${emitReference(to)}(${assign.getBitCount.value - 1} + to_integer(${emitLogic(assign.getOffset)}) downto to_integer(${emitLogic(assign.getOffset)})) <= ${emitLogic(assign.getInput)};\n"
        }
      }
      case man : MultipleAssignmentNode => { //For some case with asyncronous partial assignement
        for(assign <- man.inputs){
          emitAssignement(to,assign,ret,tab)
        }
      }
      case _ => ret ++= s"$tab${emitReference(to)} <= ${emitLogic(from)};\n"
    }
  }


  class WhenTree(val cond: Node, val instanceCounter: Int) {
    var whenTrue: AssignementLevel = new AssignementLevel
    var whenFalse: AssignementLevel = new AssignementLevel
  }

  class AssignementLevel {
    val logicChunk = mutable.Map[WhenTree, ArrayBuffer[(Node, Node)]]()
    val when = mutable.Map[when, WhenTree]()

    def isEmpty = logicChunk.isEmpty && when.isEmpty

    def isNotEmpty = !isEmpty


    def walkWhenTree(root: Node, that: Node): Unit = {
      def getElements: ArrayBuffer[Node] = {
        if (that.isInstanceOf[MultipleAssignmentNode]) {
          return that.inputs
        } else {
          return ArrayBuffer(that)
        }
      }

      var lastWhenTree: WhenTree = null
      for (node <- getElements) {
        node match {
          case whenNode: WhenNode => {
            if (!whenNode.whenTrue.isInstanceOf[NoneNode]) {
              val when = this.when.getOrElseUpdate(whenNode.w, new WhenTree(whenNode.cond, node.instanceCounter))
              lastWhenTree = when
              when.whenTrue.walkWhenTree(root, whenNode.whenTrue)
            }
            if (!whenNode.whenFalse.isInstanceOf[NoneNode]) {
              val when = this.when.getOrElseUpdate(whenNode.w, new WhenTree(whenNode.cond, node.instanceCounter))
              lastWhenTree = when
              when.whenFalse.walkWhenTree(root, whenNode.whenFalse)
            }
          }
          case reg: Reg =>
          case _ => this.logicChunk.getOrElseUpdate(lastWhenTree, new ArrayBuffer[(Node, Node)]) += new Tuple2(root, node)
        }
      }
    }


    def emitContext(ret: mutable.StringBuilder, tab: String): Unit = {
      def emitLogicChunk(key: WhenTree): Unit = {
        if (this.logicChunk.contains(key)) {
          for ((to, from) <- this.logicChunk.get(key).get) {
            emitAssignement(to, from, ret, tab)
          }
        }
      }

      emitLogicChunk(null)

      for (when <- this.when.values.toList.sortWith(_.instanceCounter < _.instanceCounter)) {
        def doTrue = when.whenTrue.isNotEmpty
        def doFalse = when.whenFalse.isNotEmpty

        if (!doTrue && doFalse) {
          ret ++= s"${tab}if ${emitLogic(when.cond)} = '0'  then\n"
          when.whenFalse.emitContext(ret, tab + "  ")
          ret ++= s"${tab}end if;\n"
        } else if (doTrue && !doFalse) {
          ret ++= s"${tab}if ${emitLogic(when.cond)} = '1' then\n"
          when.whenTrue.emitContext(ret, tab + "  ")
          ret ++= s"${tab}end if;\n"
        } else if (doTrue && doFalse) {
          ret ++= s"${tab}if ${emitLogic(when.cond)} = '1' then\n"
          when.whenTrue.emitContext(ret, tab + "  ")
          ret ++= s"${tab}else\n"
          when.whenFalse.emitContext(ret, tab + "  ")
          ret ++= s"${tab}end if;\n"
        }
        emitLogicChunk(when)
      }
    }
  }

  def emitComponentInstances(component: Component, ret: StringBuilder): Unit = {
    for (kind <- component.kinds) {
      val isBB = kind.isInstanceOf[BlackBox]
      val definitionString = if (isBB) kind.definitionName
      else s"entity $library.${
        emitedComponentRef.getOrElse(kind, kind).definitionName
      }"
      ret ++= s"  ${
        kind.getName()
      } : $definitionString\n"
      if (kind.isInstanceOf[BlackBox]) {
        val bb = kind.asInstanceOf[BlackBox]
        val genericFlat = bb.generic.flatten
        if (genericFlat.size != 0) {
          ret ++= s"    generic map(\n"


          for ((name, e) <- genericFlat) {
            e match {
              case baseType: BaseType => ret ++= s"      ${emitReference(baseType)} => ${emitLogic(baseType.inputs(0))},\n"
              case s: String => ret ++= s"      ${name} => ${"\""}${s}${"\""},\n"
              case i: Int => ret ++= s"      ${name} => $i,\n"
              case d: Double => ret ++= s"      ${name} => $d,\n"
              case b: Boolean => ret ++= s"      ${name} => $b,\n"
            }
          }
          //          genericFlat.foreach(_._2 match {
          //            case baseType: BaseType => {
          //              ret ++= s"      ${emitReference(baseType)} => ${emitLogic(baseType.inputs(0))},\n"
          //            }
          //          })
          ret.setCharAt(ret.size - 2, ' ')
          ret ++= s"    )\n"
        }
      }
      ret ++= s"    port map (\n"
      for (data <- kind.getOrdredNodeIo) {
        if (data.isOutput) {
          val bind = component.kindsOutputsToBindings.getOrElse(data, null)
          if (bind != null) {
            ret ++= s"      ${
              emitReference(data)
            } => ${
              emitReference(bind)
            },\n"
          }
        }
        else if (data.isInput)
          ret ++= s"      ${
            emitReference(data)
          } => ${
            emitReference(data.inputs(0))
          },\n"
      }
      ret.setCharAt(ret.size - 2, ' ')

      ret ++= s"    );"
      ret ++= s"\n"
    }
  }
}

//if (asyncReset) {
//ret ++= s"${tabStr}if ${emitReference(reset)} = \'${if (clockDomain.resetActiveHigh) 1 else 0}\' then\n";
//inc
//emitRegsInitialValue(tabStr)
//dec
//ret ++= s"${tabStr}elsif ${emitClockEdge(clock, clockDomain.edge)}"
//inc
//} else {
//ret ++= s"${tabStr}if ${emitClockEdge(clock, clockDomain.edge)}"
//inc
//}
//if (syncReset) {
//ret ++= s"${tabStr}if ${emitReference(reset)} = \'${if (clockDomain.resetActiveHigh) 1 else 0}\' then\n"
//inc
//emitRegsInitialValue(tabStr)
//dec
//ret ++= s"${tabStr}else\n"
//inc
//}
//if (clockEnable != null) {
//ret ++= s"${tabStr}if ${emitReference(clockEnable)} = \'${if (clockDomain.clockEnableActiveHigh) 1 else 0}\' then\n"
//inc
//emitRegsLogic(tabStr)
//}else{
//emitRegsLogic(tabStr)
//}
//
//while (tabLevel != 2) {
//dec
//ret ++= s"${tabStr}end if;\n"
//}
//ret ++= s"${tabStr}end process;\n"
//dec
//ret ++= s"${tabStr}\n"
