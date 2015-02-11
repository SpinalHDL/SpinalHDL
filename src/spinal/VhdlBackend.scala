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
import scala.collection.mutable.ArrayBuffer


/**
 * Created by PIC18F on 07.01.2015.
 */
class VhdlBackend extends Backend with VhdlBase {
  var out: java.io.FileWriter = null
  var library = "work"
  val enumPackageName = "pkg_enum"
  val packageName = "pkg_scala2hdl"
  // + Random.nextInt(100000)
  val outputFile = "out" // + Random.nextInt(100000)


  reservedKeyWords ++= vhdlKeyWords


  override protected def elaborate[T <: Component](topLevel: T): BackendReport[T] = {
    val report = super.elaborate(topLevel)
    SpinalInfoPhase("Write VHDL")

    out = new java.io.FileWriter(outputFile + ".vhd")
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

  def emit(component: Component): String = {
    val ret = new StringBuilder()
    emitLibrary(component, ret)
    emitEntity(component, ret)
    emitArchitecture(component, ret)
    //ret ++= component.name + "\n"
    ret.result()
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

  def emitLibrary(component: Component, ret: StringBuilder): Unit = {
    ret ++= "library IEEE;"
    ret ++= "use IEEE.STD_LOGIC_1164.ALL;\n"
    ret ++= "use IEEE.NUMERIC_STD.all;\n"
    ret ++= "\n"
    ret ++= s"library $library;\n"
    ret ++= s"use $library.$packageName.all;\n"
    ret ++= s"use $library.$enumPackageName.all;\n\n"
  }

  def emitEntityName(component: Component): Unit = {

  }

  def emitEntity(component: Component, ret: StringBuilder): Unit = {
    ret ++= s"\nentity ${component.definitionName} is\n"
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
    ret ++= s"end ${component.definitionName};\n"
    ret ++= s"\n"

  }


  def emitArchitecture(component: Component, ret: StringBuilder): Unit = {
    ret ++= s"architecture arch of ${component.definitionName} is\n"
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
      genericFlat.foreach(_._2 match {
        case baseType: BaseType => {
          ret ++= s"      ${baseType.getName()} : ${emitDataType(baseType, false)};\n"
        }
        case _ =>
      })
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

  def emitAsyncronous(component: Component, ret: StringBuilder): Unit = {
    for (node <- component.nodes) {
      node match {
        case signal: BaseType => {
          if (!signal.isDelay)
            if (!((signal.isIo && signal.isInput) || component.kindsOutputsBindings.contains(signal)))
              ret ++= s"  ${emitReference(signal)} <= ${emitLogic(signal.inputs(0))};\n"
        }
        case _ =>
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

  modifierImplMap.put("s==s", operatorImplAsOperator("="))
  modifierImplMap.put("s!=s", operatorImplAsOperator("/="))
  modifierImplMap.put("s<s", operatorImplAsOperator("<"))
  modifierImplMap.put("s<=s", operatorImplAsOperator("<="))


  modifierImplMap.put("s>>i", operatorImplAsFunction("pkg_shiftRight"))
  modifierImplMap.put("s<<i", operatorImplAsFunction("pkg_shiftLeft"))
  modifierImplMap.put("s>>u", operatorImplAsFunction("pkg_shiftRight"))
  modifierImplMap.put("s<<u", operatorImplAsFunction("pkg_shiftLeft"))



  //bits
  modifierImplMap.put("##", operatorImplAsOperator("&"))

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

  modifierImplMap.put("mux(B,B,B)", operatorImplAsFunction("pkg_mux"))
  modifierImplMap.put("mux(B,b,b)", operatorImplAsFunction("pkg_mux"))
  modifierImplMap.put("mux(B,u,u)", operatorImplAsFunction("pkg_mux"))
  modifierImplMap.put("mux(B,s,s)", operatorImplAsFunction("pkg_mux"))
  modifierImplMap.put("mux(B,e,e)", operatorImplAsFunction("pkg_mux"))

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
//    case outBinding: OutBinding => emitReference(outBinding)
    case lit: BitsLiteral => lit.kind match {
      case _: Bits => s"pkg_stdLogicVector(X${'\"'}${lit.value.toString(16)}${'\"'},${lit.getWidth})"
      case _: UInt => s"pkg_unsigned(X${'\"'}${lit.value.toString(16)}${'\"'},${lit.getWidth})"
      case _: SInt => s"pkg_signed(X${'\"'}${lit.value.toString(16)}${'\"'},${lit.getWidth})"
    }
    case lit: IntLiteral => lit.value.toString(10)
    case lit: SStringLiteral => "\"" + lit.value + "\""
    case lit: BoolLiteral => "\'" + (if (lit.value) "1" else "0") + "\'"
    case lit: EnumLiteral[_] => lit.enum.getName()
    case node: Modifier => modifierImplMap.getOrElse(node.opName, throw new Exception("can't find " + node.opName))(node)
    case node: ExtractBool => {
      val bitIdString = node.bitId match {
        case bitId: IntLiteral => emitLogic(bitId)
        case bitId: UInt => s"to_integer(${emitLogic(bitId)})"
      }
      s"pkg_extract(${emitLogic(node.bitVector)},$bitIdString)"
    }
    case node: ExtractBitsVector => s"pkg_extract(${emitLogic(node.bitVector)},${node.bitIdHi.value},${node.bitIdLo.value})"
    case memRead: MemReadAsync => s"${emitReference(memRead.getMem)}(to_integer(${emitReference(memRead.getAddress)}))"

    case o => throw new Exception("Don't know how emit logic of " + o.getClass.getSimpleName)
  }


  //  case mem : Mem[_] => {
  //    for(memIn <- mem.inputs)memIn match{
  //    case write : MemWrite => {
  //
  //  }
  //  }
  //  }
  def emitSyncronous(component: Component, ret: StringBuilder): Unit = {
    // ret ++= "  -- synchronous\n"

    val delayNodes = component.getDelays

    val clockDomainMap = mutable.Map[ClockDomain, ArrayBuffer[DelayNode]]()

    for (delayNode <- delayNodes) {
      clockDomainMap.getOrElseUpdate(delayNode.getClockDomain, new ArrayBuffer[DelayNode]()) += delayNode
    }

    for ((clockDomain, array) <- clockDomainMap) {
      val arrayWithReset = ArrayBuffer[DelayNode]()
      val arrayWithoutReset = ArrayBuffer[DelayNode]()

      for (delayNode <- array){
        if(delayNode.isUsingReset) arrayWithReset += delayNode else arrayWithoutReset += delayNode
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
        ret ++= s"  process(${emitReference(clock)}${if (asyncReset) "," + emitReference(reset) else ""})\n"
        ret ++= s"  begin\n"
        if (asyncReset) {
          ret ++= s"    if ${emitReference(reset)} = \'${if (clockDomain.resetActiveHigh) 1 else 0}\' then\n"
          emitRegsInitialValue("      ")
          ret ++= s"    elsif ${emitClockEdge(clock, clockDomain.edge)}"
          /*  if(clockEnable != null){
              ret ++= s"      if ${emitReference(reset)} = \'${if (clockDomain.resetActiveHigh) 1 else 0}\' then\n"
            }*/
        } else {
          ret ++= s"    if ${emitClockEdge(clock, clockDomain.edge)}"
        }
        if (syncReset) {
          ret ++= s"      if ${emitReference(reset)} = \'${if (clockDomain.resetActiveHigh) 1 else 0}\' then\n"
          emitRegsInitialValue("        ")
          ret ++= s"      else\n"
          emitRegsLogic("        ")
          ret ++= s"      end if;\n"
        } else {
          emitRegsLogic("      ")
        }

        ret ++= s"    end if;\n"
        ret ++= s"  end process;\n"
        ret ++= s"  \n"

        def emitRegsInitialValue(tab: String): Unit = {
          for (delayNode <- activeArray) delayNode match {
            case reg: Reg => {
              if (reg.hasInitialValue) {
                ret ++= s"$tab${emitReference(reg.getOutputByConsumers)} <= ${emitLogic(reg.getInitialValue)};\n"
              }
            }
            case memWrite: MemWrite =>
          }
        }
        def emitRegsLogic(tab: String): Unit = {

          class Context {
            val logic = mutable.ArrayBuffer[(Node, Node)]()
            val when = mutable.Map[Node, WhenTree]()

            def isEmpty = logic.isEmpty && when.isEmpty
            def isNotEmpty = !isEmpty
          }

          class WhenTree(val cond: Node) {
            var whenTrue: Context = new Context
            var whenFalse: Context = new Context
          }

          val rootContext = new Context

          for (delayNode <- activeArray) delayNode match {
            case reg: Reg => {
              val regSignal = reg.getOutputByConsumers
              if (!regSignal.isIo || !regSignal.isInput) {
                val in = reg.getDataInput
                if (in != reg) //check that reg has logic
                  walkMux(in, rootContext)

                def walkMux(that: Node, context: Context): Unit = {
                  if (that == null) return
                  that match {
                    case mux: Multiplexer => {
                      if (mux.whenMux) {
                        if (mux.whenTrue != reg) {
                          val when = context.when.getOrElseUpdate(mux.cond, new WhenTree(mux.cond))
                          walkMux(mux.whenTrue, when.whenTrue)
                        }
                        if (mux.whenFalse != reg) {
                          val when = context.when.getOrElseUpdate(mux.cond, new WhenTree(mux.cond))
                          walkMux(mux.whenFalse, when.whenFalse)
                        }
                      } else {
                        context.logic += new Tuple2(regSignal, that)
                      }
                    }
                    case _ => context.logic += new Tuple2(regSignal, that)
                  }
                }
              }
            }
            case memWrite: MemWrite => {
              ret ++= s"${tab}if ${emitReference(memWrite.getEnable)} = '1' then\n"
              ret ++= s"$tab  ${emitReference(memWrite.getMem)}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)};\n"
              ret ++= s"${tab}end if;\n"
            }
            case memReadSync: MemReadSync => {
              ret ++= s"${tab}if ${emitReference(memReadSync.getEnable)} = '1' then\n"
              ret ++= s"$tab   ${emitReference(memReadSync.consumers(0))} <= ${emitReference(memReadSync.getMem)}(to_integer(${emitReference(memReadSync.getAddress)}));\n"
              ret ++= s"${tab}end if;\n"
            }
          }

          emitContext(rootContext, tab)


          def emitContext(context: Context, tab: String): Unit = {
            for ((to, from) <- context.logic) {
              ret ++= s"$tab${emitReference(to)} <= ${emitLogic(from)};\n"
            }
            for (when <- context.when.values) {
              def doTrue = when.whenTrue.isNotEmpty
              def doFalse = when.whenFalse.isNotEmpty

              if (!doTrue && doFalse) {

                ret ++= s"${tab}if ${emitLogic(when.cond)} = '0'  then\n"
                emitContext(when.whenFalse, tab + "  ")
                ret ++= s"${tab}end if;\n"

              } else if (doTrue && !doFalse) {

                ret ++= s"${tab}if ${emitLogic(when.cond)} = '1' then\n"
                emitContext(when.whenTrue, tab + "  ")
                ret ++= s"${tab}end if;\n"

              } else if (doTrue && doFalse) {

                ret ++= s"${tab}if ${emitLogic(when.cond)} = '1' then\n"
                emitContext(when.whenTrue, tab + "  ")
                ret ++= s"${tab}else\n"
                emitContext(when.whenFalse, tab + "  ")
                ret ++= s"${tab}end if;\n"

              }
            }
          }
        }
      }
    }

    //ret ++= "  -- end synchronous\n"


  }


  def emitComponentInstances(component: Component, ret: StringBuilder): Unit = {
    for (kind <- component.kinds) {
      val isBB = kind.isInstanceOf[BlackBox]
      val definitionString = if (isBB) kind.definitionName else s"entity $library.${kind.definitionName}"
      ret ++= s"  ${kind.getName()} : $definitionString\n"
      if (kind.isInstanceOf[BlackBox]) {
        val bb = kind.asInstanceOf[BlackBox]
        val genericFlat = bb.generic.flatten
        if (genericFlat.size != 0) {
          ret ++= s"    generic map(\n"
          genericFlat.foreach(_._2 match {
            case baseType: BaseType => {
              ret ++= s"      ${emitReference(baseType)} => ${emitLogic(baseType.inputs(0))},\n"
            }
            case _ =>
          })
          ret.setCharAt(ret.size - 2, ' ')
          ret ++= s"    )\n"
        }
      }
      ret ++= s"    port map (\n"
      for (data <- kind.getOrdredNodeIo) {
        if (data.isOutput) {
          val bind = component.kindsOutputsToBindings.getOrElse(data,null)
          if (bind != null) {
            ret ++= s"      ${emitReference(data)} => ${emitReference(bind)},\n"
          }
        }
        else if (data.isInput)
          ret ++= s"      ${emitReference(data)} => ${emitReference(data.inputs(0))},\n"
      }
      ret.setCharAt(ret.size - 2, ' ')

      ret ++= s"    );"
      ret ++= s"\n"
    }
  }
}
