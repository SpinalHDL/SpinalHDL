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
import scala.runtime.RangedProxy
import scala.util.Random

/**
 * Created by PIC18F on 07.01.2015.
 */
class VhdlBackend extends Backend {
  var out: java.io.FileWriter = null
  var library = "work"
  val packageName = "pkg_scala2hdl" // + Random.nextInt(100000)


  val reservedKeyWords = Set[String]("in", "out", "buffer", "inout", "entity", "component", "architecture")

  override def addReservedKeyWord(scope: Scope): Unit = {
    super.addReservedKeyWord(scope)
    reservedKeyWords.foreach(scope.allocateName(_))
  }


  override def elaborate(topLevel: Component): Unit = {
    super.elaborate(topLevel)
    out = new java.io.FileWriter("out.vhd")

    emitPackage(out)

    for (c <- sortedComponents) {
      compile(c)
    }

    out.flush();
    out.close();
  }

  def emitTestBench(topLevel: Component): Unit ={

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


  def emitPackage(out: java.io.FileWriter): Unit = {

    def pkgExtractBool(kind: String): Tuple2[String, String] = {
      (s"function pkg_extract (that : $kind; bitId : integer) return std_logic",
        s"""|  begin
            |    return that(bitId);
            |  end pkg_extract;
            |  """.stripMargin)
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
    ret ++= s"""library IEEE;
              |use IEEE.STD_LOGIC_1164.ALL;
              |use IEEE.NUMERIC_STD.all;
              |
              |package $packageName is
              |${funcs.map("  " + _._1 + ";\n").reduce(_ + _)}
              |
              |  function pkg_mux (sel : std_logic;one : std_logic;zero : std_logic) return std_logic;
              |  function pkg_mux (sel : std_logic;one : std_logic_vector;zero : std_logic_vector) return std_logic_vector;
              |  function pkg_mux (sel : std_logic;one : unsigned;zero : unsigned) return unsigned;
              |  function pkg_mux (sel : std_logic;one : signed;zero : signed) return signed;
              |
              |  function pkg_toStdLogic (value : boolean) return std_logic;
              |  function pkg_toStdLogicVector (value : std_logic) return std_logic_vector;
              |  function pkg_toUnsigned(value : std_logic) return unsigned;
              |  function pkg_toSigned (value : std_logic) return signed;
              |  function pkg_stdLogicVector (lit : std_logic_vector; bitCount : integer) return std_logic_vector;
              |  function pkg_unsigned (lit : unsigned; bitCount : integer) return unsigned;
              |  function pkg_signed (lit : signed; bitCount : integer) return signed;
              |
              |  function pkg_resize (that : std_logic_vector; width : integer) return std_logic_vector;
              |  function pkg_resize (that : unsigned; width : integer) return unsigned;
              |  function pkg_resize (that : signed; width : integer) return signed;
              |
              |  function pkg_extract (that : std_logic_vector; high : integer; low : integer) return std_logic_vector;
              |
              |  function pkg_shiftRight (that : std_logic_vector; size : natural) return std_logic_vector;
              |  function pkg_shiftRight (that : std_logic_vector; size : unsigned) return std_logic_vector;
              |  function pkg_shiftLeft (that : std_logic_vector; size : natural) return std_logic_vector;
              |  function pkg_shiftLeft (that : std_logic_vector; size : unsigned) return std_logic_vector;
              |
              |  function pkg_shiftRight (that : unsigned; size : natural) return unsigned;
              |  function pkg_shiftRight (that : unsigned; size : unsigned) return unsigned;
              |  function pkg_shiftLeft (that : unsigned; size : natural) return unsigned;
              |  function pkg_shiftLeft (that : unsigned; size : unsigned) return unsigned;
              |
              |  function pkg_shiftRight (that : signed; size : natural) return signed;
              |  function pkg_shiftRight (that : signed; size : unsigned) return signed;
              |  function pkg_shiftLeft (that : signed; size : natural) return signed;
              |  function pkg_shiftLeft (that : signed; size : unsigned) return signed;
              |end  $packageName;
              |
              |package body $packageName is
              |${funcs.map(f => "  " + f._1 + " is\n" + f._2 + "\n").reduce(_ + _)}
              |
              |  -- unsigned shifts
              |  function pkg_shiftRight (that : unsigned; size : natural) return unsigned is
              |  begin
              |    if size >= that then
              |      return "";
              |    else
              |      return shift_right(that,size)(that'high-size downto 0);
              |    end if;
              |  end pkg_shiftRight;
              |
              |  function pkg_shiftRight (that : unsigned; size : unsigned) return unsigned is
              |  begin
              |    return shift_right(that,to_integer(size));
              |  end pkg_shiftRight;
              |
              |  function pkg_shiftLeft (that : unsigned; size : natural) return unsigned is
              |  begin
              |    return shift_left(resize(that,that'length + size),size);
              |  end pkg_shiftLeft;
              |
              |  function pkg_shiftLeft (that : unsigned; size : unsigned) return unsigned is
              |  begin
              |    return shift_left(resize(that,that'length + 2**size'length - 1),to_integer(size));
              |  end pkg_shiftLeft;
              |
              |
              |  -- std_logic_vector shifts
              |  function pkg_shiftRight (that : std_logic_vector; size : natural) return std_logic_vector is
              |  begin
              |    return std_logic_vector(pkg_shiftRight(unsigned(that),size));
              |  end pkg_shiftRight;
              |
              |  function pkg_shiftRight (that : std_logic_vector; size : unsigned) return std_logic_vector is
              |  begin
              |    return std_logic_vector(pkg_shiftRight(unsigned(that),size));
              |  end pkg_shiftRight;
              |
              |  function pkg_shiftLeft (that : std_logic_vector; size : natural) return std_logic_vector is
              |  begin
              |    return std_logic_vector(pkg_shiftLeft(unsigned(that),size));
              |  end pkg_shiftLeft;
              |
              |  function pkg_shiftLeft (that : std_logic_vector; size : unsigned) return std_logic_vector is
              |  begin
              |    return std_logic_vector(pkg_shiftLeft(unsigned(that),size));
              |  end pkg_shiftLeft;
              |
              |  -- signed shifts
              |  function pkg_shiftRight (that : signed; size : natural) return signed is
              |  begin
              |    return signed(pkg_shiftRight(unsigned(that),size));
              |  end pkg_shiftRight;
              |
              |  function pkg_shiftRight (that : signed; size : unsigned) return signed is
              |  begin
              |    return signed(pkg_shiftRight(unsigned(that),size));
              |  end pkg_shiftRight;
              |
              |  function pkg_shiftLeft (that : signed; size : natural) return signed is
              |  begin
              |    return signed(pkg_shiftLeft(unsigned(that),size));
              |  end pkg_shiftLeft;
              |
              |  function pkg_shiftLeft (that : signed; size : unsigned) return signed is
              |  begin
              |    return signed(pkg_shiftLeft(unsigned(that),size));
              |  end pkg_shiftLeft;
              |
              |  function pkg_extract (that : std_logic_vector; high : integer; low : integer) return std_logic_vector is
              |  begin
              |    return that(high downto low);
              |  end pkg_extract;
              |
              |  function pkg_mux (sel : std_logic;one : std_logic;zero : std_logic) return std_logic is
              |  begin
              |    if sel = '1' then
              |      return one;
              |    else
              |      return zero;
              |    end if;
              |  end pkg_mux;
              |
              |  function pkg_mux (sel : std_logic;one : std_logic_vector;zero : std_logic_vector) return std_logic_vector is
              |  begin
              |    if sel = '1' then
              |      return one;
              |    else
              |      return zero;
              |    end if;
              |  end pkg_mux;
              |
              |  function pkg_mux (sel : std_logic;one : unsigned;zero : unsigned) return unsigned is
              |  begin
              |    if sel = '1' then
              |      return one;
              |    else
              |      return zero;
              |    end if;
              |  end pkg_mux;
              |
              |  function pkg_mux (sel : std_logic;one : signed;zero : signed) return signed is
              |  begin
              |    if sel = '1' then
              |      return one;
              |    else
              |      return zero;
              |    end if;
              |  end pkg_mux;
              |
              |  function pkg_toStdLogic (value : boolean) return std_logic is
              |  begin
              |    if value = true then
              |      return '1';
              |    else
              |      return '0';
              |    end if;
              |  end pkg_toStdLogic;
              |
              |  function pkg_toStdLogicVector (value : std_logic) return std_logic_vector is
              |    variable ret : std_logic_vector(0 downto 0);
              |  begin
              |    ret(0) := value;
              |    return ret;
              |  end pkg_toStdLogicVector;
              |
              |  function pkg_toUnsigned (value : std_logic) return unsigned is
              |    variable ret : unsigned(0 downto 0);
              |  begin
              |    ret(0) := value;
              |    return ret;
              |  end pkg_toUnsigned;
              |
              |  function pkg_toSigned (value : std_logic) return signed is
              |    variable ret : signed(0 downto 0);
              |  begin
              |    ret(0) := value;
              |    return ret;
              |  end pkg_toSigned;
              |
              |  function pkg_stdLogicVector (lit : std_logic_vector; bitCount : integer) return std_logic_vector is
              |  begin
              |    return pkg_resize(lit,bitCount);
              |  end pkg_stdLogicVector;
              |
              |  function pkg_unsigned (lit : unsigned; bitCount : integer) return unsigned is
              |  begin
              |    return pkg_resize(lit,bitCount);
              |  end pkg_unsigned;
              |
              |  function pkg_signed (lit : signed; bitCount : integer) return signed is
              |  begin
              |    return pkg_resize(lit,bitCount);
              |  end pkg_signed;
              |
              |  function pkg_resize (that : std_logic_vector; width : integer) return std_logic_vector is
              |  begin
              |    return std_logic_vector(resize(unsigned(that),width));
              |  end pkg_resize;
              |
              |  function pkg_resize (that : unsigned; width : integer) return unsigned is
              |  begin
              |    return resize(that,width);
              |  end pkg_resize;
              |
              |  function pkg_resize (that : signed; width : integer) return signed is
              |  begin
              |    return resize(that,width);
              |  end pkg_resize;
              |end $packageName;
              |
              |""".stripMargin
    out.write(ret.result())
  }

  def emitLibrary(component: Component, ret: StringBuilder): Unit = {
    ret ++= s"""library IEEE;
              |use IEEE.STD_LOGIC_1164.ALL;
              |use IEEE.NUMERIC_STD.all;
              |library $library;
              |use $library.$packageName.all;
              | """.stripMargin

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
    ret.setCharAt(ret.size - 2, ' ')
    ret ++= s"  );\n"
    ret ++= s"end ${component.definitionName};\n"
    ret ++= s"\n"

  }


  def emitArchitecture(component: Component, ret: StringBuilder): Unit = {
    ret ++= s"architecture arch of ${component.definitionName} is\n"
    emitBlackBoxComponents(component, ret)
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

  def emitSignal(ref: Node, typeNode: Node): String = {
    s"  signal ${emitReference(ref)} : ${emitDataType(typeNode)};\n"
  }

  def emitSignals(component: Component, ret: StringBuilder): Unit = {
    for (node <- component.nodes) {
      node match {
        case signal: BaseType => {
          if (!signal.isIo)
            ret ++= emitSignal(signal, signal);
        }
        case outBinding: OutBinding => {
          ret ++= emitSignal(outBinding, outBinding.out);
        }
        //        case op: Operator => {
        //          ret ++= emitSignal(op, op);
        //        }
        case _ =>
      }
    }
  }

  def emitAsyncronous(component: Component, ret: StringBuilder): Unit = {
    for (node <- component.nodes) {
      node match {
        case signal: BaseType => {
          if (!signal.isReg)
            if (!signal.isIo || !signal.isInput)
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
        if (opThatNeedBoolCast.contains(vhd))
          return s"pkg_toStdLogic$temp"
        else
          return temp
      }
    }
  }

  def operatorImplAsFunction(vhd: String)(func: Modifier): String = {
    s"$vhd(${func.inputs.map(emitLogic(_)).reduce(_ + "," + _)})"
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
  modifierImplMap.put("!", operatorImplAsOperator("not"))
  modifierImplMap.put("&&", operatorImplAsOperator("and"))
  modifierImplMap.put("||", operatorImplAsOperator("or"))



  //cast
  modifierImplMap.put("s->b", operatorImplAsFunction("std_logic_vector"))
  modifierImplMap.put("u->b", operatorImplAsFunction("std_logic_vector"))
  modifierImplMap.put("B->b", operatorImplAsFunction("pkg_toStdLogicVector"))

  modifierImplMap.put("b->s", operatorImplAsFunction("signed"))
  modifierImplMap.put("u->s", operatorImplAsFunction("signed"))

  modifierImplMap.put("b->u", operatorImplAsFunction("unsigned"))
  modifierImplMap.put("s->u", operatorImplAsFunction("unsigned"))


  //misc
  modifierImplMap.put("resize(s,i)", operatorImplAsFunction("pkg_resize"))
  modifierImplMap.put("resize(u,i)", operatorImplAsFunction("pkg_resize"))
  modifierImplMap.put("resize(b,i)", operatorImplAsFunction("pkg_resize"))

  modifierImplMap.put("mux(B,B,B)", operatorImplAsFunction("pkg_mux"))
  modifierImplMap.put("mux(B,b,b)", operatorImplAsFunction("pkg_mux"))
  modifierImplMap.put("mux(B,u,u)", operatorImplAsFunction("pkg_mux"))
  modifierImplMap.put("mux(B,s,s)", operatorImplAsFunction("pkg_mux"))


  val opThatNeedBoolCast = mutable.Set[String]("==", "!=", "<", "<=")


  def emitLogic(node: Node): String = node match {
    case baseType: BaseType => emitReference(baseType)
    case outBinding: OutBinding => emitReference(outBinding)
    case lit: BitsLiteral => lit.kind match {
      case _: Bits => s"pkg_stdLogicVector(X${'\"'}${lit.value.toString(16)}${'\"'},${lit.getWidth})"
      case _: UInt => s"pkg_unsigned(X${'\"'}${lit.value.toString(16)}${'\"'},${lit.getWidth})"
      case _: SInt => s"pkg_signed(X${'\"'}${lit.value.toString(16)}${'\"'},${lit.getWidth})"
    }
    case lit: IntLiteral => lit.value.toString(10)
    case lit: SStringLiteral => "\"" + lit.value + "\""
    case lit: BoolLiteral => "\'" + (if (lit.value) "1" else "0") + "\'"
    case node: Modifier => modifierImplMap.getOrElse(node.opName, throw new Exception("can't find " + node.opName))(node)
    case node: ExtractBool => {
      val bitIdString = node.bitId match {
        case bitId: IntLiteral => emitLogic(bitId)
        case bitId: UInt => s"to_integer(${emitLogic(bitId)})"
      }
      s"pkg_extract(${emitLogic(node.bitVector)},$bitIdString)"
    }
    case node: ExtractBitsVector => s"pkg_extract(${emitLogic(node.bitVector)},${node.bitIdHi.value},${node.bitIdLo.value})"
    case _ => throw new Exception("Don't know how emit that logic")
  }

  def emitSyncronous(component: Component, ret: StringBuilder): Unit = {
    ret ++= "  -- synchronous\n"

    val regSignals = component.getRegs

    val clockDomainMap = mutable.Map[ClockDomain, ArrayBuffer[BaseType]]()

    for (regSignal <- regSignals) {
      clockDomainMap.getOrElseUpdate(regSignal.inputs(0).asInstanceOf[Reg].clockDomain, new ArrayBuffer[BaseType]()) += regSignal
    }

    for ((clockDomain, array) <- clockDomainMap) {
      val arrayWithReset = ArrayBuffer[BaseType]()
      val arrayWithoutReset = ArrayBuffer[BaseType]()
      array.foreach(regSignal => if (regSignal.inputs(0).inputs(1) != regSignal.inputs(0)) arrayWithReset += regSignal else arrayWithoutReset += regSignal)

      emitClockDomain(true)
      emitClockDomain(false)



      def emitClockDomain(withReset: Boolean): Unit = {
        val activeArray = if (withReset) arrayWithReset else arrayWithoutReset
        if (activeArray.size == 0) return;
        val clock = component.pulledDataCache.getOrElse(clockDomain.clock, throw new Exception("???")).asInstanceOf[Bool]
        val reset = if (clockDomain.reset == null || !withReset) null else component.pulledDataCache.getOrElse(clockDomain.reset, throw new Exception("???")).asInstanceOf[Bool]
        val clockEnable = if (clockDomain.clockEnable == null) null else component.pulledDataCache.getOrElse(clockDomain.clockEnable, throw new Exception("???")).asInstanceOf[Bool]
        val asyncReset = reset != null && clockDomain.resetKind == ASYNC
        val syncReset = reset != null && clockDomain.resetKind == SYNC
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
          for (regSignal <- activeArray) {
            val reg = regSignal.inputs(0).asInstanceOf[Reg]
            if (reg.hasInitialValue) {
              ret ++= s"$tab${emitReference(regSignal)} <= ${emitLogic(reg.getInitialValue)};\n"
            }
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

          for (regSignal <- activeArray) {
            if (!regSignal.isIo || !regSignal.isInput) {
              val in = regSignal.inputs(0).inputs(0)
              val reg = regSignal.inputs(0)
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

    ret ++= "  -- end synchronous\n"


  }

  def emitClockEdge(clock: Bool, edgeKind: EdgeKind): String = {
    s"${
      edgeKind match {
        case RISING => "rising_edge"
        case FALLING => "falling_edge"
      }
    }(${emitReference(clock)}) then\n"
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
      for (data <- kind.getNodeIo) {
        if (data.isOutput) {
          val bind = component.findBinding(data)
          if (bind != null)
            ret ++= s"      ${emitReference(data)} => ${emitReference(component.findBinding(data))},\n"
          // else
          //   ret ++= s"    --${emitReference(data)} => ,\n"

        }
        else if (data.isInput)
          ret ++= s"      ${emitReference(data)} => ${emitReference(data.inputs(0))},\n"
      }
      ret.setCharAt(ret.size - 2, ' ')

      ret ++= s"    );"
      ret ++= s"\n"
    }


  }

  def emitDataType(node: Node, constrained: Boolean = true) = node match {
    case bool: Bool => "std_logic"
    case uint: UInt => s"unsigned${if (constrained) emitRange(uint) else ""}"
    case sint: SInt => s"signed${if (constrained) emitRange(sint) else ""}"
    case bits: Bits => s"std_logic_vector${if (constrained) emitRange(bits) else ""}"
    case number: Number => s"integer"
    case string: SString => s"string"
    case _ => throw new Exception("Unknown datatype"); ""
  }

  def emitDirection(baseType: BaseType) = baseType.dir match {
    case spinal.in => "in"
    case spinal.out => "out"
    case _ => throw new Exception("Unknown direction"); ""
  }


  def emitRange(node: Node) = s"(${node.getWidth - 1} downto 0)"

}
