package spinal.core

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap, StringBuilder}
import scala.util.Random

/**
 * Created by PIC32F_USER on 05/06/2016.
 */



class PhaseVerilog(pc : PhaseContext) extends Phase with VerilogBase {
  import pc._
  var outFile: java.io.FileWriter = null
  var memBitsMaskKind : MemBitsMaskKind = MULTIPLE_RAM

  val emitedComponent = mutable.Map[ComponentBuilder, ComponentBuilder]()
  val emitedComponentRef = mutable.Map[Component, Component]()


  override def impl(): Unit = {
    import pc._
    SpinalInfoPhase("Write Verilog")
    
    outFile = new java.io.FileWriter(pc.config.targetDirectory + "/" +  topLevel.definitionName + ".v")
    // TODO emitEnumPackage(outFile)

    for (c <- sortedComponents) {
      SpinalInfoPhase(s"${"  " * (1 + c.level)}emit ${c.definitionName}")
      compile(c)
    }

    outFile.flush();
    outFile.close();
  }



  def compile(component: Component): Unit = {
    val text = emit(component)
    outFile.write(text)
  }


  def emit(component: Component): String = {
    val ret = new StringBuilder()
    val builder = new ComponentBuilder(component)

    emitModuleIo(component, builder)
    emitModuleContent(component, builder)

    val oldBuilder = emitedComponent.getOrElse(builder, null)
    if (oldBuilder == null) {
      emitedComponent += (builder -> builder)
      return builder.result
    } else {
      emitedComponentRef += (component -> oldBuilder.component)
      return s"\n--${component.definitionName} remplaced by ${oldBuilder.component.definitionName}\n\n"
    }
  }

  def getReEncodingFuntion(spinalEnum: SpinalEnum, source: SpinalEnumEncoding, target: SpinalEnumEncoding): String = {
    s"${spinalEnum.getName()}_${source.getName()}_to_${target.getName()}"
  }

  def getEnumToDebugFuntion(spinalEnum: SpinalEnum, source: SpinalEnumEncoding): String = {
    assert(!source.isNative)
    s"${spinalEnum.getName()}_${source.getName()}_to_debug"
  }

  def getEnumDebugType(spinalEnum: SpinalEnum): String = {
    s"${spinalEnum.getName()}_debug"
  }



  def emitModuleIo(component: Component, builder: ComponentBuilder): Unit = {
    var ret = builder.newPart(false)
    ret ++= s"module ${component.definitionName}\n"
    ret = builder.newPart(true)
    ret ++= s"(\n"
    component.getOrdredNodeIo.foreach(baseType => {
      ret ++= s"  ${emitDirection(baseType)} ${if(signalNeedProcess(baseType)) "reg " else ""}${emitDataType(baseType)} ${baseType.getName()},\n"
    })

    ret.setCharAt(ret.size - 2, ' ')
    ret ++= s");\n"
    ret = builder.newPart(false)
    ret ++= s"\n"
  }


  def emitModuleContent(component: Component, builder: ComponentBuilder): Unit = {
    var ret = builder.newPart(true)
    val enumDebugSignals = ArrayBuffer[SpinalEnumCraft[_]]()
    emitSignals(component, ret, enumDebugSignals)
    val retTemp = new StringBuilder
    emitComponentInstances(component, retTemp)
    emitAsyncronous(component, retTemp, ret)
    emitSyncronous(component, retTemp)
    //TODO emitDebug(component, retTemp, enumDebugSignals)
    retTemp ++= s"endmodule\n"
    retTemp ++= s"\n"


    ret ++= retTemp

  }


  def toSpinalEnumCraft[T <: SpinalEnum](that: Any) = that.asInstanceOf[SpinalEnumCraft[T]]

  def emitSignals(component: Component, ret: StringBuilder, enumDebugSignals: ArrayBuffer[SpinalEnumCraft[_]]): Unit = {
    for (node <- component.nodes) {
      node match {
        case signal: BaseType => {
          if (!signal.isIo) {
            ret ++= s"  ${if(signalNeedProcess(signal)) "reg " else "wire "}${emitDataType(signal)} ${emitReference(signal)}"
            val reg = if(signal.isReg) signal.input.asInstanceOf[Reg] else null
            if(reg != null && reg.initialValue != null && reg.getClockDomain.config.resetKind == BOOT) {
              ret ++= " = " + (reg.initialValue match {
                case init : BaseType => emitLogic(init.getLiteral)
                case init =>  emitLogic(init)
              })
            }else if (signal.hasTag(randomBoot)) {
              signal match {
                case b: Bool => ret ++= " = " + {
                  if (Random.nextBoolean()) "'1'" else "'0'"
                }
                case bv: BitVector => {
                  val rand = BigInt(bv.getWidth, Random).toString(2)
                  ret ++= " := " + bv.getWidth + "’b" + "0" * (bv.getWidth - rand.length) + rand
                }
                case e: SpinalEnumCraft[_] => {
                  val vec = e.blueprint.values.toVector
                  val rand = vec(Random.nextInt(vec.size))
                  ret ++= " = " + emitEnumLiteral(rand, e.encoding)
                }
              }
            }
            ret ++= ";\n"
            if (signal.isInstanceOf[SpinalEnumCraft[_]]) {
              val craft = toSpinalEnumCraft(signal)
              if (!craft.encoding.isNative) {
                //TODO
                // ret ++= s"  ${emitReference(signal)}_debug : ${getEnumDebugType(craft.blueprint)};\n"
                //enumDebugSignals += toSpinalEnumCraft(signal)
              }
            }
          }


          emitAttributes(signal, "signal", ret)
        }

        case mem: Mem[_] => {
          //ret ++= emitSignal(mem, mem);
          var initAssignementBuilder = new StringBuilder()
          if (mem.initialContent != null) {
            ??? //TODO
            initAssignementBuilder ++= " := ("
            val values = mem.initialContent.map(e => {
              e.hashCode()
            })

            var first = true
            for ((e, index) <- mem.initialContent.zipWithIndex) {
              if (!first)
                initAssignementBuilder ++= ","
              else
                first = false

              if ((index & 15) == 0) {
                initAssignementBuilder ++= "\n     "
              }

              val values = (e.flatten, mem._widths).zipped.map((e, width) => {
                e.getLiteral.getBitsStringOn(width)
              })

              initAssignementBuilder ++= "\"" + values.reduceLeft((l, r) => r + l) + "\""
            }

            initAssignementBuilder ++= ")"
          }

          val symbolWidth = mem.getMemSymbolWidth()
          val symbolCount = mem.getMemSymbolCount
          //TODO
          if(memBitsMaskKind == MULTIPLE_RAM && symbolCount != 1) {
            if(mem.initialContent != null) SpinalError("Memory with multiple symbol per line + initial contant are not suported currently")

            ret ++= s"  type ${emitReference(mem)}_type is array (0 to ${mem.wordCount - 1}) of std_logic_vector(${symbolWidth - 1} downto 0);\n"
            for(i <- 0 until symbolCount) {
              val postfix = "_symbol" + i
              ret ++= s"  signal ${emitReference(mem)}$postfix : ${emitDataType(mem)};\n"
              emitAttributes(mem, "signal", ret,postfix = postfix)
            }
          }else{
            ret ++= s"  type ${emitReference(mem)}_type is array (0 to ${mem.wordCount - 1}) of std_logic_vector(${mem.getWidth - 1} downto 0);\n"
            ret ++= s"  signal ${emitReference(mem)} : ${emitDataType(mem)}${initAssignementBuilder.toString()};\n"
            emitAttributes(mem, "signal", ret)
          }
        }
        case _ =>
      }


    }
  }



  def emitAttributes(node: Node, vhdlType: String, ret: StringBuilder,postfix : String = ""): Unit = {
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

  //TODO ? Not true for literal driven signals with multiple assignement
  def signalNeedProcess(baseType: BaseType) : Boolean = {
    if(baseType.isReg) return true
    if(baseType.input.isInstanceOf[MultipleAssignmentNode] || baseType.input.isInstanceOf[WhenNode]) return true
    return false
  }

  def emitAsyncronous(component: Component, ret: StringBuilder, funcRet: StringBuilder): Unit = {
    val processList = getAsyncProcesses(component)

    for (process <- processList if !process.needProcessDef) {
      for (node <- process.nodes) {
        emitAssignement(node, node.getInput(0), ret, "  assign ", "=")
      }
    }

    for (process <- processList if process.needProcessDef) {
      process.genSensitivity

      val context = new AssignementLevel
      for (node <- process.nodes) {
        context.walkWhenTree(node, node.getInput(0))
      }

      if (process.sensitivity.size != 0) {

        ret ++= s"  always @ (${process.sensitivity.toList.sortWith(_.instanceCounter < _.instanceCounter).map(emitReference(_)).reduceLeft(_ + "," + _)})\n"
        ret ++= "  begin\n"
        emitAssignementLevel(context,ret, "    ", "<=")
        ret ++= "  end\n\n"
      } else {
        //emit func as logic
        ??? //TODO
//        assert(process.nodes.size == 1)
//        for (node <- process.nodes) {
//          val funcName = "zz_" + emitReference(node)
//          funcRet ++= emitFuncDef(funcName, node, context)
//          ret ++= s"  ${emitReference(node)} <= ${funcName};\n"
//          //          ret ++= s"  ${emitReference(node)} <= ${emitLogic(node.getInput(0))};\n"
//        }
      }
    }

  }


  def emitFuncDef(funcName: String, node: Node, context: AssignementLevel): StringBuilder = {
    ??? //TODO
//    val ret = new StringBuilder
//    //context.emitContext(ret, "    ",":=")
//    ret ++= s"  function $funcName return ${emitDataType(node, false)} is\n"
//    ret ++= s"    variable ${emitReference(node)} : ${emitDataType(node, true)};\n"
//    ret ++= s"  begin\n"
//    emitAssignementLevel(context,ret, "    ", ":=")
//    ret ++= s"    return ${emitReference(node)};\n"
//    ret ++= s"  end function;\n"
  }


  def operatorImplAsBinaryOperator(vhd: String)(op: Modifier): String = s"(${emitLogic(op.getInput(0))} $vhd ${emitLogic(op.getInput(1))})"


  def operatorImplAsUnaryOperator(vhd: String)(op: Modifier): String = {
    s"($vhd ${emitLogic(op.getInput(0))})"
  }

  def operatorImplAsFunction(vhd: String)(func: Modifier): String = {
    s"$vhd(${func.getInputs.map(emitLogic(_)).reduce(_ + "," + _)})"
  }

  def operatorImplAsMux(func: Modifier): String = {
    val mux = func.asInstanceOf[Multiplexer]
    s"(${emitLogic(mux.cond)} ? ${emitLogic(mux.whenTrue)} : ${emitLogic(mux.whenFalse)})"
  }



  def operatorImplAsNoTransformation(func: Modifier): String = {
    emitLogic(func.getInput(0))
  }

  def shiftRightByIntImpl(func: Modifier): String = {
    val node = func.asInstanceOf[Operator.BitVector.ShiftRightByInt]
    s"(${emitLogic(node.input)} >>> ${node.shift})"
  }

  def shiftLeftByIntImpl(func: Modifier): String = {
    val node = func.asInstanceOf[Operator.BitVector.ShiftLeftByInt]
    s"(${emitLogic(node.input)} <<< ${node.shift})"
  }

  def operatorImplAsCat(op : Modifier) : String = {
    val cat = op.asInstanceOf[Operator.Bits.Cat]
    s"{${cat.left},${cat.right}}"
  }

  //TODO
  def resizeFunction(vhdlFunc : String)(func: Modifier): String = {
    val resize = func.asInstanceOf[Resize]
    func.getInput(0).getWidth match{
      case 0 => {
        func.getInput(0) match { //TODO
          /*case lit: BitsLiteral => {
            val bitString =  '"' + "0"AFAFAF * func.getWidth + '"'
            lit.kind match {
              case _: Bits => s"pkg_stdLogicVector($bitString)"
              case _: UInt => s"pkg_unsigned($bitString)"
              case _: SInt => s"pkg_signed($bitString)"
            }
            s"(${lit.getWidth}'b${lit.getBitsStringOn(lit.getWidth)})"
          }*/
          case _ => s"pkg_resize(${emitLogic(resize.input)},${resize.size})"
        }
      }
      case _ => s"pkg_resize(${emitLogic(resize.input)},${resize.size})"
    }
  }


  def enumEgualsImpl(eguals: Boolean)(op: Modifier): String = {
    val (enumDef, encoding) = op.getInput(0) match {
      case craft: SpinalEnumCraft[_] => (craft.blueprint, craft.encoding)
      case literal: EnumLiteral[_] => (literal.enum.parent, literal.encoding)
    }
    encoding match {
      case `oneHot` => s"pkg_toStdLogic((${emitLogic(op.getInput(0))} and ${emitLogic(op.getInput(1))}) ${if (eguals) "/=" else "="} ${'"' + "0" * op.getInput(0).getWidth + '"'})"
      case _ => s"pkg_toStdLogic(${emitLogic(op.getInput(0))} ${if (eguals) "=" else "/="} ${emitLogic(op.getInput(1))})"
    }
  }


  def operatorImplAsBitsToEnum(func: Modifier): String = {
    val (enumDef: SpinalEnum, encoding) = func.asInstanceOf[CastBitsToEnum].enum match {
      case craft: SpinalEnumCraft[_] => (craft.blueprint, craft.encoding)
    }
    if (!encoding.isNative) {
      emitLogic(func.getInput(0))
    } else {
      s"pkg_to${enumDef.getName()}_${encoding.getName()}(${(emitLogic(func.getInput(0)))})"
    }
  }

  def operatorImplAsEnumToBits(func: Modifier): String = {
    val (enumDef, encoding) = func.getInput(0) match {
      case craft: SpinalEnumCraft[_] => (craft.blueprint, craft.encoding)
      case literal: EnumLiteral[_] => (literal.enum.parent, literal.encoding)
    }
    if (!encoding.isNative) {
      emitLogic(func.getInput(0))
    } else {
      s"pkg_toStdLogicVector_${encoding.getName()}(${(emitLogic(func.getInput(0)))})"
    }
  }

  def operatorImplAsEnumToEnum(func: Modifier): String = {
    val (enumDefSrc, encodingSrc) = func.getInput(0) match {
      case craft: SpinalEnumCraft[_] => (craft.blueprint, craft.encoding)
      case literal: EnumLiteral[_] => (literal.enum.parent, literal.encoding)
    }
    val enumCast = func.asInstanceOf[CastBitsToEnum]
    val (enumDefDst, encodingDst) = enumCast.enum match {
      case craft: SpinalEnumCraft[_] => (craft.blueprint, craft.encoding)
    }
    if (encodingDst.isNative && encodingSrc.isNative)
      emitLogic(func.getInput(0))
    else {
      val encoding = enumCast.getInput(0) match {
        case input: SpinalEnumCraft[_] => input.encoding
        case input: EnumLiteral[_] => input.encoding
      }
      s"${getReEncodingFuntion(enumCast.enum.blueprint.asInstanceOf[SpinalEnum], encoding, enumCast.enum.encoding)}(${func.getInputs.map(emitLogic(_)).reduce(_ + "," + _)})"
    }
  }

  val modifierImplMap = mutable.Map[String, Modifier => String]()


  //unsigned
  modifierImplMap.put("u+u", operatorImplAsBinaryOperator("+"))
  modifierImplMap.put("u-u", operatorImplAsBinaryOperator("-"))
  modifierImplMap.put("u*u", operatorImplAsBinaryOperator("*"))
  modifierImplMap.put("u/u", operatorImplAsBinaryOperator("/"))
  modifierImplMap.put("u%u", operatorImplAsBinaryOperator("%"))

  modifierImplMap.put("u|u", operatorImplAsBinaryOperator("|"))
  modifierImplMap.put("u&u", operatorImplAsBinaryOperator("&"))
  modifierImplMap.put("u^u", operatorImplAsBinaryOperator("^"))
  modifierImplMap.put("~u",  operatorImplAsUnaryOperator("~"))

  modifierImplMap.put("u==u", operatorImplAsBinaryOperator("=="))
  modifierImplMap.put("u!=u", operatorImplAsBinaryOperator("!="))
  modifierImplMap.put("u<u",  operatorImplAsBinaryOperator("<"))
  modifierImplMap.put("u<=u", operatorImplAsBinaryOperator("<="))


  modifierImplMap.put("u>>i", shiftRightByIntImpl)
  modifierImplMap.put("u<<i", shiftLeftByIntImpl)
  modifierImplMap.put("u>>u", operatorImplAsBinaryOperator(">>>"))
  modifierImplMap.put("u<<u", operatorImplAsBinaryOperator("<<<"))


  //signed
  modifierImplMap.put("s+s", operatorImplAsBinaryOperator("+"))
  modifierImplMap.put("s-s", operatorImplAsBinaryOperator("-"))
  modifierImplMap.put("s*s", operatorImplAsBinaryOperator("*"))
  modifierImplMap.put("s/s", operatorImplAsBinaryOperator("/"))
  modifierImplMap.put("s%s", operatorImplAsBinaryOperator("%"))

  modifierImplMap.put("s|s", operatorImplAsBinaryOperator("|"))
  modifierImplMap.put("s&s", operatorImplAsBinaryOperator("&"))
  modifierImplMap.put("s^s", operatorImplAsBinaryOperator("^"))
  modifierImplMap.put("~s", operatorImplAsUnaryOperator("~"))
  modifierImplMap.put("-s", operatorImplAsUnaryOperator("-"))

  modifierImplMap.put("s==s", operatorImplAsBinaryOperator("="))
  modifierImplMap.put("s!=s", operatorImplAsBinaryOperator("/="))
  modifierImplMap.put("s<s", operatorImplAsBinaryOperator("<"))
  modifierImplMap.put("s<=s", operatorImplAsBinaryOperator("<="))


  modifierImplMap.put("s>>i", shiftRightByIntImpl)
  modifierImplMap.put("s<<i", shiftLeftByIntImpl)
  modifierImplMap.put("s>>u", operatorImplAsBinaryOperator(">>>"))
  modifierImplMap.put("s<<u", operatorImplAsBinaryOperator("<<<"))



  //bits
  modifierImplMap.put("b##b", operatorImplAsCat)

  modifierImplMap.put("b|b", operatorImplAsBinaryOperator("|"))
  modifierImplMap.put("b&b", operatorImplAsBinaryOperator("&"))
  modifierImplMap.put("b^b", operatorImplAsBinaryOperator("^"))
  modifierImplMap.put("~b",  operatorImplAsUnaryOperator("~"))

  modifierImplMap.put("b==b", operatorImplAsBinaryOperator("=="))
  modifierImplMap.put("b!=b", operatorImplAsBinaryOperator("!="))

  modifierImplMap.put("b>>i",  shiftRightByIntImpl)
  modifierImplMap.put("b<<i",  shiftLeftByIntImpl)
  modifierImplMap.put("b>>u",  operatorImplAsBinaryOperator(">>>"))
  modifierImplMap.put("b<<u",  operatorImplAsBinaryOperator("<<<"))
  modifierImplMap.put("brotlu", operatorImplAsFunction("pkg_rotateLeft")) //TODO



  //bool
  modifierImplMap.put("B==B", operatorImplAsBinaryOperator("=="))
  modifierImplMap.put("B!=B", operatorImplAsBinaryOperator("!="))


  modifierImplMap.put("!", operatorImplAsUnaryOperator("!"))
  modifierImplMap.put("&&", operatorImplAsBinaryOperator("&&"))
  modifierImplMap.put("||", operatorImplAsBinaryOperator("||"))
  modifierImplMap.put("B^B", operatorImplAsBinaryOperator("^"))


  //enum
  modifierImplMap.put("e==e", enumEgualsImpl(true))
  modifierImplMap.put("e!=e", enumEgualsImpl(false))

  //cast
  modifierImplMap.put("s->b", operatorImplAsNoTransformation)
  modifierImplMap.put("u->b", operatorImplAsNoTransformation)
  modifierImplMap.put("B->b", operatorImplAsNoTransformation)
  modifierImplMap.put("e->b", operatorImplAsEnumToBits)

  modifierImplMap.put("b->s", operatorImplAsNoTransformation)
  modifierImplMap.put("u->s", operatorImplAsNoTransformation)

  modifierImplMap.put("b->u", operatorImplAsNoTransformation)
  modifierImplMap.put("s->u", operatorImplAsNoTransformation)

  modifierImplMap.put("b->e", operatorImplAsBitsToEnum)
  modifierImplMap.put("e->e", operatorImplAsEnumToEnum)


  //misc

  modifierImplMap.put("resize(s,i)", operatorImplAsNoTransformation)
  modifierImplMap.put("resize(u,i)", operatorImplAsNoTransformation)
  modifierImplMap.put("resize(b,i)", operatorImplAsNoTransformation)

  //Memo whenNode hardcode emitlogic
  modifierImplMap.put("mux(B,B,B)", operatorImplAsMux)
  modifierImplMap.put("mux(B,b,b)", operatorImplAsMux)
  modifierImplMap.put("mux(B,u,u)", operatorImplAsMux)
  modifierImplMap.put("mux(B,s,s)", operatorImplAsMux)
  modifierImplMap.put("mux(B,e,e)", operatorImplAsMux)

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
    s"${emitLogic(that.getBitVector)}[${that.getBitId}]"
  }

  def extractBoolFloating(func: Modifier): String = {
    val that = func.asInstanceOf[ExtractBoolFloating]
    s"pkg_extract(${emitLogic(that.getBitVector)}[${emitLogic(that.getBitId)}]"
  }

  def extractBitVectorFixed(func: Modifier): String = {
    val that = func.asInstanceOf[ExtractBitsVectorFixed]
    s"${emitLogic(that.getBitVector)}[${that.getHi},${that.getLo}]"
  }

  def extractBitVectorFloating(func: Modifier): String = {
    val that = func.asInstanceOf[ExtractBitsVectorFloating]
    s"${emitLogic(that.getBitVector)}[${emitLogic(that.getOffset)} +: ${that.getBitCount}]"
  }



  def emitLogic(node: Node): String = node match {
    case baseType: BaseType => emitReference(baseType)
    case node: Modifier => modifierImplMap.getOrElse(node.opName, throw new Exception("can't find " + node.opName))(node)

    case lit: BitsLiteral => lit.kind match {
      case _: Bits => s"(${lit.getWidth}'b${lit.getBitsStringOn(lit.getWidth)})"
      case _: UInt => '$' + s"unsigned(${lit.getWidth}'b${lit.getBitsStringOn(lit.getWidth)})"
      case _: SInt => '$' + s"signed(${lit.getWidth}'b${lit.getBitsStringOn(lit.getWidth)})"
    }
    case lit: BitsAllToLiteral => lit.theConsumer match {
      case _: Bits => s"(${lit.getWidth}'b${lit.getBitsStringOn(lit.getWidth)})"
      case _: UInt => '$' + s"unsigned(${lit.getWidth}'b${lit.getBitsStringOn(lit.getWidth)})"
      case _: SInt => '$' + s"signed(${lit.getWidth}'b${lit.getBitsStringOn(lit.getWidth)})"
    }
    case lit: BoolLiteral => if(lit.value) "1" else "0"
    case lit: EnumLiteral[_] => emitEnumLiteral(lit.enum, lit.encoding)
    case memRead: MemReadAsync => {
      if (memRead.writeToReadKind == dontCare) SpinalWarning(s"memReadAsync with dontCare is as writeFirst into VHDL")
      val symbolCount = memRead.getMem.getMemSymbolCount
      if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
        s"${emitReference(memRead.getMem)}[${emitReference(memRead.getAddress)}]"
      else
        "{" + (0 until symbolCount).reverse.map(i => (s"${emitReference(memRead.getMem)}_symbol$i[${emitReference(memRead.getAddress)}]")).reduce(_ + " , " + _) + "}"
    }
    case whenNode: WhenNode => s"(${emitLogic(whenNode.cond)} ? ${emitLogic(whenNode.whenTrue)} : ${emitLogic(whenNode.whenFalse)})" //Exeptional case with asyncrouns of literal
    case dc: DontCareNode => {
      dc.getBaseType match {
        case to: Bool => "1'bx"
        case to: BitVector => s"(${to.getWidth}'b${"x" * dc.getWidth})"
      }
    }

    case o => throw new Exception("Don't know how emit logic of " + o.getClass.getSimpleName)
  }

  def emitDebug(component: Component, ret: StringBuilder, enumDebugSignals: ArrayBuffer[SpinalEnumCraft[_]]): Unit = {
    for (signal <- enumDebugSignals) {
      ret ++= s"  ${emitReference(signal)}_debug <= ${getEnumToDebugFuntion(toSpinalEnumCraft(signal).blueprint, signal.encoding)}(${emitReference(signal)});\n"
    }
  }

  def emitSyncronous(component: Component, ret: StringBuilder): Unit = {
    val syncNodes = component.getDelays

    val clockDomainMap = mutable.Map[ClockDomain, ArrayBuffer[SyncNode]]()

    for (syncNode <- syncNodes) {
      clockDomainMap.getOrElseUpdate(syncNode.getClockDomain, new ArrayBuffer[SyncNode]()) += syncNode
    }

    for ((clockDomain, array) <- clockDomainMap) {
      val arrayWithReset = ArrayBuffer[SyncNode]()
      val arrayWithoutReset = ArrayBuffer[SyncNode]()

      for (syncNode <- array) {
        if (syncNode.isUsingResetSignal) arrayWithReset += syncNode else arrayWithoutReset += syncNode
      }

      emitClockDomain(true)
      emitClockDomain(false)



      def emitClockDomain(withReset: Boolean): Unit = {
        val activeArray = if (withReset) arrayWithReset else arrayWithoutReset
        if (activeArray.size == 0) return;
        val clock = component.pulledDataCache.getOrElse(clockDomain.clock, throw new Exception("???")).asInstanceOf[Bool]
        val reset = if (null == clockDomain.reset || !withReset) null else component.pulledDataCache.getOrElse(clockDomain.reset, throw new Exception("???")).asInstanceOf[Bool]
        val clockEnable = if (null == clockDomain.clockEnable) null else component.pulledDataCache.getOrElse(clockDomain.clockEnable, throw new Exception("???")).asInstanceOf[Bool]
        val asyncReset = (null != reset) && clockDomain.config.resetKind == ASYNC
        val syncReset = (null != reset) && clockDomain.config.resetKind == SYNC
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
          ret ++= s"${tabStr}always @ (${emitClockEdge(clock,clockDomain.config.clockEdge)} or ${emitResetEdge(reset,clockDomain.config.resetActiveLevel)}${if(sensitivity.isEmpty) "" else sensitivity.foldLeft(",")(_ + "," + emitReference(_))})\n"
        } else {
          ret ++= s"${tabStr}always @ (${emitClockEdge(clock,clockDomain.config.clockEdge)})\n"
        }

        ret ++= s"${tabStr}begin\n"
        inc
        if (asyncReset) {
          ret ++= s"${tabStr}if (${if (clockDomain.config.resetActiveLevel == HIGH) "" else "!"}${emitReference(reset)}) begin\n";
          inc
          emitRegsInitialValue(initialValueAssignement, tabStr)
          dec
          ret ++= s"${tabStr}end else begin\n"
          inc
        }
        if (clockEnable != null) {
          ret ++= s"${tabStr}if(${if (clockDomain.config.clockEnableActiveLevel == HIGH) "" else "!"}${emitReference(clockEnable)}) begin\n"
          inc
        }
        if (syncReset) {
          ret ++= s"${tabStr}if(${if (clockDomain.config.resetActiveLevel == HIGH) "" else "!"}${emitReference(reset)}) begin\n"
          inc
          emitRegsInitialValue(initialValueAssignement, tabStr)
          dec
          ret ++= s"${tabStr}else\n"
          inc
          emitRegsLogic(tabStr)
          dec
          ret ++= s"${tabStr}end\n"
          dec
        } else {
          emitRegsLogic(tabStr)
          dec
        }

        while (tabLevel != 1) {
          ret ++= s"${tabStr}end\n"
          dec
        }
        ret ++= s"${tabStr}end\n"
        dec
        ret ++= s"${tabStr}\n"


        def emitRegsInitialValue(assignementLevel: AssignementLevel, tab: String): Unit = {
          emitAssignementLevel(assignementLevel,ret, tab, "<=")
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

              def emitWrite(tab: String) = {
                val symbolCount = memWrite.getMem.getMemSymbolCount
                val bitPerSymbole = memWrite.getMem.getMemSymbolWidth()

                if(memWrite.getMask == null) {
                  if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
                    ret ++= s"$tab${emitReference(memWrite.getMem)}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)};\n"
                  else
                    for(i <- 0 until symbolCount) {
                      val range = s"(${(i + 1) * bitPerSymbole - 1} downto ${i * bitPerSymbole})"
                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}_symbol${i}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)}$range;\n"
                    }
                }else{

                  val maskCount = memWrite.getMask.getWidth
                  for(i <- 0 until maskCount){
                    val range = s"(${(i+1)*bitPerSymbole-1} downto ${i*bitPerSymbole})"
                    ret ++= s"${tab}if ${emitReference(memWrite.getMask)}($i) = '1' then\n"
                    if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}(to_integer(${emitReference(memWrite.getAddress)}))$range <= ${emitReference(memWrite.getData)}$range;\n"
                    else
                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}_symbol${i}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)}$range;\n"

                    ret ++= s"${tab}end if;\n"
                  }
                }
              }
            }
            case memReadSync: MemReadSync => {
              //readFirst
              if (memReadSync.writeToReadKind == writeFirst) SpinalError(s"Can't translate a memReadSync with writeFirst into VHDL $memReadSync")
              if (memReadSync.writeToReadKind == dontCare) SpinalWarning(s"memReadSync with dontCare is as readFirst into VHDL $memReadSync")
              if (memReadSync.useReadEnable) {
                ret ++= s"${tab}if ${emitReference(memReadSync.getReadEnable)} = '1' then\n"
                emitRead(tab + "  ")
                ret ++= s"${tab}end if;\n"
              } else {
                emitRead(tab)
              }
              def emitRamRead() = {
                val symbolCount = memReadSync.getMem.getMemSymbolCount
                if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
                  s"${emitReference(memReadSync.getMem)}(to_integer(${emitReference(memReadSync.getAddress)}))"
                else
                  (0 until symbolCount).reverse.map(i => (s"${emitReference(memReadSync.getMem)}_symbol$i(to_integer(${emitReference(memReadSync.getAddress)}))")).reduce(_ + " & " + _)
              }
              def emitRead(tab: String) = ret ++= s"$tab${emitReference(memReadSync.consumers(0))} <= ${emitRamRead()};\n"

            }

            case memWrite: MemWriteOrRead_writePart => {
              val memReadSync = memWrite.readPart
              if (memReadSync.writeToReadKind == writeFirst) SpinalError(s"Can't translate a MemWriteOrRead with writeFirst into VHDL $memReadSync")
              if (memReadSync.writeToReadKind == dontCare) SpinalWarning(s"MemWriteOrRead with dontCare is as readFirst into VHDL $memReadSync")

              ret ++= s"${tab}if ${emitReference(memWrite.getChipSelect)} = '1' then\n"
              ret ++= s"${tab}  if ${emitReference(memWrite.getWriteEnable)} = '1' then\n"
              emitWrite(tab + "    ")
              ret ++= s"${tab}  end if;\n"
              if (memReadSync.component.nodes.contains(memReadSync))
                emitRead(tab + "  ")
              ret ++= s"${tab}end if;\n"

              def emitWrite(tab: String) = ret ++= s"$tab${emitReference(memWrite.getMem)}(to_integer(${emitReference(memWrite.getAddress)})) <= ${emitReference(memWrite.getData)};\n"
              def emitRead(tab: String) = ret ++= s"$tab${emitReference(memReadSync.consumers(0))} <= ${emitReference(memReadSync.getMem)}(to_integer(${emitReference(memReadSync.getAddress)}));\n"
            }
            case memWriteRead_readPart: MemWriteOrRead_readPart => {

            }
            case assertNode : AssertNode => {
              val cond = emitLogic(assertNode.cond)
              val message = if(assertNode.message != null) s"""report "${assertNode.message}" """ else ""
              val severity = "severity " +  (assertNode.severity match{
                case `NOTE`     => "NOTE"
                case `WARNING`  => "WARNING"
                case `ERROR`    => "ERROR"
                case `FAILURE`  => "FAILURE"
              })
              ret ++= s"${tab}assert $cond = '1' $message $severity;\n"
            }
          }
          emitAssignementLevel(rootContext,ret, tab, "<=")
        }
      }
    }
  }

  def emitAssignement(to: Node, from: Node, ret: StringBuilder, tab: String, assignementKind: String): Unit = {
    from match {
      case from: AssignementNode => {
        from match { //TODO
          case assign: BitAssignmentFixed => ret ++= s"$tab${emitReference(to)}[${assign.getBitId}] ${assignementKind} ${emitLogic(assign.getInput)};\n"
          case assign: BitAssignmentFloating => ret ++= s"$tab${emitReference(to)}[${emitLogic(assign.getBitId)}] ${assignementKind} ${emitLogic(assign.getInput)};\n"
          case assign: RangedAssignmentFixed => ret ++= s"$tab${emitReference(to)}[${assign.getHi} : ${assign.getLo}] ${assignementKind} ${emitLogic(assign.getInput)};\n"
          case assign: RangedAssignmentFloating => ret ++= s"$tab${emitReference(to)}[${emitLogic(assign.getOffset)} +: ${assign.getBitCount.value}] ${assignementKind} ${emitLogic(assign.getInput)};\n"
        }
      }
      case man: MultipleAssignmentNode => {
        //For some case with asyncronous partial assignement
        man.onEachInput(assign => {
          emitAssignement(to, assign, ret, tab, assignementKind)
        })
      }
      case _ => ret ++= s"$tab${emitReference(to)} ${assignementKind} ${emitLogic(from)};\n"
    }
  }

  def emitAssignementLevel(context : AssignementLevel,ret: mutable.StringBuilder, tab: String, assignementKind: String, isElseIf: Boolean = false): Unit = {
    def emitLogicChunk(key: WhenTree): Unit = {
      if (context.logicChunk.contains(key)) {
        for ((to, from) <- context.logicChunk.get(key).get) {
          emitAssignement(to, from, ret, tab, assignementKind)
        }
      }
    }
    val firstTab = if (isElseIf) "" else tab

    emitLogicChunk(null)
    //OPT tolist.sort
    for (conditionalTree <- context.conditionalTrees.values.toList.sortWith(_.instanceCounter < _.instanceCounter)) conditionalTree match {
      case when: WhenTree => {
        def doTrue = when.whenTrue.isNotEmpty
        def doFalse = when.whenFalse.isNotEmpty
        val condLogic = emitLogic(when.cond)

        if (doTrue && !doFalse) {
          ret ++= s"${firstTab}if $condLogic begin\n"
          emitAssignementLevel(when.whenTrue,ret, tab + "  ", assignementKind)
          ret ++= s"${tab}end\n"
        } else /*if (doTrue && doFalse)*/ {
          ret ++= s"${firstTab}if $condLogic begin\n"
          emitAssignementLevel(when.whenTrue,ret, tab + "  ", assignementKind)
          val falseHead = if (when.whenFalse.logicChunk.isEmpty && when.whenFalse.conditionalTrees.size == 1) when.whenFalse.conditionalTrees.head._1 else null
          if (falseHead != null && falseHead.isInstanceOf[WhenContext] && falseHead.asInstanceOf[WhenContext].parentElseWhen != null) {
            ret ++= s"${tab}end else "
            emitAssignementLevel(when.whenFalse,ret, tab, assignementKind, true)
          } else {
            ret ++= s"${tab}end else \n"
            emitAssignementLevel(when.whenFalse,ret, tab + "  ", assignementKind)
            ret ++= s"${tab}end\n"
          }
        }
        emitLogicChunk(when)
      }
//      case switchTree: SwitchTree => {
//        for (caseElement <- switchTree.cases if caseElement != null) {
//          val (cond, level) = caseElement
//          ret ++= s"${tab}if ${emitLogic(cond)} = '1'  then\n"
//          emitAssignementLevel(level,ret, tab + "  ", assignementKind)
//          ret ++= s"${tab}end if;\n"
//        }
//      }
    }
  }

  def emitComponentInstances(component: Component, ret: StringBuilder): Unit = {
    for (child <- component.children) {
      val isBB = child.isInstanceOf[BlackBox]
      val isBBUsingULogic = isBB && child.asInstanceOf[BlackBox].isUsingULogic
      val definitionString = child.definitionName
      ret ++= s"  $definitionString "

      if (child.isInstanceOf[BlackBox]) {
        val bb = child.asInstanceOf[BlackBox]
        val genericFlat = bb.getGeneric.flatten

        if (genericFlat.size != 0) {
          ret ++= s"#(\n"


          for ((name, e) <- genericFlat) {
            e match {
              case baseType: BaseType => ret ++= s"    .${name}(${emitLogic(baseType.getInput(0))}),\n"
              case s: String => ret ++= s"    .${name}(${"\""}${s}${"\""}),\n"
              case i: Int => ret ++= s"    .${name}($i),\n"
              case d: Double => ret ++= s"    .${name}($d),\n"
              case b: Boolean => ret ++= s"    .${name}($b),\n"
              case t: STime => {
                ???
              }
            }
          }
          ret.setCharAt(ret.size - 2, ' ')
          ret ++= s") "
        }
      }

      ret ++= s"${child.getName()} (\n"
      for (data <- child.getOrdredNodeIo) {
        if (data.isOutput) {
          val bind = component.kindsOutputsToBindings.getOrElse(data, null)
          if (bind != null) {
            ret ++= s"    .${emitReference(data)}(${emitReference(bind)}),\n"
          }
        }
        else if (data.isInput)
          ret ++= s"    .${emitReference(data)}(${emitReference(data.getInput(0))}),\n"
      }
      ret.setCharAt(ret.size - 2, ' ')

      ret ++= s"  );"
      ret ++= s"\n"
    }
  }
}
