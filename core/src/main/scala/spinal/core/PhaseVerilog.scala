//package spinal.core
//
//import scala.collection.mutable
//import scala.collection.mutable.{ArrayBuffer, HashMap, StringBuilder}
//import scala.util.Random
//
///**
// * Created by PIC32F_USER on 05/06/2016.
// */
//
//
//
//class PhaseVerilog(pc : PhaseContext) extends PhaseMisc with VerilogBase {
//  import pc._
//
//  override def useNodeConsumers: Boolean = true
//
//
//  var memBitsMaskKind : MemBitsMaskKind = MULTIPLE_RAM
//
//  val emitedComponent = mutable.Map[ComponentBuilder, ComponentBuilder]()
//  val emitedComponentRef = mutable.Map[Component, Component]()
//
//
//  override def impl(pc : PhaseContext): Unit = {
//    import pc._
//    SpinalProgress("Write Verilog")
//    if(!pc.config.oneFilePerComponent) {
//      val outFile = new java.io.FileWriter(pc.config.targetDirectory + "/" + (if (pc.config.netlistFileName == null) (topLevel.definitionName + ".v") else pc.config.netlistFileName))
//      outFile.write(VhdlVerilogBase.getHeader("//", topLevel))
//      emitEnumPackage(outFile)
//
//      for (c <- sortedComponents) {
//        if (!c.isInBlackBoxTree) {
//          SpinalProgress(s"${"  " * (1 + c.level)}emit ${c.definitionName}")
//          compile(c,outFile)
//        }
//      }
//
//      outFile.flush();
//      outFile.close();
//    } else {
//      assert(pc.config.netlistFileName == null)
//      val enumFile = new java.io.FileWriter(pc.config.targetDirectory + "/" + (if (pc.config.netlistFileName == null) (topLevel.definitionName + ".vh") else pc.config.netlistFileName))
//      enumFile.write(VhdlVerilogBase.getHeader("//", topLevel))
//      emitEnumPackage(enumFile)
//      enumFile.flush();
//      enumFile.close();
//
//      for (c <- sortedComponents) {
//        if (!c.isInBlackBoxTree) {
//          SpinalProgress(s"${"  " * (1 + c.level)}emit ${c.definitionName}")
//          val outFile = new java.io.FileWriter(pc.config.targetDirectory + "/" + (if (pc.config.netlistFileName == null) (c.definitionName + ".v") else pc.config.netlistFileName))
//          outFile.write(VhdlVerilogBase.getHeader("//", topLevel))
//          outFile.write("`include \"" + topLevel.definitionName + ".vh" + "\"\n")
//          compile(c,outFile)
//          outFile.flush();
//          outFile.close();
//        }
//      }
//
//    }
//  }
//
//
//
//
//  def compile(component: Component,outFile : java.io.FileWriter): Unit = {
//    val text = emit(component)
//    outFile.write(text)
//  }
//
//
//  def emit(component: Component): String = {
//    val ret = new StringBuilder()
//    val builder = new ComponentBuilder(component)
//
//    emitModuleIo(component, builder)
//    emitModuleContent(component, builder)
//
//    val oldBuilder = emitedComponent.getOrElse(builder, null)
//    if (oldBuilder == null) {
//      emitedComponent += (builder -> builder)
//      return builder.result
//    } else {
//      emitedComponentRef += (component -> oldBuilder.component)
//      return s"\n//${component.definitionName} remplaced by ${oldBuilder.component.definitionName}\n\n"
//    }
//  }
//
//
//  def getReEncodingFuntion(spinalEnum: SpinalEnum, source: SpinalEnumEncoding, target: SpinalEnumEncoding): String = {
//    s"${spinalEnum.getName()}_${source.getName()}_to_${target.getName()}"
//  }
//
//  def getEnumToDebugFuntion(spinalEnum: SpinalEnum, source: SpinalEnumEncoding): String = {
//    assert(!source.isNative)
//    s"${spinalEnum.getName()}_${source.getName()}_to_debug"
//  }
//
//  def getEnumDebugType(spinalEnum: SpinalEnum): String = {
//    s"${spinalEnum.getName()}_debug"
//  }
//
//
//
//  def emitModuleIo(component: Component, builder: ComponentBuilder): Unit = {
//    var ret = builder.newPart(false)
//    ret ++= s"module ${component.definitionName}\n"
//    ret = builder.newPart(true)
//    ret ++= s"( \n"
//    component.getOrdredNodeIo.foreach(baseType => {
//      ret ++= s"  ${emitSyntaxAttributes(baseType.instanceAndSyncNodeAttributes)}${emitDirection(baseType)} ${if(signalNeedProcess(baseType)) "reg " else ""}${emitDataType(baseType)} ${baseType.getName()}${getBaseTypeSignalInitialisation(baseType)}${emitCommentAttributes(baseType.instanceAndSyncNodeAttributes)},\n"
//    })
//
//    ret.setCharAt(ret.size - 2, ' ')
//    ret ++= s");\n"
//    ret = builder.newPart(false)
//    ret ++= s"\n"
//  }
//
//
//  def emitModuleContent(component: Component, builder: ComponentBuilder): Unit = {
//    var ret = builder.newPart(true)
//    val enumDebugSignals = ArrayBuffer[SpinalEnumCraft[_]]()
//    emitFunctions(component,ret)
//    emitSignals(component, ret, enumDebugSignals)
//    val retTemp,retTemp2,retTemp3 = new StringBuilder
//    emitComponentInstances(component, retTemp)
//    emitAsyncronous(component, retTemp, ret)
//    emitSyncronous(component, retTemp3, retTemp2)
//    if(component == topLevel && pc.config.dumpWave != null){
//      ret ++= s"""
//initial begin
//  $$dumpfile("${pc.config.dumpWave.vcdPath}");
//  $$dumpvars(${pc.config.dumpWave.depth}, ${component.definitionName});
//end
//"""
//    }
//
//
//    ret ++= retTemp
//    ret ++= retTemp2
//    ret ++= retTemp3
//
//    ret ++= s"endmodule\n"
//    ret ++= s"\n"
//  }
//
//
//  def toSpinalEnumCraft[T <: SpinalEnum](that: Any) = that.asInstanceOf[SpinalEnumCraft[T]]
//
//
//  def emitEnumPackage(out: java.io.FileWriter): Unit = {
//    val ret = new StringBuilder();
//
//
//    ret ++= "\n"
//    for ((enumDef, encodings) <- enums) {
//      val enumName = enumDef.getName()
//      for (encoding <- encodings) {
//        val encodingName = encoding.getName()
//        val bitCount = encoding.getWidth(enumDef)
//        val vhdlEnumType = emitEnumType(enumDef, encoding,"")
//        ret ++= s"`define $vhdlEnumType [${bitCount - 1}:0]\n"
//        for (element <- enumDef.elements) {
//          ret ++= s"`define ${emitEnumLiteral(element, encoding,"")} ${idToBits(element, encoding)}\n"
//        }
//        ret ++= "\n"
//      }
//    }
//
//    def idToBits[T <: SpinalEnum](enum: SpinalEnumElement[T], encoding: SpinalEnumEncoding): String = {
//      val str = encoding.getValue(enum).toString(2)
//      "'b" + ("0" * (encoding.getWidth(enum.spinalEnum) - str.length)) + str
//    }
//
//
//    out.write(ret.result())
//  }
//  def emitFunctions(component: Component, ret: StringBuilder): Unit = {
//    val alreadyEmitted = mutable.Set[String]()
//    for (node <- component.nodes) {
//      node match {
//        case node : CastEnumToEnum => {
//          val encodingSrc = node.input.getEncoding
//          val enumDef = node.getDefinition
//          val encodingDst = node.getEncoding
//          val fName = getReEncodingFuntion(enumDef, encodingSrc,encodingDst)
//          if(!alreadyEmitted.contains(fName)) {
//            alreadyEmitted += fName
//            ret ++= s"  function $fName(${emitEnumType(enumDef, encodingSrc)} that);\n"
//            ret ++= "  begin\n"
//            ret ++= "    case(that) \n"
//            for (e <- enumDef.elements) {
//              ret ++= s"      ${emitEnumLiteral(e, encodingSrc)} : $fName =  ${emitEnumLiteral(e, encodingDst)};\n"
//            }
//            ret ++= s"      default : $fName =  ${emitEnumLiteral(enumDef.elements.head, encodingDst)};\n"
//            ret ++= "    endcase\n"
//            ret ++= "  end\n"
//            ret ++= "  endfunction\n\n"
//          }
//        }
//        case _ =>
//      }
//    }
//  }
//
//
//
//  def getBaseTypeSignalInitialisation(signal : BaseType) : String = {
//    val reg = if(signal.isReg) signal.input.asInstanceOf[Reg] else null
//    if(reg != null){
//      if(reg.initialValue != null && reg.getClockDomain.config.resetKind == BOOT) {
//        return " = " + (reg.initialValue match {
//          case init : BaseType => emitLogic(init.getLiteral)
//          case init =>  emitLogic(init)
//        })
//      }else if (reg.hasTag(randomBoot)) {
//        return signal match {
//          case b: Bool => " = " + (if (Random.nextBoolean()) "1" else "0")
//
//          case bv: BitVector => {
//            val rand = BigInt(bv.getWidth, Random).toString(2)
//            " = " + bv.getWidth + "'b" + "0" * (bv.getWidth - rand.length) + rand
//          }
//          case e: SpinalEnumCraft[_] => {
//            val vec = e.spinalEnum.elements.toVector
//            val rand = vec(Random.nextInt(vec.size))
//            " = " + emitEnumLiteral(rand, e.getEncoding)
//          }
//        }
//      }
//    }
//    ""
//  }
//
//
//  def emitSignals(component: Component, ret: StringBuilder, enumDebugSignals: ArrayBuffer[SpinalEnumCraft[_]]): Unit = {
//    var verilogIndexGenerated = false
//    for (node <- component.nodes) {
//      node match {
//        case signal: BaseType => {
//          if (!signal.isIo) {
//            ret ++= s"  ${emitSyntaxAttributes(signal.instanceAndSyncNodeAttributes)}${if(signalNeedProcess(signal)) "reg " else "wire "}${emitDataType(signal)} ${emitReference(signal)}${getBaseTypeSignalInitialisation(signal)}${emitCommentAttributes(signal.instanceAndSyncNodeAttributes)};\n"
//          }
//        }
//
//        case mem: Mem[_] => {
//          //ret ++= emitSignal(mem, mem);
//
//
//          val symbolWidth = mem.getMemSymbolWidth()
//          val symbolCount = mem.getMemSymbolCount
//          if(memBitsMaskKind == MULTIPLE_RAM && symbolCount != 1) {
//            //if(mem.initialContent != null) SpinalError("Memory with multiple symbol per line + initial contant are not suported currently")
//
//             for(i <- 0 until symbolCount) {
//              val postfix = "_symbol" + i
//              ret ++= s"  ${emitSyntaxAttributes(mem.instanceAttributes(Language.VERILOG))}reg [${symbolWidth- 1}:0] ${emitReference(mem)}$postfix [0:${mem.wordCount - 1}]${emitCommentAttributes(mem.instanceAttributes(Language.VERILOG))};\n"
//            }
//          }else{
//            ret ++= s"  ${emitSyntaxAttributes(mem.instanceAttributes(Language.VERILOG))}reg ${emitRange(mem)} ${emitReference(mem)} [0:${mem.wordCount - 1}]${emitCommentAttributes(mem.instanceAttributes(Language.VERILOG))};\n"
//          }
//
//          if (mem.initialContent != null) {
//            ret ++= "  initial begin\n"
//            for ((value, index) <- mem.initialContent.zipWithIndex) {
//              val unfilledValue = value.toString(2)
//              val filledValue = "0" * (mem.getWidth-unfilledValue.length) + unfilledValue
//              if(memBitsMaskKind == MULTIPLE_RAM && symbolCount != 1) {
//                for(i <- (0 until symbolCount)){
//                  ret ++= s"    ${emitReference(mem)}_symbol$i[$index] = 'b${filledValue.substring(symbolWidth*(symbolCount-i-1), symbolWidth*(symbolCount-i))};\n"
//                }
//              }else{
//                ret ++= s"    ${emitReference(mem)}[$index] = 'b$filledValue;\n"
//              }
//            }
//
//            ret ++= "  end\n"
//          }else if(mem.hasTag(randomBoot)){
//            if(!verilogIndexGenerated) {
//              verilogIndexGenerated = true
//              ret ++= "integer verilogIndex;\n"
//            }
//            ret ++= s"""
//initial begin
//  for (verilogIndex = 0; verilogIndex < ${mem.wordCount}; verilogIndex = verilogIndex + 1)begin
//${
//  if(symbolCount == 1){
//    emitReference(mem) + "[verilogIndex] = -1;"
//  }  else {
//    (0 until symbolCount).map("    " + emitReference(mem)  + "_symbol" + _ + "[verilogIndex] = -1;").reduce(_ + "\n" +_)
//  }}
//  end
//end
//"""
//          }
//        }
//        case _ =>
//      }
//
//
//    }
//  }
//
//  def emitSyntaxAttributes(attributes: Iterable[Attribute]): String = {
//    val values = for (attribute <- attributes if attribute.attributeKind() == DEFAULT_ATTRIBUTE) yield attribute match {
//      case attribute: AttributeString => attribute.getName + " = \"" + attribute.value + "\""
//      case attribute: AttributeFlag => attribute.getName
//    }
//    if(values.isEmpty) return ""
//    "(* " + values.reduce(_ + " , " + _) + " *) "
//  }
//
//  def emitCommentAttributes(attributes: Iterable[Attribute]): String = {
//    val values = for (attribute <- attributes if attribute.attributeKind() == COMMENT_ATTRIBUTE) yield attribute match {
//      case attribute: AttributeString => attribute.getName + " = \"" + attribute.value + "\""
//      case attribute: AttributeFlag => attribute.getName
//    }
//    if(values.isEmpty) return ""
//    "/* " + values.reduce(_ + " , " + _) + " */ "
//  }
//
//  def signalNeedProcess(baseType: BaseType) : Boolean = {
//    if(baseType.input.isInstanceOf[SyncNode]) return true
//    if(baseType.input.isInstanceOf[MultipleAssignmentNode] || baseType.input.isInstanceOf[WhenNode]) return true
//    return false
//  }
//
//  def emitAsyncronous(component: Component, ret: StringBuilder, funcRet: StringBuilder): Unit = {
//    val processList = getAsyncProcesses(component,false)
//
//    for (process <- processList if !process.needProcessDef) {
//      for (node <- process.nodes) {
//        emitAssignement(node, node.getInput(0), ret, "  assign ", "=")
//      }
//    }
//
//    referenceSet = mutable.Set[Node with Nameable with ContextUser]()
//    for ((process,idx) <- processList.zipWithIndex if process.needProcessDef) {
//      process.genSensitivity
//
//      val context = new AssignementLevel(process.nodes.map(n => AssignementLevelCmd(n,n.getInput(0))))
//
//
//      if (process.sensitivity.size != 0) {
//        val tmp = new StringBuilder
//        referenceSet.clear()
//        emitAssignementLevel(context,tmp, "    ", "=",false,process.sensitivity)
//
//        ret ++= s"  always @ (${referenceSet.toList.sortWith(_.instanceCounter < _.instanceCounter).map(emitReference(_)).reduceLeft(_ + " or " + _)})\n"
//        ret ++= "  begin\n"
//        ret ++= tmp
////        val senList = process.sensitivity.toList
////        ret ++= s"""    $$display("$component $idx :${(senList.map(emitReference(_) + "=%b")).reduce(_+ " " +_)}",${(senList.map(emitReference(_))).reduce(_+ "," +_)});\n"""
//        ret ++= "  end\n\n"
//      } else {
//        //emit func as logic
//        assert(process.nodes.size == 1)
//        for (node <- process.nodes) {
//          ret ++= s"  initial begin\n"
//          emitAssignementLevel(context,ret, "    ", "=")
//          ret ++= s"  end\n"
//        }
//      }
//    }
//    referenceSet = null
//  }
//
//
//  def operatorImplAsBinaryOperator(vhd: String)(op: Modifier): String = s"(${emitLogic(op.getInput(0))} $vhd ${emitLogic(op.getInput(1))})"
//  def operatorImplAsBinaryOperatorSigned(vhd: String)(op: Modifier): String = s"($$signed(${emitLogic(op.getInput(0))}) $vhd $$signed(${emitLogic(op.getInput(1))}))"
//  def operatorImplAsBinaryOperatorLeftSigned(vhd: String)(op: Modifier): String = s"($$signed(${emitLogic(op.getInput(0))}) $vhd ${emitLogic(op.getInput(1))})"
//
//
//  def operatorImplAsUnaryOperator(vhd: String)(op: Modifier): String = {
//    s"($vhd ${emitLogic(op.getInput(0))})"
//  }
//
//  def operatorImplAsFunction(vhd: String)(func: Modifier): String = {
//    s"$vhd(${func.getInputs.map(emitLogic(_)).reduce(_ + "," + _)})"
//  }
//
//  def operatorImplAsMux(func: Modifier): String = {
//    val mux = func.asInstanceOf[Multiplexer]
//    s"(${emitLogic(mux.cond)} ? ${emitLogic(mux.whenTrue)} : ${emitLogic(mux.whenFalse)})"
//  }
//
//
//
//  def operatorImplAsNoTransformation(func: Modifier): String = {
//    emitLogic(func.getInput(0))
//  }
//  def operatorImplAsSigned(func: Modifier): String = {
//    "$signed(" + emitLogic(func.getInput(0)) + ")"
//  }
//  def shiftRightByIntImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftRightByInt]
//    s"(${emitLogic(node.input)} >>> ${node.shift})"
//  }
//
//  def shiftLeftByIntImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftLeftByInt]
//    s"(${emitLogic(node.input)} <<< ${node.shift})"
//  }
//
//  def shiftRightByIntFixedWidthImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftRightByIntFixedWidth]
//    s"(${emitLogic(node.input)} >>> ${node.shift})"
//  }
//  def shiftRightSignedByIntFixedWidthImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftRightByIntFixedWidth]
//    s"($$signed(${emitLogic(node.input)}) >>> ${node.shift})"
//  }
//
//  def shiftLeftByIntFixedWidthImpl(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.ShiftLeftByIntFixedWidth]
//    s"(${emitLogic(node.input)} <<< ${node.shift})"
//  }
//
//
//  def operatorImplAsCat(op : Modifier) : String = {
//    val cat = op.asInstanceOf[Operator.Bits.Cat]
//    s"{${emitLogic(cat.left)},${emitLogic(cat.right)}}"
//  }
//
//  def unimplementedModifier(message : String)(op: Modifier): String = {
//    SpinalError(message)
//  }
//
//
//
//  def enumEgualsImpl(eguals: Boolean)(op: Modifier): String = {
//    val enumDef = op.asInstanceOf[EnumEncoded].getDefinition
//    val encoding = op.asInstanceOf[EnumEncoded].getEncoding
//
//    encoding match {
//      case `binaryOneHot` => s"((${emitLogic(op.getInput(0))} & ${emitLogic(op.getInput(1))}) ${if (eguals) "!=" else "=="} 'b${"0" * encoding.getWidth(enumDef)})"
//      case _ => s"(${emitLogic(op.getInput(0))} ${if (eguals) "==" else "!="} ${emitLogic(op.getInput(1))})"
//    }
//  }
//
//  def operatorImplAsEnumToEnum(func: Modifier): String = {
//    val enumCast = func.asInstanceOf[CastEnumToEnum]
//    val enumDefSrc = enumCast.input.getDefinition
//    val encodingSrc = enumCast.input.getEncoding
//    val enumDefDst = enumCast.getDefinition
//    val encodingDst = enumCast.getEncoding
//
//    s"${getReEncodingFuntion(enumDefDst, encodingSrc,encodingDst)}(${emitLogic(enumCast.input)})"
//  }
//
//  val modifierImplMap = mutable.Map[String, Modifier => String]()
//
//
//  //unsigned
//  modifierImplMap.put("u+u", operatorImplAsBinaryOperator("+"))
//  modifierImplMap.put("u-u", operatorImplAsBinaryOperator("-"))
//  modifierImplMap.put("u*u", operatorImplAsBinaryOperator("*"))
//  modifierImplMap.put("u/u", operatorImplAsBinaryOperator("/"))
//  modifierImplMap.put("u%u", operatorImplAsBinaryOperator("%"))
//
//  modifierImplMap.put("u|u", operatorImplAsBinaryOperator("|"))
//  modifierImplMap.put("u&u", operatorImplAsBinaryOperator("&"))
//  modifierImplMap.put("u^u", operatorImplAsBinaryOperator("^"))
//  modifierImplMap.put("~u",  operatorImplAsUnaryOperator("~"))
//
//  modifierImplMap.put("u==u", operatorImplAsBinaryOperator("=="))
//  modifierImplMap.put("u!=u", operatorImplAsBinaryOperator("!="))
//  modifierImplMap.put("u<u",  operatorImplAsBinaryOperator("<"))
//  modifierImplMap.put("u<=u", operatorImplAsBinaryOperator("<="))
//
//
//  modifierImplMap.put("u>>i", shiftRightByIntImpl)
//  modifierImplMap.put("u<<i", shiftLeftByIntImpl)
//  modifierImplMap.put("u>>u", operatorImplAsBinaryOperator(">>>"))
//  modifierImplMap.put("u<<u", operatorImplAsBinaryOperator("<<<"))
//  modifierImplMap.put("u|>>i",  shiftRightByIntFixedWidthImpl)
//  modifierImplMap.put("u|<<i",  shiftLeftByIntFixedWidthImpl)
//  modifierImplMap.put("u|<<u",  operatorImplAsBinaryOperator("<<<"))
//
//
//  //signed
//  modifierImplMap.put("s+s", operatorImplAsBinaryOperatorSigned("+"))
//  modifierImplMap.put("s-s", operatorImplAsBinaryOperatorSigned("-"))
//  modifierImplMap.put("s*s", operatorImplAsBinaryOperatorSigned("*"))
//  modifierImplMap.put("s/s", operatorImplAsBinaryOperatorSigned("/"))
//  modifierImplMap.put("s%s", operatorImplAsBinaryOperatorSigned("%"))
//
//  modifierImplMap.put("s|s", operatorImplAsBinaryOperator("|"))
//  modifierImplMap.put("s&s", operatorImplAsBinaryOperator("&"))
//  modifierImplMap.put("s^s", operatorImplAsBinaryOperator("^"))
//  modifierImplMap.put("~s", operatorImplAsUnaryOperator("~"))
//  modifierImplMap.put("-s", operatorImplAsUnaryOperator("-"))
//
//  modifierImplMap.put("s==s", operatorImplAsBinaryOperatorSigned("=="))
//  modifierImplMap.put("s!=s", operatorImplAsBinaryOperatorSigned("!="))
//  modifierImplMap.put("s<s", operatorImplAsBinaryOperatorSigned("<"))
//  modifierImplMap.put("s<=s", operatorImplAsBinaryOperatorSigned("<="))
//
//
//  modifierImplMap.put("s>>i", shiftRightByIntImpl)
//  modifierImplMap.put("s<<i", shiftLeftByIntImpl)
//  modifierImplMap.put("s>>u", operatorImplAsBinaryOperatorLeftSigned(">>>"))
//  modifierImplMap.put("s<<u", operatorImplAsBinaryOperatorLeftSigned("<<<"))
//  modifierImplMap.put("s|>>i",  shiftRightSignedByIntFixedWidthImpl)
//  modifierImplMap.put("s|<<i",  shiftLeftByIntFixedWidthImpl)
//  modifierImplMap.put("s|<<u",  operatorImplAsBinaryOperatorLeftSigned("<<<"))
//
//
//
//  //bits
//  modifierImplMap.put("b##b", operatorImplAsCat)
//
//  modifierImplMap.put("b|b", operatorImplAsBinaryOperator("|"))
//  modifierImplMap.put("b&b", operatorImplAsBinaryOperator("&"))
//  modifierImplMap.put("b^b", operatorImplAsBinaryOperator("^"))
//  modifierImplMap.put("~b",  operatorImplAsUnaryOperator("~"))
//
//  modifierImplMap.put("b==b", operatorImplAsBinaryOperator("=="))
//  modifierImplMap.put("b!=b", operatorImplAsBinaryOperator("!="))
//
//  modifierImplMap.put("b>>i",  shiftRightByIntImpl)
//  modifierImplMap.put("b<<i",  shiftLeftByIntImpl)
//  modifierImplMap.put("b>>u",  operatorImplAsBinaryOperator(">>>"))
//  modifierImplMap.put("b<<u",  operatorImplAsBinaryOperator("<<<"))
//  modifierImplMap.put("b|>>i",  shiftRightByIntFixedWidthImpl)
//  modifierImplMap.put("b|<<i",  shiftLeftByIntFixedWidthImpl)
//  modifierImplMap.put("b|<<u",  operatorImplAsBinaryOperator("<<<"))
//
//
//
//  //bool
//  modifierImplMap.put("B==B", operatorImplAsBinaryOperator("=="))
//  modifierImplMap.put("B!=B", operatorImplAsBinaryOperator("!="))
//
//
//  modifierImplMap.put("!", operatorImplAsUnaryOperator("!"))
//  modifierImplMap.put("&&", operatorImplAsBinaryOperator("&&"))
//  modifierImplMap.put("||", operatorImplAsBinaryOperator("||"))
//  modifierImplMap.put("B^B", operatorImplAsBinaryOperator("^"))
//
//
//  //enum
//  modifierImplMap.put("e==e", enumEgualsImpl(true))
//  modifierImplMap.put("e!=e", enumEgualsImpl(false))
//
//  //cast
//  modifierImplMap.put("s->b", operatorImplAsNoTransformation)
//  modifierImplMap.put("u->b", operatorImplAsNoTransformation)
//  modifierImplMap.put("B->b", operatorImplAsNoTransformation)
//  modifierImplMap.put("e->b", operatorImplAsNoTransformation)
//
//  modifierImplMap.put("b->s", operatorImplAsNoTransformation)
//  modifierImplMap.put("u->s", operatorImplAsNoTransformation)
//
//  modifierImplMap.put("b->u", operatorImplAsNoTransformation)
//  modifierImplMap.put("s->u", operatorImplAsNoTransformation)
//
//  modifierImplMap.put("b->e", operatorImplAsNoTransformation)
//  modifierImplMap.put("e->e", operatorImplAsEnumToEnum)
//
//
//  //misc
//
//  modifierImplMap.put("resize(s,i)", operatorImplAsSigned)
//  modifierImplMap.put("resize(u,i)", operatorImplAsNoTransformation)
//  modifierImplMap.put("resize(b,i)", operatorImplAsNoTransformation)
//
//  modifierImplMap.put("bAllByB", unaryAllBy)
//  modifierImplMap.put("uAllByB", unaryAllBy)
//  modifierImplMap.put("sAllByB", unaryAllBy)
//
//
//  //Memo whenNode hardcode emitlogic
//  modifierImplMap.put("mux(B,B,B)", operatorImplAsMux)
//  modifierImplMap.put("mux(B,b,b)", operatorImplAsMux)
//  modifierImplMap.put("mux(B,u,u)", operatorImplAsMux)
//  modifierImplMap.put("mux(B,s,s)", operatorImplAsMux)
//  modifierImplMap.put("mux(B,e,e)", operatorImplAsMux)
//
//  modifierImplMap.put("extract(b,i)", extractBoolFixed)
//  modifierImplMap.put("extract(u,i)", extractBoolFixed)
//  modifierImplMap.put("extract(s,i)", extractBoolFixed)
//
//  modifierImplMap.put("extract(b,u)", extractBoolFloating)
//  modifierImplMap.put("extract(u,u)", extractBoolFloating)
//  modifierImplMap.put("extract(s,u)", extractBoolFloating)
//
//  modifierImplMap.put("extract(b,i,i)", extractBitVectorFixed)
//  modifierImplMap.put("extract(u,i,i)", extractBitVectorFixed)
//  modifierImplMap.put("extract(s,i,i)", extractBitVectorFixed)
//
//  modifierImplMap.put("extract(b,u,w)", extractBitVectorFloating)
//  modifierImplMap.put("extract(u,u,w)", extractBitVectorFloating)
//  modifierImplMap.put("extract(s,u,w)", extractBitVectorFloating)
//
//  def extractBoolFixed(func: Modifier): String = {
//    val that = func.asInstanceOf[ExtractBoolFixed]
//    s"${emitLogic(that.getBitVector)}[${that.getBitId}]"
//  }
//
//  def extractBoolFloating(func: Modifier): String = {
//    val that = func.asInstanceOf[ExtractBoolFloating]
//    s"${emitLogic(that.getBitVector)}[${emitLogic(that.getBitId)}]"
//  }
//
//  def extractBitVectorFixed(func: Modifier): String = {
//    val that = func.asInstanceOf[ExtractBitsVectorFixed]
//    s"${emitLogic(that.getBitVector)}[${that.getHi} : ${that.getLo}]"
//  }
//
//  def extractBitVectorFloating(func: Modifier): String = {
//    val that = func.asInstanceOf[ExtractBitsVectorFloating]
//    s"${emitLogic(that.getBitVector)}[${emitLogic(that.getOffset)} +: ${that.getBitCount}]"
//  }
//
//  def unaryAllBy(func: Modifier): String = {
//    val node = func.asInstanceOf[Operator.BitVector.AllByBool]
//    s"{${node.getWidth}{${emitLogic(node.input)}}}"
//  }
//
//
//
//
//  def emitLogic(node: Node): String = node match {
//    case baseType: BaseType => emitReference(baseType)
//    case node: Modifier => modifierImplMap.getOrElse(node.opName, throw new Exception("can't find " + node.opName))(node)
//
//    case lit: BitVectorLiteral => s"(${lit.getWidth}'b${lit.getBitsStringOn(lit.getWidth)})"
//
//    case lit: BitsAllToLiteral => lit.theConsumer match {
//      case _: Bits => if(lit.getWidth == 0) "1'b0" else s"(${lit.getWidth}'b${lit.getBitsStringOn(lit.getWidth)})"
//      case _: UInt => if(lit.getWidth == 0) "1'b0" else s"(${lit.getWidth}'b${lit.getBitsStringOn(lit.getWidth)})"
//      case _: SInt => if(lit.getWidth == 0) "1'b0" else s"(${lit.getWidth}'b${lit.getBitsStringOn(lit.getWidth)})"
//    }
//
//    case lit: BoolLiteral => if(lit.value) "1'b1" else "1'b0"
//    case lit: EnumLiteral[_] => emitEnumLiteral(lit.enum, lit.encoding)
//    case memRead: MemReadAsync => {
//      if(memRead.aspectRatio != 1) SpinalError(s"Verilog backend can't emit ${memRead.getMem} because of its mixed width ports")
//      if (memRead.readUnderWrite == dontCare) SpinalWarning(s"memReadAsync with dontCare is as writeFirst into Verilog")
//      val symbolCount = memRead.getMem.getMemSymbolCount
//      if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//        s"${emitReference(memRead.getMem)}[${emitReference(memRead.getAddress)}]"
//      else
//        "{" + (0 until symbolCount).reverse.map(i => (s"${emitReference(memRead.getMem)}_symbol$i[${emitReference(memRead.getAddress)}]")).reduce(_ + " , " + _) + "}"
//    }
//    case whenNode: WhenNode => s"(${emitLogic(whenNode.cond)} ? ${emitLogic(whenNode.whenTrue)} : ${emitLogic(whenNode.whenFalse)})" //Exeptional case with asyncrouns of literal
//    case dc : DontCareNodeEnum => {
//      val width = dc.encoding.getWidth(dc.enum)
//      s"(${width}'b${"x" * width})"
//    }
//    case dc: DontCareNode => {
//      dc.getBaseType match {
//        case to: Bool => "1'bx"
//        case to: BitVector => s"(${dc.asInstanceOf[Widthable].getWidth}'b${"x" * to.getWidth})"
//      }
//    }
//
//    case o => throw new Exception("Don't know how emit logic of " + o.getClass.getSimpleName)
//  }
//
//  def emitDebug(component: Component, ret: StringBuilder, enumDebugSignals: ArrayBuffer[SpinalEnumCraft[_]]): Unit = {
//    for (signal <- enumDebugSignals) {
//      ret ++= s"  ${emitReference(signal)}_debug <= ${getEnumToDebugFuntion(toSpinalEnumCraft(signal).spinalEnum, signal.getEncoding)}(${emitReference(signal)});\n"
//    }
//  }
//
//  def emitSyncronous(component: Component, ret: StringBuilder, preDeclarations : StringBuilder): Unit = {
//    val syncNodes = component.getDelays
//
//    val clockDomainMap = mutable.Map[ClockDomain, ArrayBuffer[SyncNode]]()
//
//    for (syncNode <- syncNodes) {
//      clockDomainMap.getOrElseUpdate(syncNode.getClockDomain, new ArrayBuffer[SyncNode]()) += syncNode
//    }
//
//    for ((clockDomain, array) <- clockDomainMap) {
//      val arrayWithReset = ArrayBuffer[SyncNode]()
//      val arrayWithoutReset = ArrayBuffer[SyncNode]()
//
//      for (syncNode <- array) {
//        if (syncNode.isUsingResetSignal || syncNode.isUsingSoftResetSignal) arrayWithReset += syncNode else arrayWithoutReset += syncNode
//      }
//
//      emitClockDomain(true)
//      emitClockDomain(false)
//
//
//
//      def emitClockDomain(withReset: Boolean): Unit = {
//        val activeArray = if (withReset) arrayWithReset else arrayWithoutReset
//        if (activeArray.size == 0) return;
//        val clock = component.pulledDataCache.getOrElse(clockDomain.clock, throw new Exception("???")).asInstanceOf[Bool]
//        val reset = if (null == clockDomain.reset || !withReset) null else component.pulledDataCache.getOrElse(clockDomain.reset, throw new Exception("???")).asInstanceOf[Bool]
//        val softReset = if (null == clockDomain.softReset || !withReset) null else component.pulledDataCache.getOrElse(clockDomain.softReset, throw new Exception("???")).asInstanceOf[Bool]
//        val clockEnable = if (null == clockDomain.clockEnable) null else component.pulledDataCache.getOrElse(clockDomain.clockEnable, throw new Exception("???")).asInstanceOf[Bool]
//        val asyncReset = (null != reset) && clockDomain.config.resetKind == ASYNC
//        val syncReset = (null != reset) && clockDomain.config.resetKind == SYNC
//
//        var tabLevel = 1
//        def tabStr = "  " * tabLevel
//        def inc = {
//          tabLevel = tabLevel + 1
//        }
//        def dec = {
//          tabLevel = tabLevel - 1
//        }
//
//
//        val initialValues = ArrayBuffer[Node]()
//        val initialTasks = ArrayBuffer[AssignementLevelCmd]()
//        for (syncNode <- activeArray) syncNode match {
//          case reg: Reg => {
//            if (reg.hasInitialValue) {
//              initialTasks += AssignementLevelCmd(reg.getOutputByConsumers, reg.getInitialValue)
//              initialValues += reg.getInitialValue
//            }
//          }
//          case _ =>
//        }
//        val initialValueAssignement = new AssignementLevel(initialTasks)
//
//
//        if (asyncReset) {
//          val sensitivity = getSensitivity(initialValues, true)  //${if(sensitivity.isEmpty) "" else sensitivity.foldLeft("")(_ + "," + emitReference(_))}
//          ret ++= s"${tabStr}always @ (${emitClockEdge(clock,clockDomain.config.clockEdge)} or ${emitResetEdge(reset,clockDomain.config.resetActiveLevel)})\n"
//        } else {
//          ret ++= s"${tabStr}always @ (${emitClockEdge(clock,clockDomain.config.clockEdge)})\n"
//        }
//
//        ret ++= s"${tabStr}begin\n"
//        inc
//        if (asyncReset) {
//          ret ++= s"${tabStr}if (${if (clockDomain.config.resetActiveLevel == HIGH) "" else "!"}${emitReference(reset)}) begin\n";
//          inc
//          emitRegsInitialValue(initialValueAssignement, tabStr)
//          dec
//          ret ++= s"${tabStr}end else begin\n"
//          inc
//        }
//
//        if (clockEnable != null) {
//          ret ++= s"${tabStr}if(${if (clockDomain.config.clockEnableActiveLevel == HIGH) "" else "!"}${emitReference(clockEnable)}) begin\n"
//          inc
//        }
//        if (syncReset || softReset != null) {
//          var condList = ArrayBuffer[String]()
//          if(syncReset) condList += s"${if (clockDomain.config.resetActiveLevel == HIGH) "" else "!"}${emitReference(reset)}"
//          if(softReset != null) condList += s"${if (clockDomain.config.softResetActiveLevel == HIGH) "" else "!"}${emitReference(softReset)}"
//
//          ret ++= s"${tabStr}if(${condList.reduce(_ + " || " + _)}) begin\n"
//          inc
//          emitRegsInitialValue(initialValueAssignement, tabStr)
//          dec
//          ret ++= s"${tabStr}end else begin\n"
//          inc
//          emitRegsLogic(tabStr)
//          dec
//          ret ++= s"${tabStr}end\n"
//          dec
//        } else {
//          emitRegsLogic(tabStr)
//          dec
//        }
//
//        while (tabLevel != 1) {
//          ret ++= s"${tabStr}end\n"
//          dec
//        }
//        ret ++= s"${tabStr}end\n"
//        dec
//        ret ++= s"${tabStr}\n"
//
//
//        def emitRegsInitialValue(assignementLevel: AssignementLevel, tab: String): Unit = {
//          emitAssignementLevel(assignementLevel,ret, tab, "<=")
//        }
//
//
//        def emitRegsLogic(tab: String): Unit = {
//
//
//          val assignementTasks = ArrayBuffer[AssignementLevelCmd]()
//
//
//          for (syncNode <- activeArray) syncNode match {
//            case reg: Reg => {
//              val regSignal = reg.getOutputByConsumers
//              if (!regSignal.isIo || !regSignal.isInput) {
//                val in = reg.getDataInput
//                if (in != reg)
//                  assignementTasks += AssignementLevelCmd(regSignal, in)
//              }
//            }
//            case memWrite: MemWrite => {
//              if(memWrite.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memWrite.getMem} because of its mixed width ports")
//
//              if (memWrite.useWriteEnable) {
//                ret ++= s"${tab}if(${emitReference(memWrite.getEnable)})begin\n"
//                emitWrite(tab + "  ")
//                ret ++= s"${tab}end\n"
//              } else {
//                emitWrite(tab)
//              }
//
//              def emitWrite(tab: String) = {
//                val symbolCount = memWrite.getMem.getMemSymbolCount
//                val bitPerSymbole = memWrite.getMem.getMemSymbolWidth()
//
//                if(memWrite.getMask == null) {
//                  if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                    ret ++= s"$tab${emitReference(memWrite.getMem)}[${emitReference(memWrite.getAddress)}] <= ${emitReference(memWrite.getData)};\n"
//                  else
//                    for(i <- 0 until symbolCount) {
//                      val range = s"[${(i + 1) * bitPerSymbole - 1} : ${i * bitPerSymbole}]"
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}_symbol${i}[${emitReference(memWrite.getAddress)}] <= ${emitReference(memWrite.getData)}$range;\n"
//                    }
//                }else{
//
//                  val maskCount = memWrite.getMask.getWidth
//                  for(i <- 0 until maskCount){
//                    val range = s"[${(i+1)*bitPerSymbole-1} : ${i*bitPerSymbole}]"
//                    ret ++= s"${tab}if(${emitReference(memWrite.getMask)}[$i])begin\n"
//                    if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}[${emitReference(memWrite.getAddress)}))$range <= ${emitReference(memWrite.getData)}$range;\n"
//                    else
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}_symbol${i}[${emitReference(memWrite.getAddress)}] <= ${emitReference(memWrite.getData)}$range;\n"
//
//                    ret ++= s"${tab}end\n"
//                  }
//                }
//              }
//            }
//            case memReadSync: MemReadSync => {
//              if(memReadSync.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memReadSync.getMem} because of its mixed width ports")
//              if(memReadSync.readUnderWrite == writeFirst) SpinalError(s"Can't translate a memReadSync with writeFirst into Verilog $memReadSync")
//              if(memReadSync.readUnderWrite == dontCare) SpinalWarning(s"memReadSync with dontCare is as readFirst into Verilog $memReadSync")
//              if(memReadSync.useReadEnable) {
//                ret ++= s"${tab}if(${emitReference(memReadSync.getReadEnable)})begin\n"
//                emitRead(tab + "  ")
//                ret ++= s"${tab}end\n"
//              } else {
//                emitRead(tab)
//              }
//              def emitRamRead() = {
//                val symbolCount = memReadSync.getMem.getMemSymbolCount
//                if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                  s"${emitReference(memReadSync.getMem)}[${emitReference(memReadSync.getAddress)}]"
//                else
//                  "{" + (0 until symbolCount).reverse.map(i => (s"${emitReference(memReadSync.getMem)}_symbol$i[${emitReference(memReadSync.getAddress)}]")).reduce(_ + "," + _) + "}"
//              }
//              def emitRead(tab: String) = ret ++= s"$tab${emitReference(memReadSync.consumers(0))} <= ${emitRamRead()};\n"
//
//            }
//
//            case memWrite: MemReadWrite_writePart => {
//              val memReadSync = memWrite.readPart
//              if(memWrite.aspectRatio != 1) SpinalError(s"VHDL backend can't emit ${memWrite.getMem} because of its mixed width ports")
//              if (memReadSync.readUnderWrite == writeFirst) SpinalError(s"Can't translate a MemWriteOrRead with writeFirst into Verilog $memReadSync")
//              if (memReadSync.readUnderWrite == dontCare) SpinalWarning(s"MemWriteOrRead with dontCare is as readFirst into Verilog $memReadSync")
//
//              //ret ++= s"${tab}if(${emitReference(memWrite.getChipSelect)}) begin\n"
//              //ret ++= s"${tab}  if(${emitReference(memWrite.getWriteEnable)}) begin\n"
//              emitWrite(tab)
//              //ret ++= s"${tab}  end\n"
//              if (memReadSync.component.nodes.contains(memReadSync))
//                emitRead(tab)
//              //ret ++= s"${tab}end\n"
//
//              def emitWrite(tab: String) = {
////                ret ++= s"$tab${emitReference(memWrite.getMem)}[${emitReference(memWrite.getAddress)}] <= ${emitReference(memWrite.getData)};\n"
//                val symbolCount = memWrite.getMem.getMemSymbolCount
//                val bitPerSymbole = memWrite.getMem.getMemSymbolWidth()
//
//                if(memWrite.getMask == null) {
//                  val condName = emitReference(memWrite.getChipSelect) + "_AND_" + emitReference(memWrite.getWriteEnable) + "_WRITE"
//                  preDeclarations ++= s"  wire $condName = ${emitReference(memWrite.getChipSelect)} && ${emitReference(memWrite.getWriteEnable)};\n"
//                  ret ++= s"${tab}if($condName) begin\n"
//                  if (memBitsMaskKind == SINGLE_RAM || symbolCount == 1) {
//
//                    ret ++= s"$tab$tab${emitReference(memWrite.getMem)}[${emitReference(memWrite.getAddress)}] <= ${emitReference(memWrite.getData)};\n"
//                  } else {
//                    for (i <- 0 until symbolCount) {
//                      val range = s"[${(i + 1) * bitPerSymbole - 1} : ${i * bitPerSymbole}]"
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}_symbol${i}[${emitReference(memWrite.getAddress)}] <= ${emitReference(memWrite.getData)}$range;\n"
//                    }
//                  }
//                  ret ++= s"${tab}  end\n"
//                }else{
//                  val maskCount = memWrite.getMask.getWidth
//                  for(i <- 0 until maskCount){
//                    val range = s"[${(i+1)*bitPerSymbole-1} : ${i*bitPerSymbole}]"
//                    val condName = emitReference(memWrite.getChipSelect) + "_AND_" + emitReference(memWrite.getWriteEnable) + "_AND_" + emitReference(memWrite.getMask) + i
//                    preDeclarations ++= s"  wire $condName = ${emitReference(memWrite.getChipSelect)} && ${emitReference(memWrite.getWriteEnable)} && ${emitReference(memWrite.getMask)}[$i];\n"
//                    ret ++= s"${tab}if($condName) begin\n"
//                    if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}[${emitReference(memWrite.getAddress)}))$range <= ${emitReference(memWrite.getData)}$range;\n"
//                    else
//                      ret ++= s"$tab  ${emitReference(memWrite.getMem)}_symbol${i}[${emitReference(memWrite.getAddress)}] <= ${emitReference(memWrite.getData)}$range;\n"
//
//                    ret ++= s"${tab}end\n"
//                  }
//                }
//              }
//
//              def emitRamRead() = {
//                val symbolCount = memReadSync.getMem.getMemSymbolCount
//
//                if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                  s"${emitReference(memReadSync.getMem)}[${emitReference(memReadSync.getAddress)}]"
//                else
//                  "{" + (0 until symbolCount).reverse.map(i => (s"${emitReference(memReadSync.getMem)}_symbol$i[${emitReference(memReadSync.getAddress)}]")).reduce(_ + "," + _) + "}"
//
//              }
//              def emitRead(tab: String) = {
//                val symbolCount = memReadSync.getMem.getMemSymbolCount
//                ret ++= s"${tab}if(${emitReference(memWrite.getChipSelect)}) begin\n"
//                if(memBitsMaskKind == SINGLE_RAM || symbolCount == 1)
//                  ret ++= s"$tab  ${emitReference(memReadSync.consumers(0))} <= ${emitReference(memReadSync.getMem)}[${emitReference(memReadSync.getAddress)}];\n"
//                else{
//                  val symboleReadDataNames = for(i <- 0 until symbolCount) yield {
//                    val symboleReadDataName = s"${emitReference(memReadSync.getMem)}_symbol${i}_${emitReference(memReadSync.getAddress)}"
//                    preDeclarations ++= s"  reg [${memReadSync.getMem.getMemSymbolWidth()-1}:0] $symboleReadDataName;\n"
//                    ret ++= s"$tab  $symboleReadDataName <= ${emitReference(memReadSync.getMem)}_symbol$i[${emitReference(memReadSync.getAddress)}];\n"
//                    symboleReadDataName
//                  }
//
//
//                  preDeclarations ++= s"  always @ (${symboleReadDataNames.mkString(" or " )}) begin\n"
//                  preDeclarations ++= s"    ${emitReference(memReadSync.consumers(0))} = {${symboleReadDataNames.reverse.mkString(", " )}};\n"
//                  preDeclarations ++= s"  end\n"
//                }
//
//                ret ++= s"${tab}end\n"
//              }
////              def emitRead(tab: String) = {
////                ret ++= s"$tab${emitReference(memReadSync.consumers(0))} <= ${emitReference(memReadSync.getMem)}[${emitReference(memReadSync.getAddress)}];\n"
////              }
//            }
//            case memWriteRead_readPart: MemReadWrite_readPart => {
//
//            }
//            case assertNode : AssertNode => {
//              val cond = emitLogic(assertNode.cond)
//              val severity = assertNode.severity match{
//                case `NOTE`     => "NOTE"
//                case `WARNING`  => "WARNING"
//                case `ERROR`    => "ERROR"
//                case `FAILURE`  => "FAILURE"
//              }
//
//
//              val frontString = (for(m <- assertNode.message) yield m match{
//                case m : String => m
//                case m : Node => "%x"
//              }).mkString
//
//              val backString = (for(m <- assertNode.signals) yield m match{
//                case m : Node => ", " + emitLogic(m)
//              }).mkString
//
//
//              ret ++= s"${tab}if (!$cond) begin\n"
//              ret ++= s"""${tab}  $$display("$severity $frontString"$backString);\n"""
//              if(assertNode.severity == `FAILURE`) ret ++= tab + "  $finish;\n"
//              ret ++= s"${tab}end\n"
//            }
//          }
//          val rootContext = new AssignementLevel(assignementTasks)
//          emitAssignementLevel(rootContext,ret, tab, "<=")
//        }
//      }
//    }
//  }
//
//  def emitAssignement(to: Node, from: Node, ret: StringBuilder, tab: String, assignementKind: String): Unit = {
//    from match {
//      case from: AssignementNode => {
//        from match {
//          case assign: BitAssignmentFixed => ret ++= s"$tab${emitAssignedReference(to)}[${assign.getBitId}] ${assignementKind} ${emitLogic(assign.getInput)};\n"
//          case assign: BitAssignmentFloating => ret ++= s"$tab${emitAssignedReference(to)}[${emitLogic(assign.getBitId)}] ${assignementKind} ${emitLogic(assign.getInput)};\n"
//          case assign: RangedAssignmentFixed => ret ++= s"$tab${emitAssignedReference(to)}[${assign.getHi} : ${assign.getLo}] ${assignementKind} ${emitLogic(assign.getInput)};\n"
//          case assign: RangedAssignmentFloating => ret ++= s"$tab${emitAssignedReference(to)}[${emitLogic(assign.getOffset)} +: ${assign.getBitCount.value}] ${assignementKind} ${emitLogic(assign.getInput)};\n"
//        }
//      }
//      case man: MultipleAssignmentNode => {
//        //For some case with asyncronous partial assignement
//        man.onEachInput(assign => {
//          emitAssignement(to, assign, ret, tab, assignementKind)
//        })
//      }
//      case _ => ret ++= s"$tab${emitAssignedReference(to)} ${assignementKind} ${emitLogic(from)};\n"
//    }
//  }
//
//   def emitAssignementLevel(context : AssignementLevel,ret: mutable.StringBuilder, tab: String, assignementKind: String, isElseIf: Boolean = false,hiddenSensitivity : mutable.Set[Node] = null): Unit = {
//    val firstTab = if (isElseIf) "" else tab
//
//    context.content.foreach(_ match {
//      case whenTree: AssignementLevelWhen => {
//        def doTrue = whenTree.whenTrue.isNotEmpty
//        def doFalse = whenTree.whenFalse.isNotEmpty
//        val condLogic = emitLogic(whenTree.cond)
//
//        if (doTrue && !doFalse) {
//          ret ++= s"${firstTab}if($condLogic)begin\n"
//          emitAssignementLevel(whenTree.whenTrue, ret, tab + "  ", assignementKind)
//          ret ++= s"${tab}end\n"
//        } else {
//          ret ++= s"${firstTab}if($condLogic)begin\n"
//          emitAssignementLevel(whenTree.whenTrue, ret, tab + "  ", assignementKind)
//          val falseHead = if (whenTree.whenFalse.isOnlyAWhen) whenTree.whenFalse.content.head else null
//          if (falseHead != null && falseHead.asInstanceOf[AssignementLevelWhen].context.parentElseWhen == whenTree.context) {
//            ret ++= s"${tab}end else "
//            emitAssignementLevel(whenTree.whenFalse, ret, tab, assignementKind, true)
//          } else {
//            ret ++= s"${tab}end else begin\n"
//            emitAssignementLevel(whenTree.whenFalse, ret, tab + "  ", assignementKind)
//            ret ++= s"${tab}end\n"
//          }
//        }
//      }
//
//      case switchTree : AssignementLevelSwitch => {
//        if(hiddenSensitivity != null) hiddenSensitivity += switchTree.key
//        ret ++= s"${tab}case(${emitLogic(switchTree.key)})\n"
//        switchTree.cases.foreach(c => {
//          ret ++= s"${tab}  ${emitLogic(c.const)} : begin\n"
//          emitAssignementLevel(c.doThat,ret,tab + "    ",assignementKind)
//          ret ++= s"${tab}  end\n"
//        })
//        ret ++= s"${tab}  default : begin\n"
//        if(switchTree.default != null){
//          emitAssignementLevel(switchTree.default.doThat,ret,tab + "    ",assignementKind)
//        }
//        ret ++= s"${tab}  end\n"
//        ret ++= s"${tab}endcase\n"
//      }
//      case task: AssignementLevelSimple => emitAssignement(task.that, task.by, ret, tab, assignementKind)
//    })
//  }
//
//  def emitComponentInstances(component: Component, ret: StringBuilder): Unit = {
//    for (child <- component.children) {
//      val isBB = child.isInstanceOf[BlackBox]
//      val isBBUsingULogic = isBB && child.asInstanceOf[BlackBox].isUsingULogic
//      val definitionString =  if (isBB) child.definitionName else emitedComponentRef.getOrElse(child, child).definitionName
//      ret ++= s"  $definitionString "
//
//      if (child.isInstanceOf[BlackBox]) {
//        val bb = child.asInstanceOf[BlackBox]
//        val genericFlat = bb.getGeneric.flatten
//
//        if (genericFlat.size != 0) {
//          ret ++= s"#( \n"
//          for (e <- genericFlat) {
//            e match {
//              case baseType: BaseType => ret ++= s"    .${emitReference(baseType)}(${emitLogic(baseType.getInput(0))}),\n"
//              case (name : String,s : String) => ret ++= s"    .${name}(${"\""}${s}${"\""}),\n"
//              case (name : String,i : Int) => ret ++= s"    .${name}($i),\n"
//              case (name : String,d : Double) => ret ++= s"    .${name}($d),\n"
//              case (name : String,b : Boolean) => ret ++= s"    .${name}($b),\n"
//            }
//          }
//          ret.setCharAt(ret.size - 2, ' ')
//          ret ++= s") "
//        }
//      }
//
//      ret ++= s"${child.getName()} ( \n"
//      for (data <- child.getOrdredNodeIo) {
//        if (data.isOutput) {
//          val bind = component.kindsOutputsToBindings.getOrElse(data, null)
//          if (bind != null) {
//            ret ++= s"    .${emitReference(data)}(${emitReference(bind)}),\n"
//          }
//        }
//        else if (data.isInput)
//          ret ++= s"    .${emitReference(data)}(${emitReference(data.getInput(0))}),\n"
//      }
//      ret.setCharAt(ret.size - 2, ' ')
//
//      ret ++= s"  );"
//      ret ++= s"\n"
//    }
//  }
//}
