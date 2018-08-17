package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.GoCompiler.{kaitaiType2NativeType, type2class}
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{HaskellTranslator, TypeDetector}

import scala.collection.mutable.ListBuffer

class HaskellCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with UpperCamelCaseClasses
    with FixedContentsUsingArrayByteLiteral
    with CommonReads
    with EveryReadIsExpression {

  import HaskellCompiler._

  val translator = new HaskellTranslator(typeProvider, importList)
  override def normalIO = "stream"

  override def switchStart(id: Identifier, on: Ast.expr): Unit =
    out.puts(s"(switchStart not implemented)")

  override def indent: String = "  "

  override def outFileName(topClassName: String): String =
    s"src/${type2class(topClassName)}.hs"

  override def outImports(topClass: ClassSpec) =
    "\n" + importList.toList.map((x) => s"import $x").mkString("\n") + "\n"

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"-- | Auto generated file for Haskell")
    outHeader.puts("{-# LANGUAGE RecordWildCards #-}")
    outHeader.puts(s"module ${type2class(topClassName)} where")

    importList.add("Lib")
    importList.add("qualified Data.ByteString.Lazy as LBS")
    importList.add("Data.Word")
    importList.add("Data.Int")

    out.puts
  }

  override def classHeader(name: List[String]): Unit = {
    val typeName = type2class(name.last)
    out.puts(s"data $typeName = $typeName")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = {
    out.puts("}")
    out.dec
    out.puts
  }

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    out.puts(s"instance Field ${type2class(name.last)} where")
    out.inc
    out.puts(s"parseField stream = do")
    out.inc
  }

  override def classConstructorFooter: Unit =
    out.puts("(notimplemented) classConstructorFooter")

  override def detailedClassConstructorFooter(name: List[String]): Unit = {
    val typeName = type2class(name.last)
    out.puts(s"pure $typeName {..}")
    out.dec
    out.dec
  }

  override def runRead(): Unit =
    out.puts("(notimplemented) runRead")


  override def runReadCalc(): Unit =
    out.puts("(notimplemented) runReadCalc")

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit =
    out.puts("(notimplemented) readHeader")

  override def readFooter(): Unit =
    out.puts("(notimplemented) readHeader")

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit =
    out.puts("(notimplemented) attributeDeclaration")

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit =
    out.puts("(notimplemented) attributeReader")

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit =
    out.puts("(notimplemented) attrParseHybrid")

  override def condIfHeader(expr: expr): Unit = {
    out.puts(s"when (${expression(expr)}) $$ do")
    out.inc
  }

  override def condIfFooter(expr: expr): Unit =
    out.dec

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    out.puts(s"${idToStr(id)} <- readUntilEOF $io $$ do")
    out.inc
  }

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    out.puts(s"${expr}")
  }

  override def condRepeatEosFooter: Unit = {
    out.dec
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: expr): Unit = {
    out.puts(s"${idToStr(id)} <- replicateM ${expression(repeatExpr)} $$ do")
    out.inc
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    out.puts(s"${expr}")

  override def condRepeatExprFooter: Unit =
    out.dec

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
    out.puts(s"readUntilEOF $io $$ do")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    out.puts(s"$expr")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit =
    out.dec

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit =
    out.puts(s"(attrProcess not implemented)")

  override def useIO(ioEx: expr): String = s"${expression(ioEx)}"

  override def pushPos(io: String): Unit =
    out.puts(s"pos' <- pos stream")

  override def seek(io: String, pos: expr): Unit =
    out.puts(s"seek ${expression(pos)} stream")

  override def popPos(io: String): Unit =
    out.puts(s"seek pos' stream")

  override def alignToByte(io: String): Unit =
    out.puts(s"(alignToByte not implemented)")

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit =
    out.puts(s"(instanceHeader not implemented")

  override def instanceFooter: Unit =
    out.puts(s"(instanceFooter not implemented")

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit =
    out.puts(s"(instanceCheckCacheAndReturn not implemented")

  override def instanceReturn(instName: InstanceIdentifier): Unit =
    out.puts(s"(instanceReturn not implemented")

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${idToStr(id)} <- ${expr}")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String =
    dataType match {
      case t: ReadableType =>
        s"(read${Utils.capitalize(t.apiCall(defEndian))} $io)"
      case blt: BytesLimitType =>
        s"(readBytes $io ${expression(blt.size)})"
      case _: BytesEosType =>
        s"(readBytesFull $io)"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"(readBytesTerm $io $terminator $include $consume $eosError)"
      case BitsType1 =>
        s"$io.readBitsInt(1) != 0"
      case BitsType(width: Int) =>
        s"$io.readBitsInt($width)"
      case t: UserType =>
        val addArgs = if (t.isOpaque) {
          "ifOpaque"
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "null"
            case Some(fp) => translator.translate(fp)
            case None => "this"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", _is_le"
            case _ => "wowend"
          }
          s", $parent, _root$addEndian"
        }
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), ", ", ", ", "")
        s"new ${types2class(t.name)}($io$addArgs$addParams)"
    }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean): String = ""

  override def attributeDeclarationWithIndex(attrName: Identifier, attrType: DataType, index: Int): Unit = {
    val prefix = if (index == 0) "{ " else ", "
    out.puts(s"${prefix}${idToStr(attrName)} :: ${kaitaiType2NativeType(attrType)}")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"ensureFixedContents' $normalIO ${contents}")

  /**
    * Renders identifier as a proper reference to a private member
    * that represents this field. This might include some prefixes
    * like "@" or "this." or "self.".
    *
    * @param id identifier to render
    * @return identifier as string
    */
  override def privateMemberName(id: Identifier): String = "_"

  /**
    * Renders identifier as a proper reference to a public member
    * that represents this field.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  override def publicMemberName(id: Identifier): String = ""

  /**
    * Renders identifier as a proper reference to a local temporary
    * variable appropriately named to hold a temporary reference to
    * this field.
    *
    * @param id identifier to render
    * @return identifier as string
    */
  override def localTemporaryName(id: Identifier): String = ""

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit =
    out.puts(s"(enumDeclaration not implemented)")

  override def switchCaseStart(condition: expr): Unit =
    out.puts(s"(switchCaseStart not implemented")

  override def switchCaseEnd(): Unit =
    out.puts(s"(switchCaseStart not implemented")


  override def switchElseStart(): Unit =
    out.puts(s"(switchElseStart not implemented")

  override def switchEnd(): Unit =
    out.puts(s"(switchEnd not implemented")

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => Utils.lowerCamelCase(name)
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => Utils.lowerCamelCase(name)
      case RawIdentifier(innerId) => "_raw_" + idToStr(innerId)
      case IoStorageIdentifier(innerId) => "_io_" + idToStr(innerId)
    }
  }

  def kaitaiType2NativeType(attrType: DataType) : String =
    attrType match {
      case Int1Type(false) => "Word8"
      case IntMultiType(false, Width2, _) => "Word16"
      case IntMultiType(false, Width4, _) => "Word32"
      case IntMultiType(false, Width8, _) => "Word64"

      case Int1Type(true) => "Int8"
      case IntMultiType(true, Width2, _) => "Int16"
      case IntMultiType(true, Width4, _) => "Int32"
      case IntMultiType(true, Width8, _) => "Int64"

      case FloatMultiType(Width4, _) => "Float"
      case FloatMultiType(Width8, _) => "Double"

      case BitsType(_) => "Word64"

      case _: BooleanType => "Bool"
      case CalcIntType => "Int"
      case CalcFloatType => "Float"

      case _: StrType => "String"
      case _: BytesType => "ByteString"

      case AnyType => "a" // Parametric polymorphism, baby.
      case KaitaiStreamType => kstreamName
      case KaitaiStructType => kstructName

      case t: UserType => types2class(t.name)
      case EnumType(name, _) => "EnumType: not implemented"

      case ArrayType(inType) => s"[${kaitaiType2NativeType(inType)}]"

      case SwitchType(_, cases) => kaitaiType2NativeType(TypeDetector.combineTypes(cases.values))
    }

  def types2class(names: List[String]) = names.map(x => type2class(x)).mkString("_")
}

object HaskellCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames {

  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new HaskellCompiler(tp, config)

  override def kstreamName: String = "KaitaiStream"
  override def kstructName: String = "KaitaiStruct"
}

