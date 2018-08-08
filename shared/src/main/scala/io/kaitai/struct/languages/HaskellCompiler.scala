package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{CalcEndian, DataType, FixedEndian, InheritedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{HaskellTranslator, TypeDetector}

class HaskellCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with UpperCamelCaseClasses
    with ObjectOrientedLanguage
    with EveryReadIsExpression
    with UniversalFooter
    with UniversalDoc
    with AllocateIOLocalVar
    with FixedContentsUsingArrayByteLiteral
    with NoNeedForFullClassPath {
  import HaskellCompiler._

  val translator = new HaskellTranslator(typeProvider, importList)

  override def universalFooter : Unit =
    out.dec  

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {}

  override def indent: String = "  "

  override def outFileName(topClassName: String): String =
    s"src/${type2class(topClassName)}"

  override def outImports(topClass: ClassSpec) =
    "\n" + importList.toList.map((x) => s"import $x").mkString("\n") + "\n"

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"-- | Auto generated file for Haskell")
    outHeader.puts("{-# LANGUAGE RecordWildCards #-}")
    outHeader.puts(s"module ${config.haskellModule} where")

    importList.add("Lib")
    importList.add("qualified Data.ByteString.Lazy as LBS")
    importList.add("Data.Word")

    out.puts
  }

  override def classHeader(name: String): Unit = {}

  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {}

  override def runRead(): Unit =
    out.puts("_read();")

  override def runReadCalc(): Unit = {}

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {}

  override def readFooter(): Unit = universalFooter

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def universalDoc(doc: DocSpec): Unit = {}

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {}

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit = {}

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {}

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = "allocIO"

  override def useIO(ioEx: expr): String =
    "io"

  override def pushPos(io: String): Unit = {}

  override def seek(io: String, pos: Ast.expr): Unit = {}

  override def popPos(io: String): Unit = {}

  override def alignToByte(io: String): Unit = {}

  override def attrDebugStart(attrId: Identifier, attrType: DataType, ios: Option[String], rep: RepeatSpec): Unit = {}

  override def attrDebugEnd(attrId: Identifier, attrType: DataType, io: String, rep: RepeatSpec): Unit = {}

  override def condIfHeader(expr: expr): Unit = {}

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {}

  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {}

  override def condRepeatEosFooter: Unit = {}

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: expr): Unit = {}

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {}

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {}

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {}

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {}

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {}

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit = {}

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = "expr"

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytesStripRight($expr0, (byte) $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytesTerminate($expr1, (byte) $term, $include)"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String): Unit = {}

  override def switchCaseFirstStart(condition: Ast.expr): Unit = {}

  override def switchCaseStart(condition: Ast.expr): Unit = {}

  override def switchCaseEnd(): Unit = {}

  override def switchElseStart(): Unit = {}

  override def switchEnd(): Unit = {}

  override def instanceDeclaration(attrName: InstanceIdentifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {}

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = {}

  override def instanceReturn(instName: InstanceIdentifier): Unit = {}

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: expr): Unit = {}

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {}

  override def debugClassSequence(seq: List[AttrSpec]) = {}

  override def privateMemberName(id: Identifier): String = s"this.${idToStr(id)}"

  def idToStr(id: Identifier): String = "wowsoidentifier"

  override def publicMemberName(id: Identifier) = idToStr(id)

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"
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

