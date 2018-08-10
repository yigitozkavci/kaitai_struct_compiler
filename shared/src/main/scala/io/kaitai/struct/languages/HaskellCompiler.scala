package io.kaitai.struct.languages

import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{HaskellTranslator, TypeDetector}

import scala.collection.mutable.ListBuffer

class HaskellCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with SingleOutputFile
    with UpperCamelCaseClasses {
  import HaskellCompiler._

  val translator = new HaskellTranslator(typeProvider, importList)

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {}

  override def indent: String = "  "

  override def outFileName(topClassName: String): String =
    s"src/${type2class(topClassName)}.hs"

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

  override def classHeader(name: List[String]): Unit = {
    val typeName = type2class(name(0))
    out.puts(s"data $typeName = $typeName")
    out.inc
  }

  override def classFooter(name: List[String]): Unit = {
    out.puts("}")
    out.dec
  }

  override def classConstructorHeader(name: List[String], parentType: DataType, rootClassName: List[String], isHybrid: Boolean, params: List[ParamDefSpec]): Unit = ???

  override def classConstructorFooter: Unit = ???

  override def runRead(): Unit = {}

  override def runReadCalc(): Unit = ???

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = ???

  override def readFooter(): Unit = ???

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeDeclarationWithIndex(attrName: Identifier, attrType: DataType, index: Int): Unit = {
    val prefix = if (index == 0) "{ " else ", "
    out.puts(s"${prefix}${idToStr(attrName)} :: ${kaitaiType2NativeType(attrType)}")
  }

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = ???

  override def attrParse(attr: AttrLikeSpec, id: Identifier, extraAttrs: ListBuffer[AttrSpec], defEndian: Option[Endianness]): Unit = ???

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = ???

  override def attrFixedContentsParse(attrName: Identifier, contents: Array[Byte]): Unit = ???

  override def condIfHeader(expr: expr): Unit = ???

  override def condIfFooter(expr: expr): Unit = ???

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = ???

  override def condRepeatEosFooter: Unit = ???

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: expr): Unit = ???

  override def condRepeatExprFooter: Unit = ???

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: expr): Unit = ???

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: expr): Unit = ???

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = ???

  override def normalIO: String = ???

  override def useIO(ioEx: expr): String = ???

  override def pushPos(io: String): Unit = ???

  override def seek(io: String, pos: expr): Unit = ???

  override def popPos(io: String): Unit = ???

  override def alignToByte(io: String): Unit = ???

  override def instanceHeader(className: List[String], instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = ???

  override def instanceFooter: Unit = ???

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier): Unit = ???

  override def instanceReturn(instName: InstanceIdentifier): Unit = ???

  override def instanceCalculate(instName: Identifier, dataType: DataType, value: expr): Unit = ???

  override def enumDeclaration(curClass: List[String], enumName: String, enumColl: Seq[(Long, EnumValueSpec)]): Unit = ???

  override def switchCaseStart(condition: expr): Unit = ???

  override def switchCaseEnd(): Unit = ???

  override def switchElseStart(): Unit = ???

  override def switchEnd(): Unit = ???

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

  def kaitaiType2NativeType(attrType: DataType) : String = "Word8"
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

