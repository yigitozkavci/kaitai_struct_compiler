package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.HaskellCompiler

class HaskellTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  override def doSubscript(container: expr, idx: expr): String = "(unimplemented)doSubscript"

  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    s"if (${translate(condition)}) then (${translate(ifTrue)}) else (${translate(ifFalse)})"

  override def doName(s: String): String =
    s"(notimplemented)doName"

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    s"(notimplemented)doEnumByLabel"

  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    s"(notimplemented)doEnumById"

  override def bytesToStr(value: String, expr: expr): String =
    s"(notimplemented)bytesToStr"

  override def strLength(s: expr): String =
    s"(notimplemented)strLength"

  override def strReverse(s: expr): String =
    s"(notimplemented)strReverse"

  override def strToInt(s: expr, base: expr): String =
    s"(notimplemented)strToInt"

  override def strSubstring(s: expr, from: expr, to: expr): String =
    s"(notimplemented)strSubstring"

  override def intToStr(value: expr, num: expr): String =
    s"(notimplemented)intToStr"

  override def floatToInt(value: expr): String =
    s"(notimplemented)floatToInt"

  override def arrayFirst(a: expr): String =
    s"(notimplemented)arrayFirst"

  override def arrayLast(a: expr): String =
    s"(notimplemented)arrayLast"

  override def arraySize(a: expr): String =
    s"(notimplemented)arraySize"

  override def arrayMin(a: expr): String =
    s"(notimplemented)arrayMin"

  override def arrayMax(a: expr): String =
    s"(notimplemented)arrayMax"

  override def enumToInt(value: expr, et: EnumType): String =
    s"(notimplemented)enumToInt"
}

