package io.kaitai.struct.translators

import io.kaitai.struct.{ImportList, Utils}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast._
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format.Identifier
import io.kaitai.struct.languages.HaskellCompiler

class HaskellTranslator(provider: TypeProvider, importList: ImportList) extends BaseTranslator(provider) {
  override def doIntLiteral(n: BigInt): String = {
    val literal = if (n > Long.MaxValue) {
      "0x" + n.toString(16)
    } else {
      n.toString
    }

    s"$literal"
  }

  override def doArrayLiteral(t: DataType, value: Seq[expr]): String = {
    "array"
  }

  override def doByteArrayLiteral(arr: Seq[Byte]): String =
    "bytearray"
  override def doByteArrayNonLiteral(elts: Seq[expr]): String =
    "bytearraynonliteral"

  override def numericBinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) = {
    (detectType(left), detectType(right), op) match {
      case (_: IntType, _: IntType, Ast.operator.Mod) =>
        s".mod(${translate(left)}, ${translate(right)})"
      case _ =>
        super.numericBinOp(left, op, right)
    }
  }

  override def doName(s: String) =
    s match {
      case Identifier.ROOT => s
      case Identifier.PARENT => "_parent()"
      case Identifier.IO => "_io()"
      case Identifier.ITERATOR => "_it"
      case Identifier.ITERATOR2 => "_buf"
      case Identifier.SWITCH_ON => "on"
      case Identifier.INDEX => "i"
      case _ => s"${Utils.lowerCamelCase(s)}()"
    }

  override def doEnumByLabel(enumTypeAbs: List[String], label: String): String =
    s"${enumClass(enumTypeAbs)}.${label.toUpperCase}"
  override def doEnumById(enumTypeAbs: List[String], id: String): String =
    s"${enumClass(enumTypeAbs)}.byId($id)"

  def enumClass(enumTypeAbs: List[String]): String = {
    val enumTypeRel = Utils.relClass(enumTypeAbs, provider.nowClass.name)
    enumTypeRel.map((x) => Utils.upperCamelCase(x)).mkString(".")
  }

  override def doStrCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) = {
    if (op == Ast.cmpop.Eq) {
      s"${translate(left)}.equals(${translate(right)})"
    } else if (op == Ast.cmpop.NotEq) {
      s"!(${translate(left)}).equals(${translate(right)})"
    } else {
      s"(${translate(left)}.compareTo(${translate(right)}) ${cmpOp(op)} 0)"
    }
  }

  override def doBytesCompareOp(left: Ast.expr, op: Ast.cmpop, right: Ast.expr): String = {
    "doBytesCompareOp"
  }

  override def doSubscript(container: expr, idx: expr): String =
    "doSubscript"
  override def doIfExp(condition: expr, ifTrue: expr, ifFalse: expr): String =
    "doIfExp"
  override def doCast(value: Ast.expr, typeName: DataType): String =
    "doCast"

  // Predefined methods of various types
  override def strToInt(s: expr, base: expr): String =
    "strToInt"
  override def enumToInt(v: expr, et: EnumType): String =
    "enumToInt"
  override def floatToInt(v: expr): String =
    "floatToInt"
  override def intToStr(i: expr, base: expr): String =
    "intToStr"
  override def bytesToStr(bytesExpr: String, encoding: Ast.expr): String = {
    "bytesToStr"
  }
  override def bytesLength(b: Ast.expr): String =
    "bytesLength"
  override def strLength(s: expr): String =
    "strLength"
  override def strReverse(s: expr): String =
    "strReverse"
  override def strSubstring(s: expr, from: expr, to: expr): String =
    "strSubstring"
  override def arrayFirst(a: expr): String =
    "def"
  override def arrayLast(a: expr): String = {
    "arrayLast"
  }
  override def arraySize(a: expr): String =
    "arraySize"
  override def arrayMin(a: Ast.expr): String = {
    "arrayMin"
  }
  override def arrayMax(a: Ast.expr): String = {
    "arrayMax"
  }
}
