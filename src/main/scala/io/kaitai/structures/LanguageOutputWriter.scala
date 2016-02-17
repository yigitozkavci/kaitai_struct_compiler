package io.kaitai.structures

import java.io.{FileOutputStream, OutputStreamWriter, PrintWriter}
import java.nio.charset.StandardCharsets

class LanguageOutputWriter(fileName: String, indentStr: String) {
  val out = new PrintWriter(new OutputStreamWriter(new FileOutputStream(fileName), StandardCharsets.UTF_8), true)
  var indentLevel = 0

  def puts(s: String): Unit = out.println(indentStr * indentLevel + s)
  def puts: Unit = out.println
  def inc: Unit = indentLevel += 1
  def dec: Unit = indentLevel -= 1
}
