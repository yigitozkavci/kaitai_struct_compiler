package io.kaitai.struct

import io.kaitai.struct.format.{AttrSpec, ClassSpec, ClassSpecs, MemberSpec}
import io.kaitai.struct.languages.HaskellCompiler
import io.kaitai.struct.languages.components.ExtraAttrs

import scala.collection.mutable.ListBuffer

class HaskellClassCompiler (
  classSpecs: ClassSpecs,
  override val topClass: ClassSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, HaskellCompiler) {
  override def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    val extraAttrs = ListBuffer[AttrSpec]()
    extraAttrs ++= generateAttrs(curClass)

    lang.classHeader(curClass.name)
    compileAttrDeclarations(curClass.seq ++ extraAttrs)
    lang.classFooter(curClass.name)
  }

  def generateAttrs(curClass: ClassSpec): List[AttrSpec] = {
    curClass.seq.foldLeft(List[AttrSpec]())(
      (attrs, attr) => attrs ++ ExtraAttrs.forAttr(attr)
    )
  }

  override def compileAttrDeclarations(attrs: List[MemberSpec]): Unit = {
    attrs.zipWithIndex.foreach { case (attr, index) =>
      lang.attributeDeclarationWithIndex(attr.id, attr.dataTypeComposite, index)
    }
  }
}