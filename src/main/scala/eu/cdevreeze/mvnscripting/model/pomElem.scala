/*
 * Copyright 2011-2017 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.mvnscripting.model

import java.net.URI

import scala.collection.immutable
import scala.reflect.classTag

import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.core.QName
import eu.cdevreeze.yaidom.core.Scope
import eu.cdevreeze.yaidom.indexed
import eu.cdevreeze.yaidom.queryapi.ElemApi.anyElem
import eu.cdevreeze.yaidom.queryapi.ScopedElemLike
import eu.cdevreeze.yaidom.queryapi.SubtypeAwareElemLike
import eu.cdevreeze.yaidom.simple
import PomElem._

/**
 * Any XML element in a POM file. See http://maven.apache.org/xsd/maven-4.0.0.xsd. Model version 4.0.0 is assumed.
 *
 * @author Chris de Vreeze
 */
sealed abstract class PomElem(val underlyingElem: indexed.Elem) extends ScopedElemLike with SubtypeAwareElemLike {

  type ThisElem = PomElem

  final def thisElem: ThisElem = this

  final def findAllChildElems: immutable.IndexedSeq[PomElem] = {
    underlyingElem.findAllChildElems.map(e => PomElem(e))
  }

  final def resolvedName: EName = {
    underlyingElem.resolvedName
  }

  final def resolvedAttributes: immutable.IndexedSeq[(EName, String)] = {
    underlyingElem.resolvedAttributes.toIndexedSeq
  }

  final def text: String = {
    underlyingElem.text
  }

  final def qname: QName = {
    underlyingElem.qname
  }

  final def attributes: immutable.IndexedSeq[(QName, String)] = {
    underlyingElem.attributes.toIndexedSeq
  }

  final def scope: Scope = {
    underlyingElem.scope
  }
}

sealed trait HasTextValue extends PomElem {

  final def value: String = text
}

/**
 * The project root element.
 */
final class ProjectElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) {
  require(resolvedName == ProjectEName, s"Required element name: $ProjectEName, but found $resolvedName")

  def modelVersionOption: Option[ModelVersionElem] = {
    findChildElemOfType(classTag[ModelVersionElem])(anyElem)
  }

  def groupIdOption: Option[GroupIdElem] = {
    findChildElemOfType(classTag[GroupIdElem])(anyElem)
  }

  def artifactIdOption: Option[ArtifactIdElem] = {
    findChildElemOfType(classTag[ArtifactIdElem])(anyElem)
  }

  def versionOption: Option[VersionElem] = {
    findChildElemOfType(classTag[VersionElem])(anyElem)
  }

  def packagingOption: Option[PackagingElem] = {
    findChildElemOfType(classTag[PackagingElem])(anyElem)
  }

  def classifierOption: Option[ClassifierElem] = {
    findChildElemOfType(classTag[ClassifierElem])(anyElem)
  }

  def propertiesOption: Option[PropertiesElem] = {
    findChildElemOfType(classTag[PropertiesElem])(anyElem)
  }

  def nameOption: Option[NameElem] = {
    findChildElemOfType(classTag[NameElem])(anyElem)
  }

  def urlOption: Option[UrlElem] = {
    findChildElemOfType(classTag[UrlElem])(anyElem)
  }

  def parentOption: Option[ParentElem] = {
    findChildElemOfType(classTag[ParentElem])(anyElem)
  }

  def dependenciesOption: Option[DependenciesElem] = {
    findChildElemOfType(classTag[DependenciesElem])(anyElem)
  }

  def dependencyManagementOption: Option[DependencyManagementElem] = {
    findChildElemOfType(classTag[DependencyManagementElem])(anyElem)
  }
}

final class GroupIdElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) with HasTextValue {
  require(resolvedName == GroupIdEName, s"Required element name: $GroupIdEName, but found $resolvedName")
}

final class ArtifactIdElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) with HasTextValue {
  require(resolvedName == ArtifactIdEName, s"Required element name: $ArtifactIdEName, but found $resolvedName")
}

final class VersionElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) with HasTextValue {
  require(resolvedName == VersionEName, s"Required element name: $VersionEName, but found $resolvedName")
}

final class PackagingElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) with HasTextValue {
  require(resolvedName == PackagingEName, s"Required element name: $PackagingEName, but found $resolvedName")
}

final class ClassifierElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) with HasTextValue {
  require(resolvedName == ClassifierEName, s"Required element name: $ClassifierEName, but found $resolvedName")
}

final class PropertiesElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) {
  require(resolvedName == PropertiesEName, s"Required element name: $PropertiesEName, but found $resolvedName")

  def properties: immutable.IndexedSeq[OtherPomElem] = {
    findAllChildElemsOfType(classTag[OtherPomElem])
  }
}

final class ParentElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) {
  require(resolvedName == ParentEName, s"Required element name: $ParentEName, but found $resolvedName")

  def groupIdOption: Option[GroupIdElem] = findChildElemOfType(classTag[GroupIdElem])(anyElem)

  def artifactIdOption: Option[ArtifactIdElem] = findChildElemOfType(classTag[ArtifactIdElem])(anyElem)

  def versionOption: Option[VersionElem] = findChildElemOfType(classTag[VersionElem])(anyElem)

  def relativePathOption: Option[RelativePathElem] = findChildElemOfType(classTag[RelativePathElem])(anyElem)
}

final class RelativePathElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) with HasTextValue {
  require(resolvedName == RelativePathEName, s"Required element name: $RelativePathEName, but found $resolvedName")
}

final class NameElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) with HasTextValue {
  require(resolvedName == NameEName, s"Required element name: $NameEName, but found $resolvedName")
}

final class DescriptionElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) with HasTextValue {
  require(resolvedName == DescriptionEName, s"Required element name: $DescriptionEName, but found $resolvedName")
}

final class UrlElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) with HasTextValue {
  require(resolvedName == UrlEName, s"Required element name: $UrlEName, but found $resolvedName")
}

final class ModelVersionElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) with HasTextValue {
  require(resolvedName == ModelVersionEName, s"Required element name: $ModelVersionEName, but found $resolvedName")
}

final class DependenciesElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) {
  require(resolvedName == DependenciesEName, s"Required element name: $DependenciesEName, but found $resolvedName")

  def dependencies: immutable.IndexedSeq[DependencyElem] = {
    findAllChildElemsOfType(classTag[DependencyElem])
  }
}

final class DependencyManagementElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) {
  require(resolvedName == DependencyManagementEName, s"Required element name: $DependencyManagementEName, but found $resolvedName")

  def dependenciesOption: Option[DependenciesElem] = {
    findChildElemOfType(classTag[DependenciesElem])(anyElem)
  }
}

final class DependencyElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) {
  require(resolvedName == DependencyEName, s"Required element name: $DependencyEName, but found $resolvedName")

  def groupIdOption: Option[GroupIdElem] = findChildElemOfType(classTag[GroupIdElem])(anyElem)

  def artifactIdOption: Option[ArtifactIdElem] = findChildElemOfType(classTag[ArtifactIdElem])(anyElem)

  def versionOption: Option[VersionElem] = findChildElemOfType(classTag[VersionElem])(anyElem)

  def classifierOption: Option[ClassifierElem] = findChildElemOfType(classTag[ClassifierElem])(anyElem)

  def scopeOption: Option[ScopeElem] = findChildElemOfType(classTag[ScopeElem])(anyElem)
}

final class ScopeElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) with HasTextValue {
  require(resolvedName == ScopeEName, s"Required element name: $ScopeEName, but found $resolvedName")
}

final class OtherPomElem(underlyingElem: indexed.Elem) extends PomElem(underlyingElem) {
}

object PomElem {

  val NS = "http://maven.apache.org/POM/4.0.0"

  val ProjectEName = EName(NS, "project")
  val GroupIdEName = EName(NS, "groupId")
  val ArtifactIdEName = EName(NS, "artifactId")
  val VersionEName = EName(NS, "version")
  val PackagingEName = EName(NS, "packaging")
  val ClassifierEName = EName(NS, "classifier")
  val PropertiesEName = EName(NS, "properties")
  val ParentEName = EName(NS, "parent")
  val RelativePathEName = EName(NS, "relativePath")
  val NameEName = EName(NS, "name")
  val DescriptionEName = EName(NS, "description")
  val UrlEName = EName(NS, "url")
  val ModelVersionEName = EName(NS, "modelVersion")
  val DependenciesEName = EName(NS, "dependencies")
  val DependencyManagementEName = EName(NS, "dependencyManagement")
  val DependencyEName = EName(NS, "dependency")
  val ScopeEName = EName(NS, "scope")

  def apply(elm: indexed.Elem): PomElem = {
    elm.resolvedName match {
      case ProjectEName              => new ProjectElem(elm)
      case GroupIdEName              => new GroupIdElem(elm)
      case ArtifactIdEName           => new ArtifactIdElem(elm)
      case VersionEName              => new VersionElem(elm)
      case PackagingEName            => new PackagingElem(elm)
      case ClassifierEName           => new ClassifierElem(elm)
      case PropertiesEName           => new PropertiesElem(elm)
      case ParentEName               => new ParentElem(elm)
      case RelativePathEName         => new RelativePathElem(elm)
      case NameEName                 => new NameElem(elm)
      case DescriptionEName          => new DescriptionElem(elm)
      case UrlEName                  => new UrlElem(elm)
      case ModelVersionEName         => new ModelVersionElem(elm)
      case DependenciesEName         => new DependenciesElem(elm)
      case DependencyManagementEName => new DependencyManagementElem(elm)
      case DependencyEName           => new DependencyElem(elm)
      case ScopeEName                => new ScopeElem(elm)
      case _                         => new OtherPomElem(elm)
    }
  }
}

object ProjectElem {

  def apply(elm: indexed.Elem): ProjectElem = {
    new ProjectElem(elm)
  }

  def apply(elm: simple.Elem, uriOption: Option[URI]): ProjectElem = {
    apply(indexed.Elem(uriOption, elm))
  }
}
