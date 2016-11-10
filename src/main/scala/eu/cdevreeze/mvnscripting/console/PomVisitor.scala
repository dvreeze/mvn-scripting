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

package eu.cdevreeze.mvnscripting.console

import java.io.File

import scala.collection.immutable
import scala.reflect.classTag

import eu.cdevreeze.mvnscripting.files.FileUtil
import eu.cdevreeze.mvnscripting.model.DependencyElem
import eu.cdevreeze.mvnscripting.model.PomElem
import eu.cdevreeze.mvnscripting.model.ProjectElem
import eu.cdevreeze.yaidom.core.EName
import eu.cdevreeze.yaidom.parse.DocumentParserUsingStax

/**
 * Console program finding POM files in a directory tree, parsing them, and performing some side-effecting operation.
 *
 * @author Chris de Vreeze
 */
object PomVisitor {

  type PomCollectionScript = (immutable.IndexedSeq[ProjectElem] => Unit)

  def main(args: Array[String]): Unit = {
    require(args.size == 2, s"Usage: PomVisitor <root dir> <script FQCN>")

    val rootDir = new File(args(0))
    require(rootDir.isDirectory, s"Not a directory: $rootDir")

    val scriptClass = Class.forName(args(1)).asInstanceOf[Class[_ <: PomCollectionScript]]
    val script = scriptClass.newInstance()

    val files = FileUtil.findPomFiles(rootDir)

    val docParser = DocumentParserUsingStax.newInstance()

    val docs = files.map(f => docParser.parse(f).withUriOption(Some(f.toURI)))

    val projectElems: immutable.IndexedSeq[ProjectElem] = docs.map(d => ProjectElem(d.documentElement, d.uriOption))

    script(projectElems)
  }

  final class PrintSnapshotDependencies extends PomCollectionScript {

    def apply(projectElems: immutable.IndexedSeq[ProjectElem]): Unit = {
      val allDependencies = projectElems.flatMap(_.findAllElemsOfType(classTag[DependencyElem]))

      val filteredDependencies =
        allDependencies filter { d =>
          d.versionOption.exists(v => v.resolvedValue(v.propertyMapInProject).contains("SNAPSHOT"))
        }

      filteredDependencies foreach { dependency =>
        println()
        println(s"Snapshot dependency in file ${dependency.underlyingElem.docUri}:\n${dependency.underlyingElem.underlyingElem}")
      }
    }
  }

  final class PrintPotentiallyConflictingDependencies extends PomCollectionScript {

    def apply(projectElems: immutable.IndexedSeq[ProjectElem]): Unit = {
      val allDependencies = projectElems.flatMap(_.findAllElemsOfType(classTag[DependencyElem]))

      val dependenciesHavingVersion = allDependencies.filter(_.versionOption.isDefined)

      val dependencyGroups =
        dependenciesHavingVersion groupBy { d =>
          val propertyMap = d.propertyMapInProject
          (d.groupIdOption.get.resolvedValue(propertyMap) -> d.artifactIdOption.get.resolvedValue(propertyMap))
        }

      val interestingDependencyGroups =
        dependencyGroups.filter(_._2.map(e => e.versionOption.get.resolvedValue(e.propertyMapInProject)).distinct.size >= 2).values.toVector

      interestingDependencyGroups foreach { dependencyGroup =>
        val versionValues = dependencyGroup.flatMap(_.versionOption).map(e => e.resolvedValue(e.propertyMapInProject))
        val groupId = dependencyGroup.head.groupIdOption.get
        val artifactId = dependencyGroup.head.artifactIdOption.get

        println()
        println(s"Artifact ${groupId.resolvedValue(groupId.propertyMapInProject)}:${artifactId.resolvedValue(artifactId.propertyMapInProject)} has versions ${versionValues.sorted.distinct.mkString(", ")}")
        println(s"\tThese artifacts occur in files ${dependencyGroup.map(_.underlyingElem.docUri).distinct.sortBy(_.toString).mkString(", ")}")
      }
    }
  }
}
