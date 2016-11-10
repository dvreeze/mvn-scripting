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
          val propertyMap = getPropertyMap(ProjectElem(d.underlyingElem.rootElem))
          d.versionOption.exists(v => elemValueContains(v, "SNAPSHOT", propertyMap))
        }

      filteredDependencies foreach { dependency =>
        println()
        println(s"Snapshot dependency in file ${dependency.underlyingElem.docUri}:\n${dependency.underlyingElem.underlyingElem}")
      }
    }

    private def getPropertyMap(projectElem: ProjectElem): Map[String, String] = {
      val propElems = projectElem.propertiesOption.toIndexedSeq.flatMap(_.properties)
      propElems.map(e => (e.resolvedName.localPart -> e.text)).toMap
    }

    // Code smell, of course. We need to model property content.

    private def elemValueContains(elm: PomElem, value: String, propertyMap: Map[String, String]): Boolean = {
      val text = elm.text

      if (text.startsWith("${") && text.endsWith("}")) {
        val propKey = text.drop(2).dropRight(1)
        propertyMap.get(propKey).exists(_.contains(value))
      } else {
        text.contains(value)
      }
    }
  }
}
