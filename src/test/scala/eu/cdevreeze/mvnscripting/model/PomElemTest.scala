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

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import eu.cdevreeze.yaidom.parse.DocumentParserUsingSax

/**
 * Test case for [[eu.cdevreeze.mvnscripting.model.PomElem]].
 *
 * @author Chris de Vreeze
 */
@RunWith(classOf[JUnitRunner])
class PomElemTest extends FunSuite {

  test("testQueryPom") {
    val docParser = DocumentParserUsingSax.newInstance()
    val docUri = classOf[PomElemTest].getResource("sample-pom.xml").toURI
    val doc = docParser.parse(docUri)

    val projectElem: ProjectElem = ProjectElem(doc.documentElement, doc.uriOption)

    val propertyMap = projectElem.propertyMap

    assertResult(PomElem.ProjectEName) {
      projectElem.resolvedName
    }

    assertResult(Some("4.0.0")) {
      projectElem.modelVersionOption.map(_.resolvedValue(propertyMap))
    }

    assertResult(Some("org.sonatype.mavenbook.simple")) {
      projectElem.groupIdOption.map(_.resolvedValue(propertyMap))
    }
    assertResult(Some("simple")) {
      projectElem.artifactIdOption.map(_.resolvedValue(propertyMap))
    }
    assertResult(Some("1.0-SNAPSHOT")) {
      projectElem.versionOption.map(_.resolvedValue(propertyMap))
    }
    assertResult(Some("jar")) {
      projectElem.packagingOption.map(_.resolvedValue(propertyMap))
    }

    assertResult(Some("simple")) {
      projectElem.nameOption.map(_.resolvedValue(propertyMap))
    }
    assertResult(Some("http://maven.apache.org")) {
      projectElem.urlOption.map(_.resolvedValue(propertyMap))
    }

    assertResult(None) {
      projectElem.parentOption
    }

    assertResult(None) {
      projectElem.propertiesOption
    }

    val dependencies = projectElem.dependenciesOption.toIndexedSeq.flatMap(_.dependencies)

    assertResult(1) {
      dependencies.size
    }

    val firstDependency = dependencies.head

    assertResult(Some("junit")) {
      firstDependency.groupIdOption.map(e => e.resolvedValue(e.propertyMapInProject))
    }
    assertResult(Some("junit")) {
      firstDependency.artifactIdOption.map(e => e.resolvedValue(e.propertyMapInProject))
    }
    assertResult(Some("3.8.1")) {
      firstDependency.versionOption.map(e => e.resolvedValue(e.propertyMapInProject))
    }
    assertResult(Some("test")) {
      firstDependency.scopeOption.map(e => e.resolvedValue(e.propertyMapInProject))
    }

    assertResult(None) {
      projectElem.dependencyManagementOption
    }
  }
}
