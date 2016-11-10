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

package eu.cdevreeze.mvnscripting.files

import java.io.File

import scala.collection.immutable

/**
 * Utility to find (POM) files. This is a simple utility, not meant to be robust. (Otherwise it is better to use Java NIO 2.)
 *
 * @author Chris de Vreeze
 */
object FileUtil {

  /**
   * Finds the normal files obeying the given predicate anywhere in the tree rooted at the given root directory.
   */
  def findFiles(rootDir: File, p: File => Boolean): immutable.IndexedSeq[File] = {
    require(rootDir.isDirectory, s"$rootDir must be a directory")

    rootDir.listFiles.toIndexedSeq flatMap {
      case d: File if d.isDirectory =>
        // Recursive call
        findFiles(d, p)
      case f: File if f.isFile && p(f) =>
        immutable.IndexedSeq(f)
      case _ =>
        immutable.IndexedSeq()
    }
  }

  /**
   * Finds the POM files with name "pom.xml" anywhere in the tree rooted at the given root directory.
   */
  def findPomFiles(rootDir: File): immutable.IndexedSeq[File] = {
    findFiles(rootDir, (_.getName == "pom.xml"))
  }
}
