/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package wvlet.airspec

import sbt.testing.Fingerprint
import wvlet.airframe.surface.MethodSurface
import wvlet.airspec.spi.Asserts
import scala.concurrent.ExecutionContext

/**
  * An interface for compatibility between Scala JVM and Scala.js
  */
trait CompatApi {
  def isScalaJVM: Boolean
  def isScalaJs: Boolean
  def isScalaNative: Boolean

  private[airspec] def executionContext: ExecutionContext
  private[airspec] def findCompanionObjectOf(fullyQualifiedName: String, classLoader: ClassLoader): Option[Any]
  private[airspec] def getFingerprint(fullyQualifiedName: String, classLoader: ClassLoader): Option[Fingerprint]
  private[airspec] def newInstanceOf(fullyQualifiedName: String, classLoader: ClassLoader): Option[Any]
  private[airspec] def withLogScanner[U](block: => U): U
  private[airspec] def startLogScanner: Unit
  private[airspec] def stopLogScanner: Unit
  private[airspec] def findCause(e: Throwable): Throwable

  private[airspec] def getSpecName(cls: Class[_]): String

  private[airspec] def getContextClassLoader: ClassLoader

  private[airspec] def platformSpecificMatcher: PartialFunction[(Any, Any), Asserts.TestResult] = PartialFunction.empty
  private[airspec] def platformSpecificPrinter: PartialFunction[Any, String]                    = PartialFunction.empty
}
