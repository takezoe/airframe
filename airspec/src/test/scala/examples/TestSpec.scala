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
package examples

import wvlet.airframe.Design
import wvlet.airspec.spi.AirSpecException
import wvlet.airspec.AirSpec
import wvlet.log.LogSupport

/**
  */
class TestSpec extends AirSpec with LogSupport {

  protected override def design: Design = {
    Design.newDesign
      .bind[String].toInstance("my message")
  }

  test("helloAirSpec") { (m: String) =>
    trace(m)
    assert(m == "my message")
  }

  test("support free-style test description") {
    trace("hello")
  }

  test("should support assertion") {
    intercept[AirSpecException] {
      assert(false, "failure")
    }
  }

  test("should support skipping") {
    skip("unnecessary test")
  }

  test("should support cancel") {
    cancel("failed to access a necessary service")
  }

  test("should support pending") {
    pendingUntil("fixing #012")
  }
  test("should support pending with a reason") {
    pending("pending reason")
  }

  test("interceptTest") {
    intercept[NoSuchElementException] {
      Seq.empty.head
    }
  }
}

object TestObjSpec extends AirSpec with LogSupport {
  test("supportTestsInObjectMethods") {
    trace("hello companion methods")
    "hello obj"
  }
}

class WordSpecTest extends AirSpec {
  test("should have a natural language description") {
    true
  }
  test("should support arbitrary texts") {
    true
  }
}
