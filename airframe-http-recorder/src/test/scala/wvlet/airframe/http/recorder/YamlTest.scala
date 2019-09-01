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
package wvlet.airframe.http.recorder

import java.time.Instant

import org.yaml.snakeyaml.DumperOptions.{FlowStyle, LineBreak, ScalarStyle}
import org.yaml.snakeyaml.{DumperOptions, Yaml}
import wvlet.airframe.codec.CollectionCodec.MapCodec
import wvlet.airframe.codec.{MessageCodec, MessageCodecFactory}
import wvlet.airframe.surface.Surface
import wvlet.airspec.AirSpec

import scala.collection.JavaConverters._

class YamlTest extends AirSpec {

  def `test`: Unit = {
    val records = Seq(
      HttpRecord(
        session = "test",
        requestHash = 123,
        method = "get",
        destHost = "localhost",
        path = "/",
        requestHeader = Seq("Content-Type" -> "application/json"),
        requestBody = "test",
        responseCode = 200,
        responseHeader = Nil,
        responseBody = "test",
        createdAt = System.currentTimeMillis()
      )
    )

    val codecFactory = MessageCodecFactory.defaultFactory.withObjectMapCodec
    val objCodec = codecFactory.of[Seq[HttpRecord]]
    val data = objCodec.toMsgPack(records)

    val mapCodec = codecFactory.of[Seq[Map[String, Any]]]
    val java = toJava(mapCodec.unpack(data))

    val options = new DumperOptions()
    val yaml = new Yaml(options).dump(java)

    println(yaml)

  }

  private def toJava(obj: Any): Any = {
    obj match {
      case x: Seq[_]    => x.map(toJava).asJava
      case x: Map[_, _] => x.map { case (key, value) => key -> toJava(value) }.asJava
      case x            => x
    }
  }

}
