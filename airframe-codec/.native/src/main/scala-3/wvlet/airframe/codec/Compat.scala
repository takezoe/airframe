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
package wvlet.airframe.codec
import java.time.Instant
import java.util.UUID

import wvlet.airframe.surface.Surface

import scala.util.Try

/**
  */
object Compat:
  def messageCodecFinder: MessageCodecFinder                = MessageCodecFinder.defaultMessageCodecFinder
  def platformSpecificCodecs: Map[Surface, MessageCodec[_]] = Map.empty

  def codecOfClass(
      cl: Class[?],
      codecFactory: MessageCodecFactory = MessageCodecFactory.defaultFactoryForJSON
  ): Option[MessageCodec[_]] = None

  private[codec] def parseInstant(s: String): Option[Instant] =
    Try(Instant.parse(s)).toOption

  def readUUIDFromBytes(data: Array[Byte]): UUID =
    throw new IllegalArgumentException("Reading binary UUID is not supported in Scala Native")
