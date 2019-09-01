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

import java.{lang => jl, util => ju}
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import java.util.{Base64, Locale}

import com.twitter.finagle.http.{Request, Response}
import com.twitter.io.Buf
import org.yaml.snakeyaml.Yaml
//import wvlet.airframe.config.YamlReader
//import wvlet.airframe.config.YamlReader.{bindMap, loadMapOf, loadYaml, trace}
//import wvlet.airframe.jdbc.{DbConfig, SQLiteConnectionPool}
//import wvlet.airframe.metrics.TimeWindow
//import wvlet.airframe.surface.Surface
import wvlet.log.LogSupport
import wvlet.log.io.IOUtil.readAsString

import scala.collection.immutable.ListMap
import scala.collection.JavaConverters._

object YamlTest extends App {
  println(new Yaml().represent(List("aaa", "bbb", "ccc").asJava))
}

/**
  * Recorder for HTTP server responses
  */
class HttpRecordStore(val recorderConfig: HttpRecorderConfig, dropSession: Boolean = false, inMemory: Boolean = false)
    extends AutoCloseable
    with LogSupport {
  private val requestCounter = scala.collection.mutable.Map.empty[Int, AtomicInteger]
  private val records: Seq[HttpRecord] = Nil

  init

  protected def init {
    // load from file
    loadYaml(recorderConfig.storageFolder + "/" + recorderConfig.sessionName + ".yaml")

    // TODO: Detect schema change
    if (dropSession) {
      clearSession
    }
  }

  private def loadYaml(resourcePath: String): Unit = {
//    val yaml = new Yaml().load(readAsString(resourcePath)).asInstanceOf[ju.Map[AnyRef, AnyRef]].asScala.toMap
//    println(yaml)
//    val surface: Surface = wvlet.airframe.surface.Surface.of[A]
//    val map              = ListMap.newBuilder[String, A]
//    for ((k, v) <- yaml) yield {
//      map += k.toString -> bindMap[A](surface, v.asInstanceOf[ju.Map[AnyRef, AnyRef]].asScala.toMap)
//    }
//    map.result
  }

  private def saveYaml(resourcePath: String): Unit = {
    println(new Yaml().represent(List("aaa", "bbb", "ccc").asJava))
  }

  def clearSession: Unit = {
    // TODO delete yaml file
//    warn(s"Deleting old session records for session:${recorderConfig.sessionName}")
//    connectionPool.executeUpdate(s"delete from ${recordTableName} where session = '${recorderConfig.sessionName}'")
  }

  def resetCounter: Unit = {
    requestCounter.clear()
  }

  def numRecordsInSession: Long = {
    records.size
  }

  def findNext(request: Request, incrementHitCount: Boolean = true): Option[HttpRecord] = {
    val rh = recorderConfig.requestMatcher.computeHash(request)

    // If there are multiple records for the same request, use the counter to find
    // n-th request, where n is the access count to the same path
    val counter  = requestCounter.getOrElseUpdate(rh, new AtomicInteger())
    val hitCount = if (incrementHitCount) counter.getAndIncrement() else counter.get()
    trace(s"findNext: request hash: ${rh} for ${request}, hitCount: ${hitCount}")

    records.sortBy(_.createdAt).filter(_.requestHash == rh).drop(hitCount).headOption
  }

  def record(request: Request, response: Response): Unit = {
    val rh = recorderConfig.requestMatcher.computeHash(request)

    val httpHeadersForRecording: Seq[(String, String)] =
      request.headerMap.toSeq.filterNot { x =>
        recorderConfig.excludeHeaderForRecording(x._1, x._2)
      }
    val entry = HttpRecord(
      recorderConfig.sessionName,
      requestHash = rh,
      method = request.method.toString(),
      destHost = recorderConfig.destAddress.hostAndPort,
      path = request.uri,
      requestHeader = httpHeadersForRecording,
      requestBody = HttpRecordStore.encodeToBase64(request.content),
      responseCode = response.statusCode,
      responseHeader = response.headerMap.toSeq,
      responseBody = HttpRecordStore.encodeToBase64(response.content),
      createdAt = Instant.now()
    )

    trace(s"record: request hash ${rh} for ${request} -> ${entry.summary}")
    // TODO save
//    connectionPool.withConnection { conn =>
//      entry.insertInto(recordTableName, conn)
//    }
  }

  override def close(): Unit = {
//    connectionPool.stop
  }

}

object HttpRecordStore {

  def encodeToBase64(content: Buf): String = {
    val buf = new Array[Byte](content.length)
    content.write(buf, 0)

    val encoder = Base64.getEncoder
    encoder.encodeToString(buf)
  }

  def decodeFromBase64(base64String: String): Array[Byte] = {
    val decoder = Base64.getDecoder
    decoder.decode(base64String)
  }
}
