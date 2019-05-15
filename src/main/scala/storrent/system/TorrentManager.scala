package storrent.system

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

import akka.actor.Actor
import storrent.bencode.{BencodeParser, BencodeValue}
import storrent.metainfo.MetaInfo
import storrent.peers.Handshake
import storrent.system.TorrentManager.{StartDownload, StartDownloadSuccess}
import storrent.tracker._

import scala.io.{Codec, Source}
import scala.util.{Failure, Try}

object TorrentManager {

  case class StartDownload(torrentFile: String)

  case class StartDownloadSuccess(torrentFile: String)

  case class StartDownloadFailure(torrentFile: String, message: String)

}

class TorrentManager extends Actor {

  val port = 6881
  val peerId = "-ST-001-123456654321" //"-ST-001-" + Random.alphanumeric.take(12).mkString("")

  def receive = {
    case StartDownload(file) => {

      val bencodeValues = parseSource(file).get
      val metaInfo = MetaInfo.fromBencode(bencodeValues).get

      val y = for {
        src <- parseSource(file)
        info <- MetaInfo.fromBencode(src)
        tr <- Try(populateTrackerRequest(info, Started))
      } yield tr


      val req = y.get

      val requestUrl = metaInfo.announceList.head + "?" + TrackerRequest.getQueryString(req)

      // Make tracker request
      import scalaj.http._
      val response: HttpResponse[Array[Byte]] = Http(requestUrl).asBytes
      val resStr = new String(response.body, StandardCharsets.ISO_8859_1)

      val res = TrackerResponse.parse(resStr)
      val suc = res match {
        case r@SuccessResponse(interval, peers, _, _, _, _, _) => r
        case FailureResponse(msg) => println(msg) //throw new Exception(msg)
      }
      val hs = Handshake.serialize(Handshake(infoHash = metaInfo.infoHash, peerId = peerId))
      println("hs " + new String(hs, StandardCharsets.ISO_8859_1))
      sender ! StartDownloadSuccess(file)
    }
    case _ => println("unknown message")
  }

  private def parseSource(filePath: String): Try[List[BencodeValue]] = {
    lazy val source = Source.fromFile(filePath)(Codec.ISO8859)
    try {
      BencodeParser.parse(source.mkString)
    }
    catch {
      case e: Exception => Failure(e)
    }
    finally {
      source.close
    }
  }

  private def populateTrackerRequest(metaInfo: MetaInfo, event: TrackerEvent): TrackerRequest =
    TrackerRequest(
      infoHash = URLEncoder.encode(new String(metaInfo.infoHash, "ISO-8859-1"), "ISO-8859-1"),
      uploaded = 0,
      downloaded = 0,
      left = 0,
      peerId = URLEncoder.encode(peerId, "ISO-8859-1"),
      port = port,
      compact = 1,
      event = Some(event),
      numWant = Some(40),
      ip = None
    )

}