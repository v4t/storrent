package storrent.system

import java.net.{InetSocketAddress, URLEncoder}
import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorSystem}
import storrent.bencode.BencodeParser
import storrent.metainfo.{MetaInfo, MetaInfoException}
import storrent.peerwireprotocol.Handshake
import storrent.system.TorrentManager.{StartDownload, StartDownloadSuccess}
import storrent.tracker.{FailureResponse, SuccessResponse, TrackerRequest, TrackerResponse}

import scala.io.{Codec, Source}
import scala.util.{Failure, Random, Success}

object TorrentManager {

  case class StartDownload(torrentFile: String)

  case class StartDownloadSuccess(torrentFile: String)

  case class StartDownloadFailure(torrentFile: String, message: String)

}

class TorrentManager extends Actor {

  val port = 6881
  val peerId = "-ST-001-" + Random.alphanumeric.take(12).mkString("")

  def receive = {
    case StartDownload(file) => {
      // Parse torrent file
      val source = Source.fromFile(file)(Codec.ISO8859)
      val contents = source.mkString
      source.close
      val bencodeValues = BencodeParser.parse(contents)

      // Create metainfo
      val metaInfo = MetaInfo.fromBencode(bencodeValues) match {
        case Success(x) => x;
        case Failure(MetaInfoException(msg)) => throw MetaInfoException("metainfo error " + msg)
        case Failure(f) => throw f
      }

      val req = TrackerRequest(
        infoHash = URLEncoder.encode(new String(metaInfo.infoHash, "ISO-8859-1"), "ISO-8859-1"),
        uploaded = 0,
        downloaded = 0,
        left = 0,
        peerId = URLEncoder.encode(peerId, "ISO-8859-1"),
        port = port,
        compact = 1,
        event = None,
        numWant = Some(40),
        ip = None
      )

      val requestUrl = metaInfo.announceList.head + "?" + TrackerRequest.getQueryString(req)
      println(requestUrl)

      // Make tracker request
      import scalaj.http._
      val response: HttpResponse[Array[Byte]] = Http(requestUrl).asBytes

      val resStr = new String(response.body, StandardCharsets.ISO_8859_1)
      //println(resStr)
      val res = TrackerResponse.parse(resStr)
      val suc = res match {
        case r@SuccessResponse(interval, peers, _, _, _, _, _) => r
        case FailureResponse(msg) => throw new Exception(msg)
      }
      val hs = Handshake.serialize(Handshake(infoHash = metaInfo.infoHash, peerId = peerId))
      println(new String(hs, StandardCharsets.ISO_8859_1))
      sender ! StartDownloadSuccess(file)
    }
    case _ => println("unknown message")
  }

}