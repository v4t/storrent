package storrent.system

import java.net.URLEncoder

import akka.actor.{Actor, Props}
import storrent.bencode.{BencodeParser, BencodeValue}
import storrent.metainfo.MetaInfo
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

  import storrent.system.Tracker._

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

      val requestUrl = TrackerRequest.getQueryString(y.get)
      val tracker = context.actorOf(Props(classOf[Tracker]), "tracker")
      tracker ! SendTrackerRequest(requestUrl)
      sender ! StartDownloadSuccess(file)
    }
    case TrackerRequestFailure(_) => println("tracker failed")
    case TrackerRequestSuccess(_) => println("tracker success")
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
      baseUrl = metaInfo.announceList.head,
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