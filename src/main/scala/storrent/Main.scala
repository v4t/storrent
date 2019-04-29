package storrent

gimport java.net.URLEncoder
import java.nio.file.{Files, Paths}

import akka.actor.{ActorSystem, Props}
import storrent.bencode.BencodeParser
import storrent.metainfo.{MetaInfo, MetaInfoException}
import storrent.tracker.TrackerRequest

import scala.io.{Codec, Source}
import scala.util.{Failure, Random, Success}

object Main {

  def main(args: Array[String]) {
    if (args.isEmpty) {
      println("Usage: storrent [torrent file]")
      System.exit(0)
    }

    if (!Files.exists(Paths.get(args(0)))) {
      println("Given file does not exist")
      System.exit(0)
    }
    val system = ActorSystem("scala-torrent")
    val torrent = system.actorOf(Props[STorrent], "STorrent")

    // Parse torrent file
    val torrentFile = args(0)
    val source = Source.fromFile(torrentFile)(Codec.ISO8859)
    val contents = source.mkString
    source.close
    val bencodeValues = BencodeParser.parse(contents)

    // Create metainfo
    val metaInfo = MetaInfo.fromBencode(bencodeValues) match {
      case Success(x) => x;
      case Failure(MetaInfoException(msg)) => throw MetaInfoException("metainfo error " + msg)
      case Failure(f) => throw f
    }

    // Create tracker request payload
    val port = 6881
    val peerId = "-ST-001-" + Random.alphanumeric.take(12).mkString("")
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
    val response: HttpResponse[String] = Http(requestUrl).asString
    println(response.body)


    torrent ! StartDownload("start download")
    system.terminate()
  }

}
