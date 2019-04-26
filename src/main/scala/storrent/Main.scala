package storrent

import java.nio.file.{Files, Paths}

import akka.actor.{ActorSystem, Props}
import storrent.bencode.BencodeParser
import storrent.metainfo.{MetaInfo, MetaInfoException}

import scala.io.{Codec, Source}
import scala.util.{Failure, Success}

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

    // Parse torrentfile
    val torrentFile = args(0)
    val source = Source.fromFile(torrentFile)(Codec.ISO8859)
    val contents = source.mkString
    source.close
    val bencodeValues = BencodeParser.parse(contents)

    // Create metainfo
    val metaInfo = MetaInfo.fromBencode(bencodeValues)
    metaInfo match {
      case Success(x) => println("success " + x.info.pieces.length)
      case Failure(MetaInfoException(msg)) => println("metainfo error " + msg)
      case Failure(f) => println("some other error " + f)
    }

    torrent ! StartDownload("start download")
    system.terminate()
  }

}
