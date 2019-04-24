package storrent

import akka.actor.Actor
import storrent.bencode.BencodeParser
import storrent.metainfo.{MetaInfo, MetaInfoException}

import scala.io.{Codec, Source}
import scala.util.{Failure, Success}

case class StartDownload(torrentFile: String)

class STorrent extends Actor {
  def receive = {
    case StartDownload(torrentFile) => {

      val source = Source.fromFile(torrentFile)(Codec.ISO8859)
      val contents = source.mkString
      source.close
      val bencodeValues = BencodeParser.parse(contents)
      val metaInfo = MetaInfo.fromBencode(bencodeValues)
      metaInfo match {
        case Success(x) => println("succ " + x)
        case Failure(MetaInfoException(msg)) => println("metainfo error " + msg)
        case Failure(f) => println("some other error " + f)
      }
    }
  }
}
