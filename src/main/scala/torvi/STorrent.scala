package torvi

import akka.actor.Actor
import torvi.bencode.{BencodeDict, BencodeInt, BencodeList, BencodeParser, BencodeString, BencodeValue}
import torvi.metainfo.{MetaInfo, MetaInfoDictionary, MetaInfoException}

import scala.io.{Codec, Source}

case class StartDownload(torrentFile: String)

class STorrent extends Actor {
  def receive = {
    case StartDownload(torrentFile) => {
      println(torrentFile)
      val source = Source.fromFile(torrentFile)(Codec.ISO8859)
      val contents = source.mkString
      source.close
      val bencodeValues = BencodeParser.parse(contents)
      val metaInfo = MetaInfo.fromBencode(bencodeValues)

      metaInfo match {
        case Some(x) => println(x)
        case None => println("No metainfo :(")
      }
    }
  }

}
