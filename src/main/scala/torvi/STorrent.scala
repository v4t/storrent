package torvi

import akka.actor.Actor
import torvi.Bencode.BencodeParser

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
      println(bencodeValues)
    }

  }

}
