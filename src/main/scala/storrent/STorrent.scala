package storrent

import akka.actor.Actor
import storrent.metainfo.MetaInfo

case class StartDownload(torrentFile: String)

class STorrent extends Actor {
  def receive = {
    case StartDownload(torrentFile) => {
      val metaInfo = MetaInfo.fromFile(torrentFile)
      metaInfo match {
        case Some(x) => println(x)
        case None => println("No metainfo :(")
      }
    }
  }
}
