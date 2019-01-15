package torvi

import akka.actor.Actor

case class StartDownload(torrentFile: String)

class STorrent extends Actor {
  def receive = {
    case StartDownload(f) =>
      println(s"$f")
  }

}
