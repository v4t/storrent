package storrent

import java.nio.file.{Files, Paths}

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import storrent.system.TorrentManager
import storrent.system.TorrentManager.StartDownloadSuccess

import scala.concurrent.Future
import scala.concurrent.duration._
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
    val manager = system.actorOf(Props(classOf[TorrentManager]), "torrentmanager")

    implicit val timeout = Timeout(2.seconds)
    implicit val ec = system.dispatcher
    val f: Future[Any] = manager ? StartDownload(args(0))
    f.onComplete {
      case Success(msg) => msg match {
        case StartDownloadSuccess(torrentFile) => {
          println(s"started download for $torrentFile")
        }
      }
      case Failure(e) => println(e.getMessage)
    }
    system.terminate()
  }

}
