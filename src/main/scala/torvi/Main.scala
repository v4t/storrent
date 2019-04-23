package torvi

import java.nio.file.{Files, Paths}

import akka.actor.{ActorSystem, Props}
import torvi.bencode.BencodeParser

import scala.io.{Codec, Source}

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

    torrent ! StartDownload(args(0))
    system.terminate()
  }
}
