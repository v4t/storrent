package storrent

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import akka.actor.{ActorSystem, Props}

import akka.util.Timeout
import storrent.bencode.{BencodeParser, BencodeValue}
import storrent.metainfo.{MetaInfo, Torrent}

import scala.concurrent.ExecutionContextExecutor
import scala.io.{Codec, Source}
import scala.util.{Failure, Try}


object Main {

//  val port = 55555
//  val peerId = Random.alphanumeric.take(20).mkString("")

  def main(args: Array[String]) {
    if (args.isEmpty) {
      println("Usage: storrent [torrent file]")
      System.exit(0)
    }
    if (!Files.exists(Paths.get(args(0)))) {
      println("Given file does not exist")
      System.exit(0)
    }

    val file = args(0)
    val name = file.split('/').last.replace(".torrent", "")
    val torrent = Torrent.fromFile(file).get
//    val bencodeValues = parseSource(file).get
//    val metaInfo = MetaInfo.fromBencode(bencodeValues).get

    val system = ActorSystem("scala-torrent")

    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val client = system.actorOf(Props(classOf[Client], torrent, system), "client")
    client ! "start"

    Thread.sleep(30000)
    client ! "stop"
        /*system.terminate()*/
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

}
