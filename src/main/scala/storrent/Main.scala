package storrent

import java.io.File
import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import storrent.metainfo.Torrent

import scala.concurrent.ExecutionContextExecutor


object Main {

//  val port = 55555
//  val peerId = Random.alphanumeric.take(20).mkString("")

  def main(args: Array[String]) {
    if (args.isEmpty) {
      println("Usage: storrent [torrent file]")
      System.exit(1)
    }
    if (!Files.exists(Paths.get(args(0)))) {
      println("Given file does not exist")
      System.exit(1)
    }

    val file = args(0)
    val name = file.split('/').last.replace(".torrent", "")
    val torrent = Torrent.fromFile(file).get
//    val bencodeValues = parseSource(file).get
//    val metaInfo = MetaInfo.fromBencode(bencodeValues).get

    println("default block size", torrent.defaultBlockSize)
    println("pieceSize", torrent.pieceSize(0))
    println("pieceblock", torrent.blockSize(0, 0))

//    println(torrent.metaInfo.toString)

    val value = ConfigFactory.load().getString("storrent.test")
    println(s"storrent.test = $value")

    val conf = ConfigFactory.load()
    val system = ActorSystem("scala-torrent", conf)
//    system.logConfiguration()

    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val client = system.actorOf(Props(classOf[Client], torrent, system), "client")
    client ! "start"

//    Thread.sleep(30000)
    client ! "stop"
        /*system.terminate()*/
  }
}
