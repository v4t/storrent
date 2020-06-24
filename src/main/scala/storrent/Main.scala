package storrent

import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import storrent.system.Client
import storrent.torrent.Torrent

import scala.concurrent.ExecutionContextExecutor

object Main {

  def main(args: Array[String]) {
    if (args.length != 2) {
      println("Usage: storrent [torrent file] [download-path]")
      System.exit(1)
    }
    if (!Files.exists(Paths.get(args(0)))) {
      println("Given file does not exist")
      System.exit(1)
    }
    if (!Files.exists(Paths.get(args(1)))) {
      println("Invalid save directory path")
      System.exit(1)
    }
    val conf = ConfigFactory.load()

    val file = args(0)
    val name = file.split('/').last.replace(".torrent", "")
    val torrent = Torrent.fromFile(file, conf.getInt("storrent.block-size")).get
    val saveDir = args(1)
    val port = 56789


    val system = ActorSystem("storrent", conf)

    implicit val timeout: Timeout = Timeout(10, TimeUnit.SECONDS)
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val client = system.actorOf(Props(classOf[Client], torrent, port, saveDir, system), "client")
    client ! "start"

    Iterator.continually(scala.io.StdIn.readLine("> ")).takeWhile(_ != "q").foreach {
      case "foo" => println("bar")
      case _ => println("Unknown input")
    }

    client ! "stop"
    Thread.sleep(1000)
    system.terminate()
  }
}

