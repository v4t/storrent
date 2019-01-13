package torvi
import akka.actor.{Props, ActorSystem}

class Main {
  def main(args: Array[String]) {
    if (args.isEmpty) {
      println("Usage: storrent [torrent file]")
      System.exit(0)
    }

    val system = ActorSystem("scala-torrent")
    
  }
}
