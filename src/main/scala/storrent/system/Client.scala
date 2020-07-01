package storrent.system

import java.io.File
import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import storrent.system.messages.Client._
import storrent.system.messages.Downloader.{AddPeer, RemovePeer}
import storrent.system.messages.Tracker.Update
import storrent.torrent.Torrent
import storrent.trackerprotocol.{Completed, Started, Stopped}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Random

class Client(torrent: Torrent, port: Int, system: ActorSystem) extends Actor with ActorLogging {
  private val localId = Random.alphanumeric.take(20).mkString("")
  private val peerActorMap = mutable.Map[String, ActorRef]()

  private var complete = false;

  private val listener = context.actorOf(
    Props(classOf[ConnectionListener], new InetSocketAddress(port), system),
    "listener"
  )

  private val tracker = context.actorOf(
    Props(classOf[Tracker], localId, port),
    "tracker"
  )

  private val downloader = context.actorOf(
    Props(classOf[Downloader], self, torrent),
    "downloader"
  )

  def receive: Receive = {
    case StopClient =>
      tracker ! Update(torrent, Some(Stopped))
      if (!complete) {
        println("Client closed before completing download. Removing incomplete files.")
        cleanUpFiles()
      }
      system.terminate()

    case StartClient =>
      tracker ! Update(torrent, Some(Started))

    case Complete =>
      complete = true
      tracker ! Update(torrent, Some(Completed))
      println("Download complete, shutting down...")
      self ! StopClient

    case UpdatePeersFromTracker(peerList, interval) =>
      peerList.foreach(p => context.actorOf(
        Props(classOf[Peer], p, torrent, localId, self),
        "peer-" + p.ip + "-" + Random.alphanumeric.take(5).mkString("")
      ))
      context.system.scheduler.scheduleOnce(interval seconds, self, Update(torrent, None))(system.dispatcher)

    case PeerConnected(peer, actor) =>
      log.info(peer.peerId + ": connected")
      peerActorMap.put(peer.peerId, actor)

    case PeerDisconnected(peer) =>
      log.debug(peer.peerId + ": disconnected")
      if (peerActorMap.isDefinedAt(peer.peerId)) peerActorMap.remove(peer.peerId)
      downloader ! RemovePeer(peer)

    case ChokeReceived(peer) =>
      downloader ! RemovePeer(peer)

    case UnchokeReceived(peer) =>
      downloader ! AddPeer(peer)

    case RequestBlock(request, peer) =>
      if (peerActorMap.contains(peer.peerId)) {
        peerActorMap(peer.peerId) ! request
        log.debug(s"Requesting block ${request.index}:${request.begin} from peer ${peer.peerId}")
      }

    case msg@RequestBlockFailed(request, peer) =>
      downloader ! msg

    case msg@BlockReceived(piece, peer) =>
      downloader ! msg

  }

  /**
   * Delete files specified in torrent file.
   */
  private def cleanUpFiles(): Unit = {
    def recursiveDelete(file: File): Unit = {
      if (file.isDirectory) {
        file.listFiles.foreach(recursiveDelete)
      }
      if (file.exists()) file.delete()
    }
    if (torrent.files.length > 1) {
      recursiveDelete(new File(torrent.downloadPath))
    }
    else {
      new File(torrent.filePath(torrent.files.head)).delete()
    }
  }
}
