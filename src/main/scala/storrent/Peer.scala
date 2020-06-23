package storrent

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import storrent.torrent.Torrent
import storrent.peerprotocol._
import storrent.trackerprotocol.PeerInfo

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

class Peer(peer: PeerInfo, torrent: Torrent, localId: String, client: ActorRef) extends Actor with ActorLogging{

  import Tcp._
  import context.system

  val remote = new InetSocketAddress(peer.ip, peer.port)
  val peerBitfield: Array[Boolean] = new Array(torrent.pieceCount)
  val tcpBuffer: ArrayBuffer[Byte] = ArrayBuffer[Byte]()

  var choking = true
  var peerChoking = true

  var interested = false
  var peerInterested = false

  IO(Tcp) ! Connect(remote, timeout = Some(30.seconds))

  def receive = {
    case CommandFailed(_: Connect) =>
      client ! PeerDisconnected(peer)
      context.stop(self)

    case c@Connected(remote, local) =>
      val connection = sender()
      connection ! Register(self)
      context.become(waitingHandshake(connection))
      connection ! Write(ByteString(Handshake.encode(torrent.metaInfo.infoHash, localId)))
  }

  def waitingHandshake(connection: ActorRef): Receive = {
    case Received(data) =>
      val handshakeBytes = data.take(68).toArray
      if (data.length > 68) {
        tcpBuffer ++= data.drop(68)
      }
      if (Handshake.decode(handshakeBytes.toArray).isEmpty) {
        log.debug(s"[${peer.peerId}]: Failed to decode handshake")
        self ! "close"
      } else {
        context.become(active(connection))
        client ! PeerConnected(peer, self)
        connection ! Interested.encode()
        parseMessagesFromBuffer()
      }

    case "close" => connection ! Close

    case _: ConnectionClosed => context.stop(self)
  }

  def active(connection: ActorRef): Receive = {
    case CommandFailed(w: Write) =>
      client ! "write failed"

    case Received(data) =>
      tcpBuffer ++= data
      parseMessagesFromBuffer()

    case r@Request(index, begin, length) =>
      if (!peerChoking && peerBitfield(index)) {
        val bytes = Request.encode(index, begin, length)
        connection ! Write(ByteString(bytes))
      } else {
        log.debug(s"[${peer.peerId}]: Failed to request block")
        sender ! RequestBlockFailed(r, peer)
      }

    case "close" =>
      log.debug(s"[${peer.peerId}]: close connection")
      connection ! Close

    case _: ConnectionClosed =>
      client ! PeerDisconnected(peer)
      context.stop(self)
  }

  @tailrec
  private def parseMessagesFromBuffer(): Unit = {
    val lengthPrefix = Message.decodeLength(tcpBuffer)
    if (lengthPrefix.isDefined && tcpBuffer.length >= 4 + lengthPrefix.get) {
      val messageLength = 4 + lengthPrefix.get
      handleMessage(tcpBuffer.take(messageLength).toArray)
      tcpBuffer.remove(0, messageLength)
      parseMessagesFromBuffer()
    }
  }

  private def handleMessage(message: Array[Byte]): Unit = {
    val msg = Message.decode(message);
    if (msg.isEmpty) {
      log.debug(s"[${peer.peerId}]: Received unknown message of " + message.length)
    }
    else msg.get match {
      case KeepAlive() => handleKeepAlive()
      case Choke() => handleChoke()
      case Unchoke() => handleUnchoke()
      case Interested() => handleInterested()
      case NotInterested() => handleNotInterested()
      case Have(index) => handleHave(index)
      case Bitfield(dp) => handleBitfield(dp)
      case Request(index, begin, length) => handleRequest(index, begin, length)
      case piece@Piece(_, _, _) => handlePiece(piece)
      case Cancel(index, begin, length) => handleCancel(index, begin, length)
      case Port(listenPort) => handlePort(listenPort)
    }
  }

  private def handleKeepAlive(): Unit = {
    log.debug(s"[${peer.peerId}]: Received keepalive")
  }

  private def handleChoke(): Unit = {
    log.debug(s"[${peer.peerId}]: Received choke")
    peerChoking = true
    client ! ChokeReceived(peer)
  }

  private def handleUnchoke(): Unit = {
    log.debug(s"[${peer.peerId}]: Received unchoke")
    peerChoking = false
    client ! UnchokeReceived(peer)
  }

  private def handleInterested(): Unit = {
    log.debug(s"[${peer.peerId}]: Received interested")
    peerInterested = true
  }

  private def handleNotInterested(): Unit = {
    log.debug(s"[${peer.peerId}]: Received not interested")
    peerInterested = false
  }

  private def handleHave(index: Int): Unit = {
    log.debug(s"[${peer.peerId}]: Received have")
    if (peerBitfield.isDefinedAt(index)) peerBitfield(index) = true
  }

  private def handleBitfield(bitfield: Array[Boolean]): Unit = {
    // Fail if bitfield is not of the correct size, or if the bitfield has any of the spare bits set.
    if (bitfield.length != peerBitfield.length && bitfield.drop(torrent.pieceCount).contains(true)) {
      log.debug(s"[${peer.peerId}]: Received invalid bitfield")
      self ! "close"
    } else {
      log.debug(s"[${peer.peerId}]: Received bitfield")
      for (i <- 0 until torrent.pieceCount) peerBitfield(i) = bitfield(i)
    }
  }

  private def bitsToByte(bits: Array[Boolean]): Byte =
    bits.foldLeft(0)((acc, bit) => if (bit) (acc << 1) | 1 else acc << 1).toByte

  private def handleRequest(index: Int, begin: Int, length: Int): Unit = {
    log.debug(s"[${peer.peerId}]: Received request")
  }

  private def handlePiece(piece: Piece): Unit = {
    log.debug(s"[${peer.peerId}]: Received piece")
    client ! BlockReceived(piece, peer)
  }

  private def handleCancel(index: Int, begin: Int, length: Int): Unit = {
    log.debug(s"[${peer.peerId}]: Received cancel")
  }

  private def handlePort(listenPort: Int): Unit = {
    log.debug(s"[${peer.peerId}]: Received port")
  }
}