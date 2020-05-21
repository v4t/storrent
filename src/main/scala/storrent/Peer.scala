package storrent

import java.net.InetSocketAddress
import java.nio.ByteBuffer

import akka.actor.{Actor, ActorRef, ReceiveTimeout}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import storrent.metainfo.Torrent
import storrent.peers._
import storrent.tracker.PeerInfo

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

class Peer(peer: PeerInfo, torrent: Torrent, localId: String, client: ActorRef) extends Actor {

  import Tcp._
  import context.system

  val remote = new InetSocketAddress(peer.ip, peer.port)
  val peerBitfield: Array[Boolean] = new Array(torrent.pieceCount)
  val blockBuffer = ArrayBuffer[Byte]()


  var choking = true
  var peerChoking = true

  var interested = false
  var peerInterested = false

  IO(Tcp) ! Connect(remote, timeout = Some(30.seconds))

  def receive = {
    case CommandFailed(_: Connect) =>
      client ! "Failed to connect with peer"
      context.stop(self)

    case c@Connected(remote, local) =>
      val connection = sender()
      connection ! Register(self)
      context.become(waitingHandshake(connection))
      connection ! Write(ByteString(Handshake.encode(torrent.metaInfo.infoHash, localId)))
  }

  def waitingHandshake(connection: ActorRef): Receive = {
    case Received(data) =>
      if (Handshake.decode(data.toArray).isEmpty) {
        println(peer.peerId + ": Failed to decode handshake")
        self ! "close"
      } else {
        context.become(active(connection))
        client ! PeerConnected(peer, self)
        connection ! Interested.encode()
      }

    case "close" => connection ! Close

    case _: ConnectionClosed => context.stop(self)
  }

  def active(connection: ActorRef): Receive = {
    case CommandFailed(w: Write) =>
      client ! "write failed"

    case Received(data) =>
      handleReceivedData(data, connection)

    case Request(index, begin, length) =>
      if (!peerChoking && peerBitfield(index)) {
        val bytes = Request.encode(index, begin, length)
        connection ! Write(ByteString(bytes))
        println(peer.peerId + ": Requested block " + index)
      } else {
        println(peer.peerId + ": Failed to request block. choke: " + peerChoking + " & bf: " + peerBitfield(index))
        sender ! RequestBlockFailed(index, peer)
      }

    case "close" =>
      println(peer.peerId + ": close connection")
      connection ! Close

    case _: ConnectionClosed =>
      client ! PeerDisconnected(peer)
      context.stop(self)
  }

  def buffering(piece: Piece, blockSize: Int, connection: ActorRef): Receive = {
    case Received(data) =>
      blockBuffer ++= data
      println("Buffered " + blockBuffer.length + " bytes for piece " + piece.index)
      if (blockBuffer.length == blockSize) self ! "complete"
      else if (blockBuffer.length > blockSize) self ! "overflow"

    case "complete" =>
      val completePiece = Piece(piece.index, piece.begin, blockBuffer.toArray)
      client ! BlockReceived(completePiece, peer)
      context.become(active(connection))

    case "overflow" =>
      println("Block buffer overflow: " + blockBuffer.length)
      val completePiece = Piece(piece.index, piece.begin, blockBuffer.take(blockSize).toArray)
      client ! BlockReceived(completePiece, peer)
      context.become(active(connection))
      val remainingBytes = ByteString.fromArray(blockBuffer.drop(blockSize).toArray)
      handleReceivedData(remainingBytes, connection)

    case "close" => connection ! Close

    case _: ConnectionClosed => context.stop(self)
  }

  private def handleReceivedData(data: ByteString, connection: ActorRef): Unit = {
    val msg = Message.decode(data.toArray);
    if (msg.isEmpty) {
      println(peer.peerId + ": Received unknown message of " + data.length + " bytes. Message id: " + (data(4) & 0xff))
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
      case piece@Piece(_, _, _) => handlePiece(piece, data, connection)
      case Cancel(index, begin, length) => handleCancel(index, begin, length)
      case Port(listenPort) => handlePort(listenPort)
    }
  }

  private def handleKeepAlive(): Unit = {
    println(peer.peerId + ": Received keepalive")
  }

  private def handleChoke(): Unit = {
    println(peer.peerId + ": Received choke")
    peerChoking = true
    client ! ChokeReceived(peer)
  }

  private def handleUnchoke(): Unit = {
    println(peer.peerId + ": Received unchoke")
    peerChoking = false
    client ! UnchokeReceived(peer)
  }

  private def handleInterested(): Unit = {
    println(peer.peerId + ": Received interested")
    peerInterested = true
  }

  private def handleNotInterested(): Unit = {
    println(peer.peerId + ": Received not interested")
    peerInterested = false
  }

  private def handleHave(index: Int): Unit = {
    println(peer.peerId + ": Received have")
    if (peerBitfield.isDefinedAt(index)) peerBitfield(index) = true
  }

  private def handleBitfield(bitfield: Array[Boolean]): Unit = {
    // Fail if bitfield is not of the correct size, or if the bitfield has any of the spare bits set.
    if (bitfield.length != peerBitfield.length && bitfield.drop(torrent.pieceCount).contains(true)) {
      println(peer.peerId + ": Received invalid bitfield")
      self ! "close"
    } else {
      for (i <- 0 until torrent.pieceCount) peerBitfield(i) = bitfield(i)
    }
  }

  private def handleRequest(index: Int, begin: Int, length: Int): Unit = {
    println(peer.peerId + ": Received request")
  }

  private def handlePiece(piece: Piece, data:ByteString, connection: ActorRef): Unit = {
    println(peer.peerId + ": Received piece(block) " + piece.index + ", total of " + data.length + " bytes.")
    println(piece)
    if(piece.block.length != torrent.defaultBlockSize) {
      println("block size should be " + (ByteBuffer.wrap(data.take(4).toArray).getInt - 9))
      blockBuffer.clear()
      blockBuffer ++= piece.block
      context.become(buffering(piece, 16384, connection))
    } else {
      client ! BlockReceived(piece, peer)
    }
  }

  private def handleCancel(index: Int, begin: Int, length: Int): Unit = {
    println(peer.peerId + ": Received cancel")
  }

  private def handlePort(listenPort: Int): Unit = {
    println(peer.peerId + ": Received port")
  }
}