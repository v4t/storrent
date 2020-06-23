package storrent.peerprotocol

import java.nio.charset.StandardCharsets

case class Handshake(infoHash: Array[Byte], peerId: String) extends Message

object Handshake {

  def encode(infoHash: Array[Byte], peerId: String): Array[Byte] =
    Array.concat(
      Array[Byte](19),
      "BitTorrent protocol".getBytes(StandardCharsets.ISO_8859_1),
      Array.fill[Byte](8)(0),
      infoHash,
      peerId.getBytes(StandardCharsets.ISO_8859_1)
    )

  def decode(bytes: Array[Byte]): Option[Handshake] = {
    if (bytes.length != 68) return None
    val protocolStrLen = bytes(0)
    val protocolStr = new String(bytes.slice(1, 1 + protocolStrLen), StandardCharsets.ISO_8859_1)
    if (protocolStr != "BitTorrent protocol") return None
    Some(Handshake(
      infoHash = bytes.slice(bytes.length - 40, bytes.length - 20),
      peerId = new String(bytes.slice(bytes.length - 20, bytes.length), StandardCharsets.ISO_8859_1)
    ))
  }
}
