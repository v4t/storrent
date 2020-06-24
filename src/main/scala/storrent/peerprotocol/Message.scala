package storrent.peerprotocol

import java.nio.{ByteBuffer, ByteOrder}

trait Message {}

object Message {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN

  /**
   * Decode given bytes into a valid peer protocol message.
   * @param payload Message bytes
   * @return
   */
  def decode(payload: Array[Byte]): Option[Message] = {
    if (payload.length < 4) None
    else if (payload.length == 4) KeepAlive.decode(payload)
    else payload(4) match {
      case Message.Id.CHOKE => Choke.decode(payload)
      case Message.Id.UNCHOKE => Unchoke.decode(payload)
      case Message.Id.INTERESTED => Interested.decode(payload)
      case Message.Id.NOT_INTERESTED => NotInterested.decode(payload)
      case Message.Id.HAVE => Have.decode(payload)
      case Message.Id.BITFIELD => Bitfield.decode(payload)
      case Message.Id.REQUEST => Request.decode(payload)
      case Message.Id.PIECE => Piece.decode(payload)
      case Message.Id.CANCEL => Cancel.decode(payload)
      case Message.Id.PORT => Port.decode(payload)
      case _ => None
    }
  }

  /**
   * Decode peer protocol message length from first 4 bytes of byte block.
   * @param bytes Message bytes
   * @return
   */
  def decodeLength(bytes: Seq[Byte]): Option[Int] =
    if(bytes.length >= 4) Some(ByteBuffer.wrap(bytes.take(4).toArray).getInt)
    else None

  /** Valid peer protocol message ids */
  object Id {
    val HANDSHAKE: Byte = -2
    val KEEP_ALIVE: Byte = -1
    val CHOKE: Byte = 0
    val UNCHOKE: Byte = 1
    val INTERESTED: Byte = 2
    val NOT_INTERESTED: Byte = 3
    val HAVE: Byte = 4
    val BITFIELD: Byte = 5
    val REQUEST: Byte = 6
    val PIECE: Byte = 7
    val CANCEL: Byte = 8
    val PORT: Byte = 9
  }
}

