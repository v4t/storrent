package storrent.peers

import java.nio.{ByteBuffer, ByteOrder}

trait Message {}

object Message {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN

  def decode(payload: Array[Byte]): Option[Message] = {
    if (payload.length < 4) None
    else if (payload.length == 4) KeepAlive.decode(payload)
    else payload(4) match {
      case MessageId.CHOKE => Choke.decode(payload)
      case MessageId.UNCHOKE => Unchoke.decode(payload)
      case MessageId.INTERESTED => Interested.decode(payload)
      case MessageId.NOT_INTERESTED => NotInterested.decode(payload)
      case MessageId.HAVE => Have.decode(payload)
      case MessageId.BITFIELD => Bitfield.decode(payload)
      case MessageId.REQUEST => Request.decode(payload)
      case MessageId.PIECE => Piece.decode(payload)
      case MessageId.CANCEL => Cancel.decode(payload)
      case MessageId.PORT => Port.decode(payload)
      case _ => None
    }
  }

  def decodeLength(bytes: Seq[Byte]): Option[Int] =
    if(bytes.length >= 4) Some(ByteBuffer.wrap(bytes.take(4).toArray).getInt)
    else None
}

object MessageId {
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
