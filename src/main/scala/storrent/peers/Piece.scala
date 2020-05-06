package storrent.peers

import java.nio.{ByteBuffer, ByteOrder}

import akka.util.ByteStringBuilder

case class Piece(index: Int, begin: Int, block: Array[Byte]) extends Message

object Piece {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  val messageId: Byte = MessageId.PIECE
  val lengthPrefix: Int = 9

  def encode(index: Int, begin: Int, block: Array[Byte]): Array[Byte] = {
    val len = lengthPrefix + block.length
    val bsb = new ByteStringBuilder()
    bsb.putInt(len)
    bsb.putByte(messageId)
    bsb.putInt(index)
    bsb.putInt(begin)
    bsb.putBytes(block)
    bsb.result().toArray
  }

  def decode(bytes: Array[Byte]): Option[Piece] = {
    if (bytes.length <= 14 || bytes(4) != messageId) None
    else {
      val index = ByteBuffer.wrap(bytes.slice(5, 9)).getInt
      val begin = ByteBuffer.wrap(bytes.slice(9, 13)).getInt
      val block = bytes.drop(5)
      Some(Piece(index, begin, block))
    }
  }

}
