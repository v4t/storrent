package storrent.peers

import java.nio.{ByteBuffer, ByteOrder}

import akka.util.ByteStringBuilder

case class Request(index: Int, begin: Int, length: Int) extends  Message

object Request {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  val messageId: Byte = MessageId.REQUEST
  val lengthPrefix: Int = 13

  def encode(index: Int, begin: Int, length: Int): Array[Byte] = {
    val bsb = new ByteStringBuilder()
    bsb.putInt(lengthPrefix)
    bsb.putByte(messageId)
    bsb.putInt(index)
    bsb.putInt(begin)
    bsb.putInt(length)
    bsb.result().toArray
  }

  def decode(bytes: Array[Byte]): Option[Request] = {
    if (bytes.length != 17 || bytes(4) != messageId || bytes.slice(0, 4).last != lengthPrefix) None
    else {
      val index = ByteBuffer.wrap(bytes.slice(5,9)).getInt
      val begin = ByteBuffer.wrap(bytes.slice(9,13)).getInt
      val length = ByteBuffer.wrap(bytes.slice(13,17)).getInt
      Some(Request(index, begin, length))
    }
  }
}