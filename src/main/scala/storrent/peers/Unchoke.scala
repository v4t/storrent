package storrent.peers

import java.nio.{ByteBuffer, ByteOrder}

import akka.util.ByteStringBuilder

case class Unchoke() extends Message

object Unchoke {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  val messageId: Byte = MessageId.UNCHOKE
  val lengthPrefix: Int = 1

  def encode(): Array[Byte] = {
    val bsb = new ByteStringBuilder()
    bsb.putInt(lengthPrefix)
    bsb.putByte(messageId)
    bsb.result().toArray
  }

  def decode(bytes: Array[Byte]): Option[Unchoke] = {
    if (bytes.length != 5 || bytes(4) != messageId || ByteBuffer.wrap(bytes.slice(0, 4)).getInt != lengthPrefix) None
    else Some(Unchoke())
  }
}