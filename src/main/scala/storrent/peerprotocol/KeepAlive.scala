package storrent.peerprotocol

import java.nio.{ByteBuffer, ByteOrder}

import akka.util.ByteStringBuilder

/**
 * keep-alive: <len=0000>
 */
case class KeepAlive() extends Message

object KeepAlive {
  implicit val byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  val lengthPrefix: Int = 0

  def encode(): Array[Byte] = {
    val builder = new ByteStringBuilder()
    builder.putInt(lengthPrefix)
    builder.result().toArray
  }

  def decode(bytes: Array[Byte]): Option[KeepAlive] = {
    if (bytes.length != 4 || ByteBuffer.wrap(bytes).getInt != lengthPrefix) None
    else Some(KeepAlive())
  }
}