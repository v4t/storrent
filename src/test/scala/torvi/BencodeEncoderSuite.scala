package torvi

import org.scalatest.FunSuite

class BencodeEncoderSuite extends FunSuite {

  test("BencodeStringValue can be encoded into bencode string") {
    val value = List(BencodeStringValue("test"))
    assert(BencodeEncoder.encode(value) == "4:test")
  }

  test("Both empty and nonempty BencodeStringValues can be encoded into bencode strings") {
    val value = List(BencodeStringValue(""), BencodeStringValue("test"))
    assert(BencodeEncoder.encode(value) == "0:4:test")
  }

  test("BencodeIntValue can be encoded into bencode string") {
    val value = List(BencodeIntValue(2414))
    assert(BencodeEncoder.encode(value) == "i2414e")
  }

  test("Negative BencodeIntValues can be encoded into bencode string") {
    val value = List(BencodeIntValue(-500))
    assert(BencodeEncoder.encode(value) == "i-500e")
  }
}

