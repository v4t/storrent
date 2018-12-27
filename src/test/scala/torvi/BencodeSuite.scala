package torvi

import org.scalatest.FunSuite

class BencodeSuite extends FunSuite {

  test("Bencode decodes string literals") {
    val value = "4:test"
    assert(Bencode.decode(value.toStream) == List(BencodeStringValue("test")))
  }

  test("Bencode decodes empty and nonempty string literals") {
    val value = "0:4:test"
    assert(Bencode.decode(value.toStream) == List(BencodeStringValue(""), BencodeStringValue("test")))
  }

  test("Bencode decodes integer literals") {
    val value = "i2414e"
    assert(Bencode.decode(value.toStream) == List(BencodeIntValue(2414)))
  }

  test("Bencode decodes negative integer literals") {
    val value = "i-500e"
    assert(Bencode.decode(value.toStream) == List(BencodeIntValue(-500)))
  }
}
