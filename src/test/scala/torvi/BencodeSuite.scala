package torvi

import org.scalatest.FunSuite

class BencodeSuite extends FunSuite {

  test("Bencode decodes string literals") {
    val value = "4:test"
    assert(BencodeParser.parse(value) == List(BencodeStringValue("test")))
  }

  test("Bencode decodes empty and nonempty string literals") {
    val value = "0:4:test"
    assert(BencodeParser.parse(value) == List(BencodeStringValue(""), BencodeStringValue("test")))
  }

  test("Bencode decodes integer literals") {
    val value = "i2414e"
    assert(BencodeParser.parse(value) == List(BencodeIntValue(2414)))
  }

  test("Bencode decodes negative integer literals") {
    val value = "i-500e"
    assert(BencodeParser.parse(value) == List(BencodeIntValue(-500)))
  }

  test("Bencode decodes string lists") {
    val value = "l3:foo3:bare"
    assert(
      BencodeParser.parse(value) == List(BencodeListValue(List(BencodeStringValue("foo"), BencodeStringValue("bar"))))
    )
  }

  test("Bencode decodes mixed string and integer lists") {
    val value = "l3:fooi2ee"
    assert(
      BencodeParser.parse(value) == List(BencodeListValue(List(BencodeStringValue("foo"), BencodeIntValue(2))))
    )
  }

  test("Bencode decodes list of lists") {
    val value = "ll3:fooel3:baree"
    assert(BencodeParser.parse(value) == List(BencodeListValue(List(
      BencodeListValue(List(BencodeStringValue("foo"))),
      BencodeListValue(List(BencodeStringValue("bar")))
    ))))
  }
}

