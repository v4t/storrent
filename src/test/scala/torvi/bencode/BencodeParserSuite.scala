package torvi.bencode

import org.scalatest.FunSuite

class BencodeParserSuite extends FunSuite {

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

  test("Bencode decodes empty list") {
    val value = "le"
    assert(BencodeParser.parse(value) == List(BencodeListValue(List())))
  }

  test("Bencode decodes dictionaries with one element") {
    val value = "d3:foo3:bare"
    assert(
      BencodeParser.parse(value) == List(BencodeDictValue(Map(BencodeStringValue("foo") -> BencodeStringValue("bar"))))
    )
  }

  test("Bencode decodes dictionaries with mixed elements") {
    val value = "d3:foo3:bar3:inti45ee"
    assert(
      BencodeParser.parse(value) == List(BencodeDictValue(Map(
        BencodeStringValue("foo") -> BencodeStringValue("bar"),
        BencodeStringValue("int") -> BencodeIntValue(45)
      )))
    )
  }

  test("Bencode decodes dictionaries inside dictionaries") {
    val value = "d3:food3:bari99eee"
    assert(
      BencodeParser.parse(value) == List(BencodeDictValue(Map(
        BencodeStringValue("foo") -> BencodeDictValue(Map(
          BencodeStringValue("bar") -> BencodeIntValue(99)))
      )))
    )
  }

  test("Bencode decodes empty dictionaries") {
    val value = "de"
    assert(
      BencodeParser.parse(value) == List(BencodeDictValue(Map()))
    )
  }
}

