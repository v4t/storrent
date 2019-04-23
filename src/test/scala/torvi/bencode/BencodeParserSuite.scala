package torvi.bencode

import org.scalatest.FunSuite

class BencodeParserSuite extends FunSuite {

  test("Bencode decodes string literals") {
    val value = "4:test"
    assert(BencodeParser.parse(value) == List(BencodeString("test")))
  }

  test("Bencode decodes empty and nonempty string literals") {
    val value = "0:4:test"
    assert(BencodeParser.parse(value) == List(BencodeString(""), BencodeString("test")))
  }

  test("Bencode decodes integer literals") {
    val value = "i2414e"
    assert(BencodeParser.parse(value) == List(BencodeInt(2414)))
  }

  test("Bencode decodes negative integer literals") {
    val value = "i-500e"
    assert(BencodeParser.parse(value) == List(BencodeInt(-500)))
  }

  test("Bencode decodes string lists") {
    val value = "l3:foo3:bare"
    assert(
      BencodeParser.parse(value) == List(BencodeList(List(BencodeString("foo"), BencodeString("bar"))))
    )
  }

  test("Bencode decodes mixed string and integer lists") {
    val value = "l3:fooi2ee"
    assert(
      BencodeParser.parse(value) == List(BencodeList(List(BencodeString("foo"), BencodeInt(2))))
    )
  }

  test("Bencode decodes list of lists") {
    val value = "ll3:fooel3:baree"
    assert(BencodeParser.parse(value) == List(BencodeList(List(
      BencodeList(List(BencodeString("foo"))),
      BencodeList(List(BencodeString("bar")))
    ))))
  }

  test("Bencode decodes empty list") {
    val value = "le"
    assert(BencodeParser.parse(value) == List(BencodeList(List())))
  }

  test("Bencode decodes dictionaries with one element") {
    val value = "d3:foo3:bare"
    assert(
      BencodeParser.parse(value) == List(BencodeDict(Map(BencodeString("foo") -> BencodeString("bar"))))
    )
  }

  test("Bencode decodes dictionaries with mixed elements") {
    val value = "d3:foo3:bar3:inti45ee"
    assert(
      BencodeParser.parse(value) == List(BencodeDict(Map(
        BencodeString("foo") -> BencodeString("bar"),
        BencodeString("int") -> BencodeInt(45)
      )))
    )
  }

  test("Bencode decodes dictionaries inside dictionaries") {
    val value = "d3:food3:bari99eee"
    assert(
      BencodeParser.parse(value) == List(BencodeDict(Map(
        BencodeString("foo") -> BencodeDict(Map(
          BencodeString("bar") -> BencodeInt(99)))
      )))
    )
  }

  test("Bencode decodes empty dictionaries") {
    val value = "de"
    assert(
      BencodeParser.parse(value) == List(BencodeDict(Map()))
    )
  }
}

