package ex1

import ex1.Parsers.charParser
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserTests extends AnyFlatSpec with Matchers:

  "A basic parser defined on chars 'a', 'b', 'c' " should "accept all kind of strings" in:
    val parser = new BasicParser(Set('a', 'b', 'c'))
    parser.parseAll("aabc".toList) shouldBe true
    parser.parseAll("aabcdc".toList) shouldBe false
    parser.parseAll("".toList) shouldBe true

  "A non empty parser " should "not accept empty strings" in:
    val parserNE = new NonEmptyParser(Set('0', '1'))
    parserNE.parseAll("0101".toList) shouldBe true
    parserNE.parseAll("0123".toList) shouldBe false
    parserNE.parseAll(List()) shouldBe false

  "A non two consecutive parser " should "not accept strings with two equal consecutive elements " in:
    val parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
    parserNTC.parseAll("XYZ".toList) shouldBe true
    parserNTC.parseAll("XYYZ".toList) shouldBe false
    parserNTC.parseAll("".toList) shouldBe true

  "A NTNE parser" should "behave as a combination of the two:" in:
    val parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
    parserNTCNE.parseAll("XYZ".toList) shouldBe true
    parserNTCNE.parseAll("XYYZ".toList) shouldBe false
    parserNTCNE.parseAll("".toList) shouldBe false

  "A string parser" should "accept a string as an extension method" in:
    val sparser: Parser[Char] = "abc".charParser()
    sparser.parseAll("aabc".toList) shouldBe true
    sparser.parseAll("aabcdc".toList) shouldBe false
    sparser.parseAll("".toList) shouldBe true
