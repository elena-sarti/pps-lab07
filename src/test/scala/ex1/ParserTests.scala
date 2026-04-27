package ex1

import ex1.Parsers.charParser
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*

class ParserTests:
  def parser = new BasicParser(Set('a', 'b', 'c'))
  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  // note we do not need a class name here, we use the structural type
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
  def sparser: Parser[Char] = "abc".charParser()

  def testBasicParser: Assertion =
    parser.parseAll("aabc".toList) shouldBe true
    parser.parseAll("aabcdc".toList) shouldBe false
    parser.parseAll("".toList) shouldBe true

  def testNotEmptyParser: Assertion =
    parserNE.parseAll("0101".toList) shouldBe true
    parserNE.parseAll("0123".toList) shouldBe false
    parserNE.parseAll(List()) shouldBe false

  def testNotTwoConsecutiveParser: Assertion =
    parserNTC.parseAll("XYZ".toList) shouldBe true
    parserNTC.parseAll("XYYZ".toList) shouldBe false
    parserNTC.parseAll("".toList) shouldBe true

  def testNotEmptyAndNotTwoConsecutiveParser: Assertion =
    parserNTCNE.parseAll("XYZ".toList) shouldBe true
    parserNTCNE.parseAll("XYYZ".toList) shouldBe false
    parserNTCNE.parseAll("".toList) shouldBe false

  def testStringParser: Assertion =
    sparser.parseAll("aabc".toList) shouldBe true
    sparser.parseAll("aabcdc".toList) shouldBe false
    sparser.parseAll("".toList) shouldBe true
