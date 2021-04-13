package u06lab.solution

import org.junit.jupiter.api.{Assertions, Test}

class TryParsers {
  @Test
  def testBasicParser(): Unit = {
    val parser = new BasicParser(Set('a', 'b', 'c'))
    Assertions.assertTrue(parser.parseAll("aabc".toList))
    Assertions.assertFalse(parser.parseAll("aabcdc".toList))
    Assertions.assertTrue(parser.parseAll("".toList))
  }

  @Test
  def testNotEmptyParser(): Unit = {
    val parserNE = new NonEmptyParser(Set('0', '1'))
    Assertions.assertTrue(parserNE.parseAll("0101".toList))
    Assertions.assertFalse(parserNE.parseAll("0123".toList))
    Assertions.assertFalse(parserNE.parseAll(List()))
  }

  @Test
  def testNotTwoConsecutiveParser(): Unit = {
    val parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
    Assertions.assertTrue(parserNTC.parseAll("XYZ".toList))
    Assertions.assertFalse(parserNTC.parseAll("XYYZ".toList))
    Assertions.assertTrue(parserNTC.parseAll("".toList))
  }

  @Test
  def testNotEmptyAndNotTwoConsecutiveParser(): Unit = {
    val parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
    Assertions.assertTrue(parserNTCNE.parseAll("XYZ".toList))
    Assertions.assertFalse(parserNTCNE.parseAll("XYYZ".toList))
    Assertions.assertFalse(parserNTCNE.parseAll("".toList))
  }

  @Test
  def testStringParser(): Unit = {
    import u06lab.solution.String._
    val sparser = "abc".charParser()
    Assertions.assertTrue(sparser.parseAll("aabc".toList))
    Assertions.assertFalse(sparser.parseAll("aabcdc".toList))
    Assertions.assertTrue(sparser.parseAll("".toList))
  }
}


