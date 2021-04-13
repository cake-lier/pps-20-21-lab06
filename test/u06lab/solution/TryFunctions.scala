package u06lab.solution

import org.junit.jupiter.api.{Assertions, Test}
import u06lab.solution.FunctionsImpl.{sum, concat, max}

class TryFunctions {
  @Test
  def testSum() {
    Assertions.assertEquals(60.1, sum(List(10.0, 20.0, 30.1)), 0.001)
    Assertions.assertEquals(0.0, sum(List()))
  }

  @Test
  def testConcat() {
    Assertions.assertEquals("abc", concat(Seq("a", "b", "c")))
    Assertions.assertEquals("", concat(Seq()))
  }

  @Test
  def testMax() {
    Assertions.assertEquals(3, max(List(-10, 3, -5, 0)))
    Assertions.assertEquals(Integer.MIN_VALUE, max(List()))
  }
}