package nl.biopet.tools.gtftorefflat

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class GtftoRefflatTest extends BiopetTest {
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      GtftoRefflat.main(Array())
    }
  }
}
