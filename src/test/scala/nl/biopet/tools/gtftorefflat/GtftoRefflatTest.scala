package nl.biopet.tools.gtftorefflat

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

object GtftoRefflatTest extends BiopetTest {
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      ToolTemplate.main(Array())
    }
  }
}
