package nl.biopet.tools.gtftorefflat

import nl.biopet.utils.test.tools.ToolTest
import org.testng.annotations.Test

class GtftoRefflatTest extends ToolTest[Args] {
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      GtftoRefflat.main(Array())
    }
  }
}
