package nl.biopet.tools.gtftorefflat

import nl.biopet.utils.test.tools.ToolTest
import org.testng.annotations.Test

class GtftoRefflatTest extends ToolTest[Args] {
  def toolCommand: GtftoRefflat.type = GtftoRefflat
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      GtftoRefflat.main(Array())
    }
  }
}
