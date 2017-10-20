package nl.biopet.tools.gtftorefflat

import java.io.File

import nl.biopet.utils.tool.AbstractOptParser

class ArgsParser(cmdName: String) extends AbstractOptParser[Args](cmdName) {
  opt[File]('r', "refFlat") required () unbounded () valueName "<file>" action { (x, c) =>
    c.copy(refFlat = x)
  } text "Output refFlat file. Mandatory"
  opt[File]('g', "gtfFile") required () unbounded () valueName "<file>" action { (x, c) =>
    c.copy(gtfFile = x)
  } text "Input gtf file. Mandatory"
  opt[File]('R', "referenceFasta") unbounded () valueName "<file>" action { (x, c) =>
    c.copy(referenceFasta = Some(x))
  } text "Reference file"
}
