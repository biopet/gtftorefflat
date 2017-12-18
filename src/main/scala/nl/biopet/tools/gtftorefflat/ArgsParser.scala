package nl.biopet.tools.gtftorefflat

import java.io.File

import nl.biopet.utils.tool.{AbstractOptParser, ToolCommand}

class ArgsParser(toolCommand: ToolCommand[Args])
    extends AbstractOptParser[Args](toolCommand) {
  opt[File]('r', "refFlat") required () unbounded () valueName "<file>" action {
    (x, c) =>
      c.copy(refFlat = x)
  } text "Output refFlat file. Mandatory"
  opt[File]('g', "gtfFile") required () unbounded () valueName "<file>" action {
    (x, c) =>
      c.copy(gtfFile = x)
  } text "Input gtf file. Mandatory"
  opt[File]('R', "referenceFasta") unbounded () valueName "<file>" action {
    (x, c) =>
      c.copy(referenceFasta = Some(x))
  } text "Reference file"
}
