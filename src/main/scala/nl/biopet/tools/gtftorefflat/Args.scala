package nl.biopet.tools.gtftorefflat

import java.io.File

case class Args(refFlat: File = null,
                gtfFile: File = null,
                referenceFasta: Option[File] = None)
