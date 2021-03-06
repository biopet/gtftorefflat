/*
 * Copyright (c) 2017 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package nl.biopet.tools.gtftorefflat

import java.io.{File, PrintWriter}

import nl.biopet.utils.ngs.fasta
import nl.biopet.utils.ngs.annotation.{Exon, Feature, Gene, Transcript}
import nl.biopet.utils.tool.ToolCommand

import scala.collection.mutable
import scala.io.Source

object GtftoRefflat extends ToolCommand[Args] {
  def emptyArgs: Args = Args()
  def argsParser = new ArgsParser(this)
  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    logger.info("Start")
    gtfToRefflat(cmdArgs.gtfFile, cmdArgs.refFlat, cmdArgs.referenceFasta)
    logger.info("Done")
  }

  def gtfToRefflat(gtfFile: File,
                   refflatFile: File,
                   referenceFasta: Option[File] = None): Unit = {
    val reader = Source.fromFile(gtfFile)

    val featureBuffer: mutable.Map[String, Int] = mutable.Map()

    val genesBuffer: mutable.Map[String, Gene] = mutable.Map()
    val transcriptsBuffer: mutable.Map[String, List[Transcript]] =
      mutable.Map()
    val exonBuffer: mutable.Map[(String, String), List[Exon]] = mutable.Map()
    val codingBuffer
      : mutable.Map[(String, String), (Option[Int], Option[Int])] =
      mutable.Map()

    val referenceDict = referenceFasta.map(file => fasta.getCachedDict(file))

    def updateCodingRegion(geneId: String,
                           transcriptId: String,
                           start: Option[Int] = None,
                           end: Option[Int] = None): Unit = {
      genesBuffer
        .get(geneId)
        .flatMap(_.transcripts.find(_.name == transcriptId)) match {
        case Some(transcriptOld) =>
          val geneOld = genesBuffer(geneId)
          genesBuffer(geneId) = geneOld.copy(
            transcripts = updateTranscriptCodingRegion(
              transcriptOld,
              start,
              end) :: geneOld.transcripts.filter(_.name != transcriptOld.name))
        case _ =>
          transcriptsBuffer
            .get(geneId)
            .flatMap(_.find(_.name == transcriptId)) match {
            case Some(transcriptOld) =>
              transcriptsBuffer(geneId) = updateTranscriptCodingRegion(
                transcriptOld,
                start,
                end) ::
                transcriptsBuffer(geneId).filter(_.name != transcriptId)
            case _ =>
              val (oldStart, oldEnd) =
                codingBuffer.getOrElse((geneId, transcriptId), (None, None))
              codingBuffer((geneId, transcriptId)) =
                (start.map(Some(_)).getOrElse(oldStart),
                 end.map(Some(_)).getOrElse(oldEnd))
          }
      }
    }

    def updateTranscriptCodingRegion(transcript: Transcript,
                                     start: Option[Int] = None,
                                     end: Option[Int] = None): Transcript = {
      val newStart: Option[Int] = start match {
        case Some(x) =>
          transcript.codingStart match {
            case Some(y) if y < x => Some(y)
            case _                => Some(x)
          }
        case _ => transcript.codingStart
      }

      val newEnd: Option[Int] = end match {
        case Some(x) =>
          transcript.codingEnd match {
            case Some(y) if y > x => Some(y)
            case _                => Some(x)
          }
        case _ => transcript.codingEnd
      }

      transcript.copy(codingStart = newStart, codingEnd = newEnd)
    }

    logger.info("Reading gtf file")
    reader
      .getLines()
      .filter(!_.startsWith("#"))
      .foreach { line =>
        val feature = Feature.fromLine(line)
        referenceDict.foreach(
          dict =>
            require(dict.getSequence(feature.contig) != null,
                    s"Contig '${feature.contig}' does not exist on reference"))
        featureBuffer += feature.feature -> (featureBuffer.getOrElse(
          feature.feature,
          0) + 1)
        lazy val geneId = feature.attributes("gene_id")
        lazy val transcriptId = feature.attributes("transcript_id")
        feature.feature match {
          case "gene" =>
            genesBuffer += geneId -> Gene(
              geneId,
              feature.contig,
              feature.minPosition,
              feature.maxPosition,
              feature.strand.get,
              transcriptsBuffer.remove(geneId).getOrElse(Nil))
          case "transcript" =>
            val (start, end) = codingBuffer
              .remove((geneId, transcriptId))
              .getOrElse((None, None))
            val transcript =
              Transcript(transcriptId,
                         feature.minPosition,
                         feature.maxPosition,
                         start,
                         end,
                         exonBuffer.remove(geneId, transcriptId).getOrElse(Nil))
            if (genesBuffer.contains(geneId)) {
              val oldGene = genesBuffer(geneId)
              genesBuffer(geneId) =
                oldGene.copy(transcripts = transcript :: oldGene.transcripts)
            } else
              transcriptsBuffer(geneId) = transcript :: transcriptsBuffer
                .getOrElse(geneId, Nil)
          case "exon" =>
            val exon = Exon(feature.minPosition, feature.maxPosition)
            genesBuffer
              .get(geneId)
              .flatMap(_.transcripts.find(_.name == transcriptId)) match {
              case Some(transcriptOld) =>
                val geneOld = genesBuffer(geneId)
                genesBuffer(geneId) = geneOld.copy(
                  transcripts = transcriptOld
                    .copy(exons = exon :: transcriptOld.exons) :: geneOld.transcripts
                    .filter(_.name != transcriptOld.name))
              case _ =>
                transcriptsBuffer
                  .get(geneId)
                  .flatMap(_.find(_.name == transcriptId)) match {
                  case Some(transcriptOld) =>
                    transcriptsBuffer(geneId) = transcriptOld.copy(
                      exons = exon :: transcriptOld.exons) ::
                      transcriptsBuffer(geneId).filter(_.name != transcriptId)
                  case _ =>
                    exonBuffer((geneId, transcriptId)) = exon :: exonBuffer
                      .getOrElse((geneId, transcriptId), Nil)
                }
            }
          case "stop_codon" if !feature.strand.contains(false) =>
            updateCodingRegion(geneId, transcriptId, end = Some(feature.end))
          case "stop_codon" =>
            updateCodingRegion(geneId,
                               transcriptId,
                               start = Some(feature.start - 1))
          case "start_codon" if !feature.strand.contains(false) =>
            updateCodingRegion(geneId,
                               transcriptId,
                               start = Some(feature.start - 1))
          case "start_codon" =>
            updateCodingRegion(geneId, transcriptId, end = Some(feature.end))
          case "CDS" =>
            updateCodingRegion(geneId,
                               transcriptId,
                               start = Some(feature.start - 1),
                               end = Some(feature.end))
          case _ =>
        }
      }

    featureBuffer.foreach { case (k, c) => logger.info(s"$k\t$c") }

    val writer = new PrintWriter(refflatFile)

    for {
      (_, genes) <- genesBuffer.values.toList
        .groupBy(_.contig)
        .toList
        .sortBy { case (name, _) => name }
      gene <- genes.sortBy(_.start)
      transcript <- gene.transcripts
    } {
      val exons = transcript.exons.sortBy(_.start)
      val values = List(
        gene.name,
        transcript.name,
        gene.contig,
        if (gene.strand) "+" else "-",
        (transcript.transcriptionStart - 1).toString,
        transcript.transcriptionEnd.toString,
        transcript.codingStart
          .getOrElse(transcript.transcriptionEnd)
          .toString,
        transcript.codingEnd
          .getOrElse(transcript.transcriptionEnd)
          .toString,
        transcript.exons.length.toString,
        exons.map(_.start - 1).mkString("", ",", ","),
        exons.map(_.end).mkString("", ",", ",")
      )
      writer.println(values.mkString("\t"))
    }

    writer.close()
  }

  def descriptionText: String =
    s"""
      |$toolName converts a GTF file to a refflat file.
    """.stripMargin

  def manualText: String =
    s"""
      |$toolName converts a GTF file to a refflat file.
      |It can optionally use a reference FASTA file to check
      |if all contigs are present on this reference FASTA file.
    """.stripMargin

  def exampleText: String =
    s"""
       |To convert a gtf to refflat and check if all contigs are present
       |in the reference:
       |${example("-g",
                  "input.gtf",
                  "-r",
                  "output.refflat",
                  "-R",
                  "reference.fa")}
     """.stripMargin
}
