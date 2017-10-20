# Manual

GtfToRefflat converts a GTF file to a refflat file. 
It can optionally use a reference FASTA file to check if all contigs are present on this reference FASTA file.

example:

```BASH
java -jar GtfToRefflat-version.jar \
-g input.gtf
-r output.refflat
-R reference.fa
```
