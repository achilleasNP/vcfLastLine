# vcfLastLine

A little command line program that returns the last line of a VCF file that is bgzipped and tabix indexed and contains a single sequence e.g. chrom 1.
The program uses the tabix index to seek to the last compressed block so is quite faster than doing something like: 
`zcat vcfile | tail -n1`

This can be handy for large vcf files.

