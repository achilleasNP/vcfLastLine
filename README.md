# vcfLastLine

A little command line program that returns the last line of a VCF file that is bgzipped and tabix indexed.

The program uses the tabix index to seek to the last compressed block so is quite faster than doing something like: 
```
zcat vcfile | tail -n1
```

This can be handy for large vcf files, when you want to know the last position in the file. 

## Installation
The easiest way to install is using stack:
First clone the repository:

```
git clone https://github.com/achilleasNP/vcfLastLine
```

Enter the created directory:

```
cd vcfLastLine
```


The use stack to install
```
stack install
```


## Usage
For the last line in the file do:
```
vcfLastLine vcf_file.vcf.gz
```

To get the last line for each sequence in the file do:
```
vcfLastLine -a vcf_file.vcf.gz
```
