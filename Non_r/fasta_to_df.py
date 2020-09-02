#!/usr/bin/env python3

import sys
import fileinput
from Bio.Seq import Seq

with open(sys.argv[1]) as f:
    ids = {}
    for line in f:
        id, pheno = line.strip().split(',')
        ids[id] = pheno

aminoCat = 0
usedAmino = {}
nucleoCat = 0
usedNucleo = {}

print ('ID', 'Gene Name', 'Sequence', 'Nucleotide Categorical', 'AminoAcid', 'AminoAcid Categorical' sep = ',')
with fileinput.input(sys.argv[2:]) as f:
    for line in f:
        line = line.strip()
        if line.startswith('>'):
            try:
                if header in ids:
                    phenotype = ids[header]
                    protein = str(Seq(sequence).translate(table=11))
                    if protein not in usedAmino:
                        usedAmino[protein] = aminoCat
                        aminoCat += 1
                    categoricalAA = usedAmino[protein]
                    if sequence not in usedNucleo:
                        usedNucleo[sequence] = nucleoCat
                        nucleoCat += 1
                    categoricalNuc = usedNucleo[sequence]
                    print(fileinput.filename(), header, phenotype, sequence, categoricalNuc, protein, categoricalAA, sep = ',')
            except NameError:
                pass
            header = line.strip('>')
            sequence = ''
        else:
            sequence = line + sequence
    if header in ids:
        phenotype = ids[header]
        protein = str(Seq(sequence).translate(table=11))
        if protein not in usedAmino:
            usedAmino[protein] = aminoCat
            aminoCat += 1
        categoricalAA = usedAmino[protein]
        if sequence not in usedNucleo:
            usedNucleo[sequence] = nucleoCat
            nucleoCat += 1
        categoricalNuc = usedNucleo[sequence]
        print(fileinput.filename(), header, phenotype, sequence, categoricalNuc, protein, categoricalAA, sep = ',')
