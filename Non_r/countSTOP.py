#!/usr/bin/env python3

import sys
import fileinput
from Bio.Seq import Seq

with fileinput.input() as f:
    for line in f:
        line = line.strip()
        if line.startswith('>'):
            try:
                stopCodons = Seq(sequence).translate(table=11).count('*')
                print(fileinput.filename(), header, stopCodons)
            except NameError:
                pass
            header = line.strip('>')
            sequence = ''
        else:
            sequence = line + sequence
