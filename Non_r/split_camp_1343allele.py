#!/usr/bin/env python
#open file

# if line starts with '>'
#     allele = line.split()[-1]
#     output_file = allele.xmfa
# write line to output_file append


import sys

file = open(sys.argv[1],'r')
for line in file:
    if line.startswith('>'):
        try:
            outF.close()
        except:
            pass
        allele = line.split()[-1]
        output_file = allele + '.xmfa'
        outF = open(output_file, 'a')
    outF.write(line)

file.close()



#cat *xmfa > all.xmfa
