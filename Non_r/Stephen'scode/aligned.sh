#!/usr/bin/env bash

# Delete if file exists
rm 4food_chain_nucleotide_merged.fasta

# Append source to end of FASTA header, replace space with underscore the | with ;
# Delete blank sequences (represented by -) and blank lines
# Delete empty FASTA records
# Merge to single file
for source in caecal carcass food humanfeces; do
  sed -e "s/>.*/&;${source}/" -e 's/ /_/g' \
      -e 's/|/;/g' -e '/^\-$/d' \
      -e '/^[[:space:]]*$/d' \
      "${source}"_nucleotide.fasta.txt \
    | awk 'BEGIN {RS = ">" ; FS = "\n" ; ORS = ""} $2 {print ">"$0}' \
    >> 4food_chain_nucleotide_merged.fasta
done

# Translate sequences
transeq -sequence 4food_chain_nucleotide_merged.fasta \
        -outseq 4food_chain_aminoacid_merged.fasta

# Align sequences
clustalo -i 4food_chain_aminoacid_merged.fasta \
         -o 4food_chain_aminoacid_aligned.fasta \
         --threads 12 -v

# Linearise fasta sequences (all sequence on line after header)
for fasta in 4food_chain*; do
  awk '!/^>/ { printf "%s", $0; n = "\n" } /^>/ { print n $0; n = "" } END { printf "%s", n }' "${fasta}" \
    > tmp.fasta
  mv tmp.fasta "${fasta}"
done

# Merge FASTA header from before clustalo with the headers after alignment
# This is neccessary because clustalo trims the long headers
awk 'NR%2==0' 4food_chain_aminoacid_aligned.fasta \
  | paste -d '\n' <(awk 'NR%2==1' 4food_chain_aminoacid_merged.fasta) - \
  > tmp.fasta
mv tmp.fasta 4food_chain_aminoacid_merged.fasta
