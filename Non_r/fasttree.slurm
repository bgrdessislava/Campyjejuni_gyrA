#!/bin/bash

#SBATCH --job-name=fasttree_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=0-2:00:00
#SBATCH --workdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/13672_analysis
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%j.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=spet4850@ox.ac.uk


module load FastTree/2.1.11-GCCcore-9.3.0

file=$1

echo making_fasttree_with_gtr+cat "${file}"

FastTree -gtr -nt < "${file}" > "${file/-aligned.fasta/-gtr_cat.nwk}"
