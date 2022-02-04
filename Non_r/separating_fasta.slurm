#!/bin/bash

#SBATCH --job-name=concatinated1355_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=00-01:00:00
#SBATCH --workdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/align
#SBATCH --mail-type=ALL
#SBATCH --mail-user=spet4850@spc.ox.ac.uk

module purge
module load slurm
module load python/3.3

/data/zool-microbe-genomics/spet4850/campy_analysis/code/separating_fasta.py *-aligned.fasta | sed 's/-aligned.fasta//' | gzip > separated_aligned_fasta.csv.gz
