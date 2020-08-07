#!/bin/bash

#SBATCH --job-name=concatinated1355_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=0-2:00:00
#SBATCH --workdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/align
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/concatinated1355_%j.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/concatinated1355_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=spet4850@spc.ox.ac.uk

module purge
module load slurm
module load perl

mkdir concatinated
# Concatinate my alighned fasta files into one
../../code/catfasta2phyml.pl -f *fasta > concatinated/concatinate.fasta
