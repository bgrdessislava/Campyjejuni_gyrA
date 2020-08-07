#!/bin/bash

#SBATCH --job-name=concatinated1355_newik%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=6
#SBATCH --time=0-12:00:00
#SBATCH --workdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/align/concatinated
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/concatinated1355_newik%j.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/concatinated1355_newik%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=spet4850@spc.ox.ac.uk

module purge
module load slurm
module load fasttree/2.1.9


FastTreeMP -gtr -nt < concatinate.fasta > concatinated1355-gtr_cat.nwk
