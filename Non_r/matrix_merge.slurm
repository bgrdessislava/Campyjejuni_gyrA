#!/bin/bash

#SBATCH --job-name=merge_matrix_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=0-12:00:00
#SBATCH --chdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/13672_analysis
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/merge_matrix_%j.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/merge_matrix_%j.err


module load Anaconda3

source activate py3

python merge_matrix.py $@