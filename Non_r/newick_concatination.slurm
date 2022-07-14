#!/bin/bash

#SBATCH --job-name=pairwise_matrix_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=0-2:00:00
#SBATCH --workdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/13672_analysis
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/pairwise_matrix_%j.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/pairwise_matrix_%j.err


module load Anaconda3

source activate py3

python pairwise.py $1