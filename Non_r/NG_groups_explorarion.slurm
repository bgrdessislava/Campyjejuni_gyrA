#!/bin/bash

#SBATCH --job-name=NG_ExploreNumber_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=0-02:00:00
#SBATCH --chdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/NG_CheckClusterNumber/
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/data/NG_CheckClusterNumber/logs/NG_ExploreNumber_%j.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/data/NG_CheckClusterNumber/logs/NG_ExploreNumber_%j.err

module load Anaconda3


/data/zool-microbe-genomics/spet4850/envs/py3/bin/python NG_ExploreNumber.py $1