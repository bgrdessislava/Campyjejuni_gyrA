#!/bin/bash

#SBATCH --job-name=NG20_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=0-00:20:00
#SBATCH --chdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/NG_Catboost_20/
#SBATCH --output=data/zool-microbe-genomics/spet4850/campy_analysis/data/NG_Catboost_20/logs/NG20_%j.out
#SBATCH --error=data/zool-microbe-genomics/spet4850/campy_analysis/data/NG_Catboost_20/logs/NG20_%j.err

module load Anaconda3

source activate py3

/data/zool-microbe-genomics/spet4850/envs/py3/bin/python run_coreMLST_catboost.py $@