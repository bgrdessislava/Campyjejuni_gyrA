#!/bin/bash

#SBATCH --job-name=m1343loci_each_NG_prediction_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=0-00:20:00
#SBATCH --chdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/NG_Catboost/
#SBATCH --output=data/zool-microbe-genomics/spet4850/campy_analysis/data/NG_Catboost/logs/catboost_%j.out
#SBATCH --error=data/zool-microbe-genomics/spet4850/campy_analysis/data/NG_Catboost/logs/catboost_%j.err

module load Anaconda3

source activate py3

python run_coreMLST_catboost.py $@