#!/bin/bash


#SBATCH --job-name=fasttree_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=1-0:00:00
#SBATCH --chdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/CC353/
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%j.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=spet4850@spc.ox.ac.uk

module load FastTree/2.1.11-GCCcore-9.3.0

FastTree -gtr -nt  < CC353_aligned_coreMLST_OdileAdvice_core.fas > CC353_alignedto_coreMLST_OdileAdvice.fas.nwk