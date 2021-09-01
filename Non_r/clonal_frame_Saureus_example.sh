#!/bin/bash


#SBATCH --job-name=fasttree_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=1-0:00:00
#SBATCH --chdir=/data/zool-microbe-genomics/spet4850/campy_analysis/ClonalFrameML2/
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%j.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=spet4850@spc.ox.ac.uk

module load Anaconda3/2020.11 

source activate /data/zool-microbe-genomics/spet4850/clonalframe

ClonalFrameML Saureus.phyML.newick Saureus.non-core-sites.txt Saureus_output
