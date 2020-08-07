#!/bin/bash

#SBATCH --array=1
#SBATCH --job-name=fasttree_%A_%a
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=0-2:00:00
#SBATCH --workdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%A_%a.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%A_%a.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=spet4850@spc.ox.ac.uk

module purge
module load slurm
module load fasttree/2.1.9

# Get the 'nth' file where n is the array ID
file=$(ls *-aligned.fasta | sed -n ${SLURM_ARRAY_TASK_ID}p)

echo making_fasttree_with_gtr+cat "${file}"

FastTreeMP -gtr -nt < "${file}" > "${file/-aligned.fasta/-gtr_cat.nwk}"
