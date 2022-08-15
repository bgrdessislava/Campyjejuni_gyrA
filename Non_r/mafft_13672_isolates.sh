#!/bin/bash

#SBATCH --job-name=mafft__%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --time=0-1:00:00
#SBATCH --chdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/mafft_%j.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/mafft_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=spet4850@spc.ox.ac.uk

module purge
module load slurm
module load mafft/7.305

# Get the 'nth' file where n is the array ID
file=$1

echo Aligning "${file}"

mafft --thread "${SLURM_CPUS_PER_TASK}" "${file}" > "${file/.fasta.gz/-aligned.fasta.gz}"
