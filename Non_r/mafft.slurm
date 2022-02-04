#!/bin/bash

#SBATCH --array=1-1355
#SBATCH --job-name=mafft_%A_%a
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --time=0-1:00:00
#SBATCH --workdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/mafft_%A_%a.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/mafft_%A_%a.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=spet4850@spc.ox.ac.uk

module purge
module load slurm
module load mafft/7.305

# Get the 'nth' file where n is the array ID
file=$(ls CAMP*.xmfa | sed -n ${SLURM_ARRAY_TASK_ID}p)

echo Aligning "${file}"

/data/zool-microbe-genomics/spet4850/campy_analysis/code/xmfa2fasta.pl --file "${file}" > "${file/.xmfa/.fasta}"

mafft --thread "${SLURM_CPUS_PER_TASK}" "${file/.xmfa/.fasta}" > "${file/.xmfa/-aligned.fasta}"
