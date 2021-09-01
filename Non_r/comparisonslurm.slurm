
#!/bin/bash


#SBATCH --job-name=clonalframe_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=1-0:00:00
#SBATCH --chdir=/data/zool-microbe-genomics/spet4850/campy_analysis/data/CC353/
#SBATCH --output=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%j.out
#SBATCH --error=/data/zool-microbe-genomics/spet4850/campy_analysis/logs/fasttree_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=spet4850@spc.ox.ac.uk

module load Anaconda3/2020.11

source activate /data/zool-microbe-genomics/spet4850/clonalframe

ClonalFrameML CC353_alignedto11168.fas.nwk CC353_alignedto11168.fas CC353_alignedto11168_clonalframe_output

