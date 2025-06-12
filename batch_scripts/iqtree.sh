#!/bin/bash
#SBATCH --job-name=iqtree
#SBATCH --error=/work/richlab/aliciarich/bioinformatics_stats/logs/iqtree_%A.err
#SBATCH --output=/work/richlab/aliciarich/bioinformatics_stats/logs/iqtree_%A.out
#SBATCH --partition=guest
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8         
#SBATCH --mem=16G                 
#SBATCH --time=2:00:00            

module purge
module load iqtree/2.3

aligned_fasta="/work/richlab/aliciarich/bioinformatics_stats/microeco/loris/refs_aligned_mafft.fasta"
out_prefix="/work/richlab/aliciarich/bioinformatics_stats/microeco/loris/refs_tree"

iqtree -s $aligned_fasta \
        -m MFP+MERGE \
        -B 1000 \
        -T $SLURM_CPUS_PER_TASK \
        -pre "$out_prefix" \
        -redo