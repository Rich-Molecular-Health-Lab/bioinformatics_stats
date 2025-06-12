#!/bin/bash
#SBATCH --job-name=align_mafft
#SBATCH --error=/work/richlab/aliciarich/bioinformatics_stats/logs/align_mafft_%A.err
#SBATCH --output=/work/richlab/aliciarich/bioinformatics_stats/logs/align_mafft_%A.out
#SBATCH --partition=guest
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8         
#SBATCH --mem=64G                 
#SBATCH --time=6:00:00            

module purge
module load mafft/7.526

input_fasta="/work/richlab/aliciarich/bioinformatics_stats/microeco/loris/refs_renamed.fasta"
aligned_fasta="/work/richlab/aliciarich/bioinformatics_stats/microeco/loris/refs_aligned_mafft.fasta"

mafft \
  --thread 8 \
  --retree 2 \
  --maxiterate 1000 \
  "$input_fasta" > "$aligned_fasta"