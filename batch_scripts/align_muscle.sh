#!/bin/bash
#SBATCH --time=3:00:00
#SBATCH --job-name=align_muscle
#SBATCH --error=/work/richlab/aliciarich/bioinformatics_stats/logs/align_muscle_%A_%a.err
#SBATCH --output=/work/richlab/aliciarich/bioinformatics_stats/logs/align_muscle_%A_%a.out
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=250GB
#SBATCH --partition=guest

module purge
module load muscle/5.1

input_fasta="/work/richlab/aliciarich/bioinformatics_stats/microeco/loris/refs_renamed.fasta"
aligned_fasta="/work/richlab/aliciarich/bioinformatics_stats/microeco/loris/refs_aligned.fasta"

muscle -super5 $input_fasta -output $aligned_fasta