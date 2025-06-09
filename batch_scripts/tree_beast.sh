#!/bin/bash
#SBATCH --time=1:00:00
#SBATCH --job-name=tree_beast
#SBATCH --error=/work/richlab/aliciarich/bioinformatics_stats/logs/tree_beast_%A_%a.err
#SBATCH --output=/work/richlab/aliciarich/bioinformatics_stats/logs/tree_beast_%A_%a.out
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=250GB
#SBATCH --partition=guest

module purge
module load beast2/2.7

aligned_fasta="/work/richlab/aliciarich/bioinformatics_stats/microeco/loris/refs_aligned.fasta"

