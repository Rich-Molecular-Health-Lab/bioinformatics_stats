#!/bin/bash
#SBATCH --time=3:00:00
#SBATCH --job-name=align_tree_hdz9
#SBATCH --error=/work/richlab/aliciarich/bioinformatics_stats/logs/align_tree_hdz9_%A_%a.err
#SBATCH --output=/work/richlab/aliciarich/bioinformatics_stats/logs/align_tree_hdz9_%A_%a.out
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=250GB
#SBATCH --partition=guest

module purge
module load mafft 
module load fasttree 

input_fasta="/work/richlab/aliciarich/bioinformatics_stats/tmp/tmp4.fasta"
aligned_fasta="/work/richlab/aliciarich/bioinformatics_stats/data/loris/taxonomy/refseqs_aligned.fasta"
tree_file="/work/richlab/aliciarich/bioinformatics_stats/data/loris/taxonomy/refseqs_tree.newick"

mafft --auto $input_fasta > $aligned_fasta

FastTree -nt $aligned_fasta > $tree_file