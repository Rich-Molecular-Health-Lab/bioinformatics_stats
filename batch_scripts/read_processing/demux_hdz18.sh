#!/bin/bash
#SBATCH --time=0:45:00
#SBATCH --job-name=hdz18demux
#SBATCH --error=/work/richlab/aliciarich/microbiomes_loris/logs/hdz18demux_%A_%a.err
#SBATCH --output=/work/richlab/aliciarich/microbiomes_loris/logs/hdz18demux_%A_%a.out
#SBATCH --partition=guest
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=100GB

module purge
module load dorado/0.7

basecalled="/work/richlab/aliciarich/ont_reads/loris_microbiome/basecalled/hdz18/hdz18.bam"
sample_sheet="/work/richlab/aliciarich/microbiomes_loris/dataframes/dorado_sample_sheet_hdz18.csv"
trimmed="/work/richlab/aliciarich/ont_reads/loris_microbiome/trimmed/hdz18"

mkdir -p $trimmed

dorado demux "$basecalled" \
    --output-dir "$trimmed" \
    --kit-name "SQK-16S114-24" \
    --sample-sheet "$sample_sheet" \
    --emit-fastq --emit-summary
