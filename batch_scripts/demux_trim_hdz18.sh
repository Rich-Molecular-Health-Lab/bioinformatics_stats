#!/bin/bash
#SBATCH --time=00:45:00
#SBATCH --job-name=demux_trim_hdz18
#SBATCH --error=/work/richlab/aliciarich/bioinformatics_stats/logs/demux_trim_hdz18_%A_%a.err
#SBATCH --output=/work/richlab/aliciarich/bioinformatics_stats/logs/demux_trim_hdz18_%A_%a.out
#SBATCH --partition=guest
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=100GB

module purge
module load dorado/0.7

cd /work/richlab/aliciarich/bioinformatics_stats

basecalled="/work/richlab/aliciarich/ont_reads/loris_microbiome/basecalled/hdz18/"
sample_sheet="/work/richlab/aliciarich/bioinformatics_stats/dataframes/sample_sheet/loris/params$sampleset/hdz18_sample_sheet.csv"
trimmed="/work/richlab/aliciarich/ont_reads/loris_microbiome/trimmed/hdz18/"

mkdir -p $trimmed

dorado demux "$basecalled" \
    --output-dir "$trimmed" \
    --kit-name "'SQK-16S114-24'" \
    --sample-sheet "$sample_sheet" \
    --emit-fastq --emit-summary