#!/bin/bash
#SBATCH --time=03:00:00
#SBATCH --job-name=wf16s_hdz18
#SBATCH --error=/work/richlab/aliciarich/bioinformatics_stats/logs/wf16s_hdz18_%A_%a.err
#SBATCH --output=/work/richlab/aliciarich/bioinformatics_stats/logs/wf16s_hdz18_%A_%a.out
#SBATCH --partition=guest
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=350GB

module purge
module load dorado/0.7

filtered="/work/richlab/aliciarich/ont_reads/loris_microbiome/filtered/hdz18/"

cd "/work/richlab/aliciarich/bioinformatics_stats/"

processed="../bioinformatics_stats/data/loris/outputs_wf16s/hdz18/"

module load "nextflow"

nextflow run epi2me-labs/wf-16s \
-profile singularity \
--fastq "$filtered" \
--taxonomic_rank S \
--keep_bam \
--minimap2_by_reference \
--out_dir "$processed" \
--min_len 1000 \
--max_len 2000 \
--abundance_threshold 0 \
--min_read_qual 7 \
--min_percent_identity 85 \
--min_ref_coverage 80 \
--n_taxa_barplot 12 \
--threads 32