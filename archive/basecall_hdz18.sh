#!/bin/bash
#SBATCH --time=2:00:00
#SBATCH --job-name=basecall_hdz18
#SBATCH --error=/work/richlab/aliciarich/bioinformatics_stats/logs/basecall_hdz18_%A_%a.err
#SBATCH --output=/work/richlab/aliciarich/bioinformatics_stats/logs/basecall_hdz18_%A_%a.out
#SBATCH --partition=gpu,guest_gpu
#SBATCH --constraint='gpu_v100|gpu_t4'
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=200GB
#SBATCH --gres=gpu:1

module purge
module load dorado-gpu/0.7

pod5="/work/richlab/aliciarich/ont_reads/loris_microbiome/hdz_raw/hdz18/"
basecalled="/work/richlab/aliciarich/ont_reads/loris_microbiome/basecalled/hdz18/"
mkdir -p $basecalled

algo="/work/richlab/aliciarich/ont_reads/dna_r10.4.1_e8.2_400bps_sup@v5.0.0/"

dorado basecaller "$algo" \
    "$pod5" \
    --recursive \
    --no-trim \
    > "$basecalled/hdz18.bam" && \
dorado summary "$basecalled/hdz18.bam" > "$basecalled/hdz18_basecall_summary.tsv"