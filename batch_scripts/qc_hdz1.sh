#!/bin/bash
#SBATCH --time=00:45:00
#SBATCH --job-name=qc_hdz1
#SBATCH --error=/work/richlab/aliciarich/bioinformatics_stats/logs/qc_hdz1_%A_%a.err
#SBATCH --output=/work/richlab/aliciarich/bioinformatics_stats/logs/qc_hdz1_%A_%a.out
#SBATCH --partition=guest
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=100GB

module purge
module load anaconda

conda activate filter

trimmed="/work/richlab/aliciarich/ont_reads/loris_microbiome/trimmed/hdz1/"
filtered="/work/richlab/aliciarich/ont_reads/loris_microbiome/filtered/hdz1/"

mkdir -p $filtered

cd $trimmed


for file in "$trimmed"/*.fastq; do
    
    if [ -f "$file" ]; then
       
        base_filename=$(basename "$file")

        chopper --maxlength 2000 --minlength 1000 --quality 7 --input "$file" > "$filtered/$base_filename"

        echo "Processed $file"
    else
        echo "Error: File $file does not exist or is not a regular file."
    fi
done

cd $filtered

for file in "$filtered"/*.fastq; do
    if [ -f "$file" ]; then
        base_filename=$(basename "$file" .fastq)
        
        mkdir -p "$filtered/$base_filename"
        
        mv "$file" "$filtered/$base_filename/${base_filename}.fastq"
        
        echo "Organized $file"
    else
        echo "Error: File $file does not exist or is not a regular file."
    fi
done