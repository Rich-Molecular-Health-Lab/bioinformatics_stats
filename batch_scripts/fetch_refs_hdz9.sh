#!/bin/bash
#SBATCH --time=1:00:00
#SBATCH --job-name=fetch_refs_hdz9
#SBATCH --error=/work/richlab/aliciarich/bioinformatics_stats/logs/fetch_refs_hdz9_%A_%a.err
#SBATCH --output=/work/richlab/aliciarich/bioinformatics_stats/logs/fetch_refs_hdz9_%A_%a.out
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=100GB
#SBATCH --partition=guest

module purge
module load entrez-direct
module load seqkit 

uid_file="/work/richlab/aliciarich/bioinformatics_stats/tmp/fetch_references.txt"
accessions_file="/work/richlab/aliciarich/bioinformatics_stats/tmp/accessions.txt"
fasta_file="/work/richlab/aliciarich/bioinformatics_stats/tmp/tmp1.fasta"
renamed_fasta="/work/richlab/aliciarich/bioinformatics_stats/tmp/tmp2.fasta"
fasta_out="/work/richlab/aliciarich/bioinformatics_stats/tmp/tmp3.fasta"


> "$fasta_file"

cut -f1 "$uid_file" > "$accessions_file"

if efetch -db nuccore -input "$accessions_file" -format fasta -email 'aliciarich@unomaha.edu' > "$fasta_file"; then
    echo "Sequences fetched successfully."
else
    echo "Error fetching sequences." >&2
    exit 1
fi

seqkit replace $fasta_file -p '^(\S+)(.+?)$' -r '{kv}$2' -k $uid_file -o $renamed_fasta

rm $fasta_file
mv $renamed_fasta $fasta_out