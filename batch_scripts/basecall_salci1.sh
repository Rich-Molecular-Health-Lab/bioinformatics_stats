seqrun="salci1"
sampleset="isolates"

cd GitRepos

dorado basecaller sup \
    "seqRuns/$seqrun/pod5" \
    --recursive \
    --no-trim \
    > "reads/basecalled/$seqrun.bam" && \
dorado summary "reads/basecalled/$seqrun.bam" > "bioinformatics_stats/$sampleset/summary_$seqrun.tsv"
