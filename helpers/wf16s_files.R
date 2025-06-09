wf16s.files <- tibble(
  Name = c(
    "Species-Level Abundance Table",
    "Wf-16s Summary Report",
    "Read Alignment Tables"
  ),
  Filename = c(
    "abundance_table_species.tsv",
    "wf-16s-report.html",
    "wf-metagenomics-alignment.csv"
  ),
  Location = c(
    "output directory created by wf-16s - transfer this to the path bioinformatics_stats/data/loris/ and rename with the sequencing run id as the file prefix.",
    "This is in the output directory from wf-16s. Open the html report in your browser, and from there you can download other files for analysis.",
    "You must individually download one table for each sample from the html summary file produced by wf-16s (see below)."
  )
) %>%
  gt::gt(rowname_col = "Name") %>%
  gt::opt_stylize(style = 3, color = "gray")

gt::gtsave(wf16s.files, "visuals/tbl_wf16s_files.png")