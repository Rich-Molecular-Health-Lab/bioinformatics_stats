
seqruns      <- seqruns %>% keep_at(params$sampleset) %>% list_flatten(name_spec = "")
subject_list <- keep_at(subjects, paste0(params$sampleset)) %>% list_flatten(name_spec = "{inner}")

metadata <- read.table(path$metadata$summary, header = T, sep = "\t")  %>%
  select(all_of(names(metadata_key))) %>%
  mutate(across(all_of(date.vars), ~ ymd(.)),
         across(all_of(binary.vars), ~ as.character(.)),
         across(all_of(ids),         ~ str_to_lower(.))) %>%
  mutate(
    Subject        = factor(str_to_lower(Subject), levels = c("culi", "warble", "unknown")),
    StudbookID     = factor(StudbookID, levels = unique(StudbookID)),
    Sex           = factor(Sex, levels = unique(Sex)),
    MotherID      = factor(MotherID, levels = unique(MotherID)),
    FatherID      = factor(FatherID, levels = unique(FatherID)),
    BirthLocation = factor(BirthLocation, levels = unique(BirthLocation)),
    BirthYear     = year(make_date(year = BirthYear))
  ) %>%
  mutate(across(all_of(binary.vars), ~ case_when(. == "1" ~ "yes",
                                                 . == "0" ~ "no",
                                                 .default = .))) %>%
  filter(!is.na(CollectionDate)) %>%
  mutate(across(any_of(replace.unknowns), ~ 
                  if_else(is.na(.), replace_na("unknown"), .))) %>%
  arrange(study_day, Subject)

sample.list <- metadata %>% 
  distinct(SampleID, Subject, CollectionDate) %>%
  arrange(Subject, CollectionDate) %>%
  distinct(SampleID) %>%
  map(\(x) as.list(x)) %>%
  list_flatten(name_spec = "")

samples <- sample.list %>% unlist()

alignment.files.list <- list.files(path       = path$read_alignments, 
                                   pattern    = "*_wf-metagenomics-alignment.csv$", 
                                   full.names = TRUE)
alignment.filenames  <- list.files(path       = path$read_alignments, 
                                   pattern    = "*_wf-metagenomics-alignment.csv$", 
                                   full.names = FALSE)
alignment.files      <- lapply(alignment.files.list, read_alignment_file)
alias                <- str_remove(alignment.filenames, "_wf-metagenomics-alignment.csv")

alignment.files <- Map(function(df, id) {
  if (nrow(df) > 0) {
    df$alias <- id
  } else {
    warning(paste("Data frame for", id, "is empty. Alias not assigned."))
  }
  return(df)
}, alignment.files, alias)

alignments.long     <- bind_rows(alignment.files) %>%
  as_tibble() %>% fix.strings() %>% 
  select(ref,
         taxid,
         species,
         genus,
         family,
         order,
         class,
         phylum,
         superkingdom,
         SequenceID = alias,
         coverage,
         n_reads = number.of.reads)    %>%
  left_join(select(metadata, c(SequenceID, SampleID)), 
            by = join_by(SequenceID)) %>%
  select(-SequenceID) %>%
  filter(coverage >= methods_16s$min_cov & !is.na(SampleID)) %>%
  group_by(ref,
           taxid,
           species,
           genus,
           family,
           order,
           class,
           phylum,
           superkingdom,
           SampleID) %>%
  summarize(samp_cov     = mean(coverage),
            samp_n_reads = mean(n_reads)) %>% ungroup() %>%
  mutate(SampleID = factor(SampleID, levels = unique(samples))) %>%
  arrange(SampleID) %>%
  sort.taxa()

alignments.refs <- alignments.long %>% 
  group_by(ref,
           taxid,
           species,
           genus,
           family,
           order,
           class,
           phylum,
           superkingdom) %>%
  summarize(ref_n_reads   = round(sum(samp_n_reads),  digits = 2),
            ref_mean_cov  = round(mean(samp_cov),     digits = 2)) %>%
  ungroup() %>% group_by(species,
                         genus,
                         family,
                         order,
                         class,
                         phylum,
                         superkingdom) %>%
  arrange(species, desc(ref_n_reads), desc(ref_mean_cov)) %>%
  mutate(ref_order       = row_number(),
         tax_total_count = sum(ref_n_reads)) %>%
  ungroup() %>%
  filter(ref_order == 1) %>%
  select(-ref_order) %>% sort.taxa()

alignments    <- alignments.long %>% 
  group_by(superkingdom,
           phylum,
           class,
           order,
           family,
           genus,
           species,
           SampleID) %>%
  summarize(counts = sum(samp_n_reads)) %>%
  ungroup() %>%
  left_join(alignments.refs,
            by = join_by(superkingdom,
                         phylum,
                         class,
                         order,
                         family,
                         genus,
                         species)) %>%
  pivot_wider(id_cols    = c(ref,
                             taxid,
                             taxonomy.ordered,
                             ref_mean_cov),
              names_from  = SampleID,
              values_from = counts) %>%
  sort.taxa() %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)),
         organism = str_glue("txid", "{taxid}")) 

depth <- alignments.long %>%
  select(SampleID,
         samp_cov,
         samp_n_reads) %>%
  group_by(SampleID) %>%
  summarize(depth         = round(sum(samp_n_reads), digits = 0),
            mean_coverage = round(mean(samp_cov), digits = 2)) %>%
  mutate(mean_coverage = mean_coverage/100) %>%
  ungroup() %>%
  left_join(metadata, by = join_by(SampleID)) %>%
  distinct(
    CollectionDate,
    study_day,
    Subject,
    SampleID,
    depth,
    mean_coverage,
    probiotic,
    oatgel,
    steroid,
    cauliflower,
    tomatoes,
    broccoli,
    med_type,
    med_name,
    med_dose,
    enclosure,
    access,
    estrus,
    pregnant,
    bristol_mean,
    health_note,
    repro_note,
    SampleNotes
  ) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "yes", "\u2714\ufe0f")),
         across(where(is.character), ~ str_replace_all(., "^no$", "\u2718")),
         across(where(is.character), ~ str_replace_all(., "unknown", "\u2754")),
         across(where(is.character), ~str_replace_all(., "none", "")),
         across(c("estrus", "pregnant"), ~if_else(Subject == "culi", NA, .)),
         steroid = if_else(steroid == 0.0, "\u2718", as.character(steroid)),
         med_dose= if_else(med_dose    == 0.0, "", as.character(med_dose))
  ) %>%
  arrange(CollectionDate, Subject)

depth_table <- reactable(
  depth,
  groupBy = c("CollectionDate"),
  defaultPageSize = 25,
  sortable = TRUE,
  resizable = TRUE,
  height = 900,
  fullWidth = TRUE,
  compact = TRUE, 
  showPagination = TRUE,
  showPageSizeOptions = TRUE,
  columns = list(
    CollectionDate = colDef(name = "Date Collected", 
                            format = colFormat(date = TRUE), sticky = "left"),
    Subject        = colDef(name = "Subject", aggregate = "frequency", filterable = TRUE),
    SampleID       = colDef(name = "Sample", aggregate = "count"),
    depth          = colDef(name = "Read Count", aggregate = "frequency", 
                            filterable = TRUE, html = TRUE,
                            style= function(value) {
                              if (value >= 10000) {
                                color <- "#3A488AFF"
                              } else if (value >= 7000 & value < 10000) {
                                color <- "#8785B2FF"
                              } else if (value >= 5000 & value < 7000) {
                                color <- "#DABD61FF"
                              } else if (value >= 1000 & value < 5000) {
                                color <- "#D95F30FF"
                              } else {
                                color <- "#BE3428FF"
                              }
                              list(color = color, fontWeight = "bold")
                            },
                            format = colFormat(separators = TRUE),
                            aggregated = JS("
    function(cellInfo) {
      let values = cellInfo.subRows.map(row => row['depth']);
      let uniqueValues = [...new Set(values)];

      // Function to determine color based on value
      function getColor(value) {
        if (value >= 10000) {
          return '#3A488AFF';
        } else if (value >= 7000 && value < 10000) {
          return '#8785B2FF';
        } else if (value >= 5000 && value < 7000) {
          return '#DABD61FF';
        } else if (value >= 1000 && value < 5000) {
          return '#D95F30FF';
        } else {
          return '#BE3428FF';
        }
      }

      // Format unique values with their respective colors
      return uniqueValues
        .map(v => {
          let color = getColor(v);
          return `<span style='color: ${color}; font-weight: bold;'>${Number(v).toLocaleString()}</span>`;
        })
        .join('; ');
    }
  ")
    ),
    mean_coverage  = colDef(name = "Mean Taxonomic Coverage", aggregate = "frequency", html = TRUE,
                            format = colFormat(percent = TRUE, digits = 1),
                            style= function(value) {
                              if (value >= .98) {
                                color <- "#3A488AFF"
                              } else if (value >= .97 & value < .98) {
                                color <- "#8785B2FF"
                              } else if (value >= .96 & value < .97) {
                                color <- "#DABD61FF"
                              } else if (value >= .95 & value < .96) {
                                color <- "#D95F30FF"
                              } else {
                                color <- "#BE3428FF"
                              }
                              list(color = color, fontWeight = "bold")
                            },
                            aggregated = JS("
    function(cellInfo) {
      let values = cellInfo.subRows.map(row => row['mean_coverage']);
      let uniqueValues = [...new Set(values)];
      // Function to determine color based on value
      function getColor(value) {
        if (value >= .98) {
          return '#3A488AFF';
        } else if (value >= .97 & value < .98) {
          return '#8785B2FF';
        } else if (value >= .96 & value < .97) {
          return '#DABD61FF';
        } else if (value >= .95 & value < .96) {
          return '#D95F30FF';
        } else {
          return '#BE3428FF';
        }
      }
      // Format unique values as percentages with 1 decimal place and style them
      return uniqueValues
        .map(v => {
          let color = getColor(v);
          return `<span style='color: ${color}; font-weight: bold;'>${(v * 100).toFixed(1)}%</span>`;
        })
        .join('; ');
    
    } 
  ")),
    probiotic      = colDef(name = "Probiotic", aggregate = "unique", filterable = TRUE),
    oatgel         = colDef(name = "Oat Gel", aggregate = "unique", filterable = TRUE),
    steroid        = colDef(name = "Steroid (mg/day)", aggregate = "unique", filterable = TRUE),
    cauliflower    = colDef(name = "Cauliflower", aggregate = "unique", filterable = TRUE),
    tomatoes       = colDef(name = "Tomatoes", aggregate = "unique", filterable = TRUE),
    broccoli       = colDef(name = "Broccoli", aggregate = "unique", filterable = TRUE),
    med_type       = colDef(name = "Type", aggregate = "unique", filterable = TRUE),
    med_name       = colDef(name = "Name", aggregate = "unique"),
    med_dose       = colDef(name = "Dose (mg/day)", aggregate = "unique"),
    enclosure      = colDef(name = "Old or New Enclosure", aggregate = "unique", filterable = TRUE),
    access         = colDef(name = "Breeding Access", aggregate = "unique", filterable = TRUE),
    estrus         = colDef(name = "In Estrus", aggregate = "unique", filterable = TRUE),
    pregnant       = colDef(name = "Pregnant", aggregate = "unique", filterable = TRUE),
    bristol_mean   = colDef(name = "Mean Bristol Score", aggregate = "mean"),
    health_note    = colDef(name = "Health", aggregate = "unique"),
    repro_note     = colDef(name = "Reproduction", aggregate = "unique"),
    SampleNotes    = colDef(name = "Sample Collection", aggregate = "unique"),
    study_day      = colDef(show = FALSE)
  ),
  columnGroups = list(
    colGroup(name = "Diet Trial", columns = c("probiotic", "oatgel", "steroid")),
    colGroup(name = "Foods in Rotation", columns = c("cauliflower", "tomatoes", "broccoli")),
    colGroup(name = "Medication Administered", columns = c("med_type", "med_name", "med_dose")),
    colGroup(name = "Misc. Notes", columns = c("health_note", "repro_note", "SampleNotes"))
  )
)

depth.plot <- depth %>% filter(Subject == "warble" | Subject == "culi") %>%
  ggplot(aes(depth, after_stat(count))) +
  geom_histogram(binwidth = 1000, fill = "#8785B2FF", color = "black") +
  geom_text(
    stat = "bin",
    binwidth = 1000,
    aes(label = after_stat(count), fontface = "bold"),
    vjust = -0.5,
    color = "black",
    size = 4
  ) +
  theme_classic() +
  scale_x_continuous(
    breaks = seq(0, max(depth$depth, na.rm = TRUE), by = 1000),  
    labels = scales::comma,
    limits = c(0, 15000)
  ) +
  labs(
    title = "Frequency of Read Counts",
    x = "Depth (Read Count)",
    y = "Frequency"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for clarity
  )

