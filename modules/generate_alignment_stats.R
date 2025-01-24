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
  groupBy             = c("CollectionDate"),
  defaultPageSize     = 25,
  sortable            = TRUE,
  resizable           = TRUE,
  height              = 900,
  fullWidth           = TRUE,
  compact             = TRUE, 
  showPagination      = TRUE,
  showPageSizeOptions = TRUE,
  columns = list(
    CollectionDate = colDef.CollectionDate ,
    Subject        = colDef.Subject        ,
    SampleID       = colDef.SampleID       ,
    depth          = colDef.depth          ,
    mean_coverage  = colDef.mean_coverage  ,
    probiotic      = colDef.probiotic      ,
    oatgel         = colDef.oatgel         ,
    steroid        = colDef.steroid        ,
    cauliflower    = colDef.cauliflower    ,
    tomatoes       = colDef.tomatoes       ,
    broccoli       = colDef.broccoli       ,
    med_type       = colDef.med_type       ,
    med_name       = colDef.med_name       ,
    med_dose       = colDef.med_dose       ,
    enclosure      = colDef.enclosure      ,
    access         = colDef.access         ,
    estrus         = colDef.estrus         ,
    pregnant       = colDef.pregnant       ,
    bristol_mean   = colDef.bristol_mean   ,
    health_note    = colDef.health_note    ,
    repro_note     = colDef.repro_note     ,
    SampleNotes    = colDef.SampleNotes    ,
    study_day      = colDef(show = FALSE)
  ),
  columnGroups = list(
    colGroup(name = "Diet Trial"             , columns = c("probiotic"  , "oatgel"    , "steroid")),
    colGroup(name = "Foods in Rotation"      , columns = c("cauliflower", "tomatoes"  , "broccoli")),
    colGroup(name = "Medication Administered", columns = c("med_type"   , "med_name"  , "med_dose")),
    colGroup(name = "Misc. Notes"            , columns = c("health_note", "repro_note", "SampleNotes"))
  )
)

depth_list <- as.list(depth$depth) %>% set_names(depth$SampleID)

max_depth <- as.numeric(max(depth$depth))

thresholds <- list(
  "3000" = keep(depth_list, \(x) all(x < 3000)),
  "4000" = keep(depth_list, \(x) all(x < 4000)),
  "5000" = keep(depth_list, \(x) all(x < 5000)),
  "6000" = keep(depth_list, \(x) all(x < 6000)),
  "7000" = keep(depth_list, \(x) all(x < 7000)),
  "8000" = keep(depth_list, \(x) all(x < 8000)),
  "9000" = keep(depth_list, \(x) all(x < 9000)),
  "Total_N" = keep(depth_list, \(x) all(x <= max_depth))
)

total <- c(as.numeric(nrow(depth)))

depth_summary <- map(thresholds, \(x) length(x)) %>% set_names(names(thresholds)) %>%
  enframe(name = "Threshold", value = "Samples_Lost") %>%
  unnest_longer("Samples_Lost") %>%
  rowwise() %>%
  mutate(Prop_Lost = Samples_Lost/total)

gt_depth_summary <- depth_summary %>%
  gt(rowname_col = "Threshold") %>%
  fmt_percent(columns = "Prop_Lost") %>%
  tab_header("Total Samples Lost by Rarefaction Threshold") %>%
  tab_stubhead("Rarefaction Thresholds for SRS") %>%
  tab_spanner("Loss in Sample Size",
              columns = c("Samples_Lost", "Prop_Lost")) %>%
  tab_style(style = list(
    cell_text(weight    = "bold", 
              transform = "uppercase", 
              stretch   = "condensed",
              style     = "oblique")),
    locations = list(cells_body(rows = Threshold == "Total_N"),
                     cells_stub(rows = Threshold == "Total_N"))) %>%
  cols_label(Samples_Lost = "Count (N)",
             Prop_Lost    = "Percent of Total N") %>%
  cols_width(stub() ~ px(80)) %>%
  text_case_match("Total_N" ~ "Total N", .locations = cells_stub()) %>%
  opt_align_table_header("left") %>%
  opt_stylize(style = 2, color = "gray")

gtsave(gt_depth_summary, paste0(params$sampleset, "_depth_summary.png"), global$visuals)

depth.plot <- ggplot(depth, aes(depth)) +
  geom_dotplot(method="histodot", binwidth = 1000) +
  xlim(0, 30000)

depth_plot <- renderPlot({depth.plot})

ggsave(paste0(params$sampleset, "_depth_histogram.png"),
       depth.plot,
       path = global$visuals,
       dpi  = 320)

