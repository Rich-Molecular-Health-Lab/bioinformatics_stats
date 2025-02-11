
metadata <- read.table(path$metadata$summary, header = T, sep = "\t")  %>%
  filter(steps_remaining == "sample extracted and sequenced") %>%
  filter(!is.na(CollectionDate)) %>%
  select(all_of(microeco_cols)) %>%
  mutate(across(all_of(date.vars),   ~ ymd(.)),
         across(all_of(yn.vars),     ~ str_to_lower(as.character(.))),
         across(all_of(ids),         ~ str_to_lower(.))) %>%
  mutate(Subject        = str_to_lower(subject)) %>%
  arrange(study_day, Subject)

sample.list <- metadata %>% 
  distinct(identifier, Subject, CollectionDate) %>%
  arrange(Subject, CollectionDate) %>%
  distinct(identifier) %>%
  map(\(x) as.list(x)) %>%
  list_flatten(name_spec = "")

samples <- sample.list %>% unlist()

samp.list.culi  <- metadata %>% 
  filter(Subject == "culi") %>%
  distinct(identifier, CollectionDate) %>%
  arrange(CollectionDate) %>%
  distinct(identifier) %>%
  map(\(x) as.list(x)) %>%
  list_flatten(name_spec = "")

samples.culi       <- samp.list.culi %>% unlist()



samp.list.warb  <- metadata %>% 
  filter(Subject == "warble") %>%
  distinct(identifier, CollectionDate) %>%
  arrange(CollectionDate) %>%
  distinct(identifier) %>%
  map(\(x) as.list(x)) %>%
  list_flatten(name_spec = "")

samples.warb <- samp.list.warb %>% unlist()

libraries <- metadata %>%
  arrange(LibPrepDate) %>%
  select(LibraryCode, identifier) %>%
  group_by(LibraryCode) %>%
  group_map(~ {
    setNames(list(as.list(.x$identifier)), .y$LibraryCode)
  }, .keep = TRUE) %>%
  flatten()

working_libraries <- libraries %>%
  keep_at(paste0(params$seqrun)) %>%
  list_c()


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
         identifier = alias,
         coverage,
         n_reads = number.of.reads)    %>%
  left_join(select(metadata, identifier), 
            by = join_by(identifier)) %>%
  filter(coverage >= methods_16s$min_cov & !is.na(identifier)) %>%
  group_by(ref,
           taxid,
           species,
           genus,
           family,
           order,
           class,
           phylum,
           superkingdom,
           identifier) %>%
  summarize(samp_cov     = mean(coverage),
            samp_n_reads = mean(n_reads)) %>% ungroup() %>%
  mutate(identifier = factor(identifier, levels = unique(samples))) %>%
  arrange(identifier) %>%
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
           identifier) %>%
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
              names_from  = identifier,
              values_from = counts) %>%
  sort.taxa() %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)),
         organism = str_glue("txid", "{taxid}")) 


depth <- alignments.long %>%
  filter(!is.na(identifier)) %>%
  select(identifier,
         samp_cov,
         samp_n_reads) %>%
  group_by(identifier) %>%
  summarize(depth         = round(sum(samp_n_reads), digits = 0),
            mean_coverage = round(mean(samp_cov), digits = 2)) %>%
  mutate(mean_coverage = mean_coverage/100) %>%
  ungroup() %>%
  left_join(metadata, by = join_by(identifier)) %>%
  select(all_of(coverage_table_cols)) %>%
  distinct()


foods_expand    <- read.table(path$metadata$foods   , sep = "\t", header = TRUE)
fats_expand     <- read.table(path$metadata$fats    , sep = "\t", header = TRUE)
proteins_expand <- read.table(path$metadata$proteins, sep = "\t", header = TRUE)
CHOs_expand     <- read.table(path$metadata$CHOs    , sep = "\t", header = TRUE)
Ash_expand      <- read.table(path$metadata$Ash     , sep = "\t", header = TRUE)
vitamins_expand <- read.table(path$metadata$vitamins, sep = "\t", header = TRUE)

depth_summary <- depth %>% 
  mutate(
    diet_color            = as.character(fct_recode(diet_name, !!!diet_colors)),
    holding_color         = as.character(fct_recode(holding, !!!holding_colors)),
    warb_status_color     = as.character(fct_recode(warb_status, !!!warb_status_colors)),
    color_Subj_Certainty  = as.character(fct_recode(Subj_Certainty, !!!certainty_colors)),
    icon_Subj_Certainty   = as.character(fct_recode(Subj_Certainty, !!!certainty_icons)),
    color_pair_access     = as.character(fct_recode(pair_access, !!!pair_access_colors)),
    icon_pair_access      = as.character(fct_recode(pair_access, !!!pair_access_icons)),
    color_subject         = as.character(fct_recode(Subject, !!!subj_colors)),
    icon_subject          = as.character(fct_recode(Subject, !!!subj_icons)),
    vitamins              = "Expand Details"
  ) %>%
  mutate(diet_name    = fct_recode(diet_name, !!!rename_diets),
         subject      = str_to_title(Subject),
         holding      = str_to_title(holding),
         pair_access  = str_to_title(pair_access),
         warb_status  = str_to_title(warb_status),
         across(all_of(dose_cols), ~ rescale_dose(.x, max(.x))), .keep = "unused") %>%
  relocate(ends_with("fed"), .after = "total_mg_dry") %>%
  select(all_of(depth_table_cols)) %>%
  arrange(study_day, subject)

