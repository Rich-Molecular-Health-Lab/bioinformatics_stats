here::i_am("setup/global/setup.R")

global            <- config::get(config = "default")
swan              <- config::get(config = "swan")
micro             <- config::get(config = "microbiome")
notebooks         <- config::get(config = "notebooks")
modules           <- config::get(config = "modules")
widgets           <- config::get(config = "widgets")
loris             <- config::get(config = "loris")
isolates          <- config::get(config = "isoaltes")
methods_16s       <- config::get(config = "methods_16s")
sample_sheets     <- config::get(config = "sample_sheets")
abund_wf16s_files <- config::get(config = "abund_wf16s_files")
barcode_alignments<- config::get(config = "barcode_alignments")

source(here::here(global$packages))
source(here::here(global$conflicts))
source(here::here(global$functions))
source(here::here(global$inputs))
source(here::here(global$knit_engines))

opts_chunk$set(message = FALSE,
               warning = FALSE,
               echo    = TRUE,
               include = TRUE,
               eval    = TRUE,
               comment = "")

seqruns      <- seqruns %>% keep_at(params$sampleset) %>% list_flatten(name_spec = "")
subject_list <- keep_at(subjects, paste0(params$sampleset)) %>% list_flatten(name_spec = "{inner}")
path         <- config::get(config = paste0(params$sampleset))