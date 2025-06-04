knitr::knit_engines$set(terminal = function(options) {
  code <- paste(options$code, collapse = "\n")
  
  params <- map(params, ~ if (is.atomic(.)) {list(.)} else {(.)}) %>%
    list_flatten()
  
  patterns <- list(
    params             = list(
      sampleset    = paste0(params$sampleset),
      seqrun       = paste0(params$seqrun),
      samplesheet  = as.character(sample_sheets[paste0(tolower(params$seqrun))])
    )            ,
    
    global            = global            ,
    swan              = swan              ,
    micro             = micro             ,
    loris             = loris             ,
    isolates          = isolates          ,
    bats              = bats              ,
    methods_16s       = methods_16s       ,
    sample_sheets     = sample_sheets     ,
    abund_wf16s_files = abund_wf16s_files ,
    barcode_alignments= barcode_alignments
  )
  
  
  # Replace placeholders group by group
  for (group in names(patterns)) {
    placeholder_list <- patterns[[group]]
    for (name in names(placeholder_list)) {
      placeholder <- paste(group, name, sep = "\\$") # Match exact placeholder
      value <- placeholder_list[[name]]
      
      # Replace placeholders exactly and avoid breaking suffixes
      code <- gsub(placeholder, value, code, perl = TRUE)
    }
  }
  
  options$warning <- FALSE
  knitr::engine_output(options, code, out = code)
})


opts_chunk$set(message = FALSE,
               warning = FALSE,
               echo    = TRUE,
               include = TRUE,
               eval    = TRUE,
               comment = "")

