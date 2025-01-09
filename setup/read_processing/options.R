# Set global options for all chunks
opts_chunk$set(message = FALSE,
               warning = FALSE,
               echo    = TRUE,
               include = TRUE,
               eval    = TRUE)

knitr::knit_engines$set(terminal = function(options) {
  code <- paste(options$code, collapse = "\n")
  for (param in names(params)) {
    param_placeholder <- paste0("params\\$", param)
    param_value <- params[[param]]
    code <- gsub(param_placeholder, param_value, code)
  }
  knitr::engine_output(options, code, out = code)
})

knitr::knit_engines$set(swan = function(options) {
  code <- paste(options$code, collapse = "\n")
  for (param in names(params)) {
    param_placeholder <- paste0("params\\$", param)
    param_value <- params[[param]]
    code <- gsub(param_placeholder, param_value, code)
  }
  knitr::engine_output(options, code, out = code)
})


