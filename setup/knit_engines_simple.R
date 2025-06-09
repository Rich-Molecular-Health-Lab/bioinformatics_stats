knitr::knit_engines$set(terminal = function(options) {
  code <- paste(options$code, collapse = "\n")
  
  options$warning <- FALSE
  knitr::engine_output(options, code, out = code)
})


knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      echo    = TRUE,
                      include = TRUE,
                      eval    = TRUE,
                      comment = "")

