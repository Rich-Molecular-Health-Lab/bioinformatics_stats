# Set global options for all chunks
opts_chunk$set(message = FALSE,
               warning = FALSE,
               echo    = FALSE,
               include = TRUE,
               eval    = TRUE)

knitr::knit_engines$set(terminal = function(options) {
  code <- paste(options$code, collapse = "\n")
  for (param in names(params)) {
    param_placeholder <- paste0("params\\$", param)
    param_value <- params[[param]]
    code <- gsub(param_placeholder, param_value, code)
  }
  for (method in names(methods_16s)) {
    method_placeholder <- paste0("methods_16s\\$", method)
    method_value <-      methods_16s[[method]]
    code <- gsub(method_placeholder, method_value, code)
  }
  knitr::engine_output(options, code, out = code)
})


