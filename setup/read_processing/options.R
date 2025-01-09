xfun::pkg_load2(c("htmltools", "mime"))

# Set global options for all chunks
opts_chunk$set(message = FALSE,
               warning = FALSE,
               echo    = TRUE,
               include = TRUE,
               eval    = TRUE)


# Custom engine for swan that substitutes variables and prints the code
knit_engines$set(swan = function(options) {
  code <- paste(options$code, collapse = "\n")
  
  # Substitute all params variables with their values
  for (param in names(params)) {
    param_placeholder <- paste0("\\$\\{params\\$", param, "\\}")
    param_value <- params[[param]]
    code <- gsub(param_placeholder, param_value, code)
  }
  
  # Print the code as a formatted code block
  output <- paste0("paste to terminal:\n\n", code, "\n\n")
  
  # Return the output to be displayed in the knitted document
  knitr::knit_print(output)
})
