#Terminal Code as Text
open.job <- function(name, mem, hrs, cpus) {
  cat("paste to terminal:\n\nsrun --partition=guest --nodes=1 --ntasks-per-node=1", paste0(" --job-name=", name, " --mem=", mem, "GB --time=", hrs, ":00:00 --cpus-per-task=", cpus), "--pty $SHELL\n\n")
}

conda.env <- function(env) {
  cat("paste to terminal:\n\ncd", params$work_dir, "\nmodule load anaconda\nconda activate", env, "\n\n")
}

load.pkg <- function(pkg, wd) {
  cat("paste to terminal:\n\ncd", paste0(params$work_dir, wd), "\nmodule load", pkg, "\n\n")
}

sbatch <- function(script) {
  cat("paste to terminal:\n\ncd", params$work_dir, "\n\nsbatch", paste0(script, ".sh"), "\n\nsqueue -u aliciarich\n\n")
}

name.subdir <- function(name, subdir) {
  cat("paste to terminal:\n\n", paste0(name, "=", params$work_dir, subdir), "\n\n", paste0("mkdir -p", " $", name), "\n\n")
}

out.dir <- function(name) {
  cat("paste to terminal:\n\n", paste0(name, "=", params$work_dir, "/", name, "/", params$seqrun, "/", params$date), "\n\n", paste0("mkdir -p", " $", name), "\n\n")
}

read_alignment_file  <- function(file) {
  read.csv(file, stringsAsFactors = FALSE, fill = TRUE)
}

export_table <- function(df, path) {
  write.table(df,
              path,
              sep       = "\t",
              quote     = FALSE,
              row.names = FALSE)
}


export_extracts <- function(path) {
  write.table(blank.extracts,
              path,
              sep       = "\t",
              quote     = FALSE,
              row.names = FALSE)
}

export_libraries <- function(path) {
  write.table(blank.libraries,
              path,
              sep       = "\t",
              quote     = FALSE,
              row.names = FALSE)
}



read_extracts <- function(path) {
  tibble <- read.table(path, sep = "\t", header = TRUE) %>% 
    mutate(CollectionDate = ymd(CollectionDate), 
           ExtractDate    = ymd(ExtractDate))
  
  return(tibble)
}


read_libraries <- function(path) {
  tibble <- read.table(path, sep = "\t", header = TRUE) %>% 
    mutate(LibPrepDate = ymd(LibPrepDate))
  
  return(tibble)
}
