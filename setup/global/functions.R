install_missing_packages <- function(required_packages) {
  installed_packages <- rownames(installed.packages())
  missing_packages   <- setdiff(required_packages, installed_packages)
  
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  } else {
    message("All required packages are already installed.")
  }
}

fix.strings <-  function(df) {
  df <- df %>%
    mutate(across(where(is.character), ~str_remove_all(.x, fixed("'")))) %>%
    mutate(across(where(is.character), ~str_remove_all(.x, fixed("[")))) %>%
    mutate(across(where(is.character), ~str_remove_all(.x, fixed("]")))) %>%
    mutate(across(where(is.character), ~str_trim(.x, "both"))) %>%
    mutate(across(where(is.character), ~str_squish(.x)))
  return(df)
}

export.list <- function(df, path) {
  write.table(df,
              paste0(path),
              sep       = "\t",
              quote     = FALSE,
              row.names = FALSE,
              col.names = FALSE)
}


backup.df  <- function(df, directory, prefix) {
  write.table(df,
              paste0(directory, prefix, "_", Sys.Date(), ".tsv"),
              sep       = "\t",
              quote     = FALSE,
              row.names = FALSE)
}


since.start <- function(day1, date.col, units) {
  as.numeric(as.period(interval(ymd(day1), date.col), unit = units), units)
}


check.duplicates <- function(df2, df, group) {
  df2 <- df %>%
    group_by(group) %>%
    filter(n() > 1) %>%
    ungroup()
}


read.tables <- function(file) {
  data <- read.table(file, 
                     header = TRUE, 
                     sep = "\t", 
                     stringsAsFactors = FALSE) %>%
    tibble()
  return(data)
}


read.recent.version.csv <- function(directory, pattern) {
  files             <- list.files(path       = paste0(directory), 
                                  pattern    = paste0(pattern, "\\d{4}-\\d{1,2}-\\d{1,2}\\.csv"), 
                                  full.names = TRUE)
  dates             <- gsub(".*_(\\d{4}-\\d{1,2}-\\d{1,2})\\.csv", "\\1", files)
  dates             <- as.Date(dates, format = "%Y-%m-%d")
  most_recent_index <- which.max(dates)
  most_recent_file  <- files[most_recent_index]
  data              <- read.csv(most_recent_file, header = TRUE)
  
  return(data)
}


read.recent.version.tsv <- function(directory, pattern) {
  files             <- list.files(path       = paste0(directory), 
                                  pattern    = paste0(pattern, "\\d{4}-\\d{1,2}-\\d{1,2}\\.tsv"), 
                                  full.names = TRUE)
  dates             <- gsub(".*_(\\d{4}-\\d{1,2}-\\d{1,2})\\.tsv", "\\1", files)
  dates             <- as.Date(dates, format = "%Y-%m-%d")
  most_recent_index <- which.max(dates)
  most_recent_file  <- files[most_recent_index]
  data              <- read.table(most_recent_file, sep = "\t", header = T)
  
  return(data)
}

read_abundance_file  <- function(file) {
  read.table(file, 
             stringsAsFactors = FALSE, 
             header = T,
             sep = "\t")
}


read.tables <- function(file) {
  data <- read.table(file, 
                     header = TRUE, 
                     sep = "\t", 
                     stringsAsFactors = FALSE) %>%
    tibble()
  return(data)
}

read_alignment_file  <- function(file) {
  read.csv(file, stringsAsFactors = FALSE, fill = T, header = T, blank.lines.skip = T)
}


read.csvs <- function(file) {
  data <- read.table(file, 
                     header = TRUE, 
                     sep = ",", 
                     stringsAsFactors = FALSE) %>%
    tibble()
  return(data)
}

yes.no.aggregated <- function(col) {
  JS("
  function(cellInfo) {
        const values = cellInfo.subRows.map(function(row) { 
        return row['col'] === 'no' ? '\u274c No' : '\u2714\ufe0f Yes' 
        })
      
      // Count occurrences of each value
      const counts = values.reduce(function(acc, v) {
        acc[v] = (acc[v] || 0) + 1;
        return acc;
      }, {});
      
      // Format the counts as a string
      return Object.entries(counts)
        .map(([key, count]) => `${key}: ${count}`)
        .join(', ');
  }
  ")
}

relative_values <- function(col, col2) {
  col2 = col/max(col)
}

wide_subtables <- function(tbl, class) {
  tbl %>% select(identifier, subject, class) %>% 
    unnest(class) %>% 
    select(identifier, subject, nutrient, fed) %>%
    pivot_wider(id_cols     = c("identifier", "subject"), 
                names_from  = "nutrient", 
                values_from = "fed") %>%
    rename_with( ~ paste0(class, "_", .x), total)
}


subtab_div <- function(data_sub) {
  tags$div(style = "position: relative; margin-left: 350px; width: max-content; 
                                background: white; padding: 10px; border: 1px solid #ccc; 
                                box-shadow: 2px 2px 10px rgba(0,0,0,0.2);", data_sub)
}

nutrient_cell <- function(data) {
  data_bars(data, round_edges = TRUE, text_position = "center", number_fmt = label_number(scale = .001, scale_cut = cut_si("g")))
}

food_subtab <- function(index) {
  data_sub <- foods_expand[foods_expand$identifier == metadata_summary$identifier[index], ]
  food_sub <-  reactable(data_sub,
                         theme     = flatly(),
                         fullWidth = FALSE,
                         columns   = list(
                           identifier = colDef(show = FALSE),
                           food       = colDef(name = "Food"),
                           mg_fed     = colDef(name = "Amount Daily", cell = nutrient_cell(data_sub))
                         )
  )
  subtab_div(food_sub)
}


nutrient_details <- function(index, data) {
  data_sub <- data[data$identifier == metadata_summary$identifier[index], ]
  table_sub <-  reactable(
    data_sub,
    theme     = flatly(),
    fullWidth = FALSE,
    columns   = list(
      identifier = colDef(show = FALSE),
      nutrient   = colDef(name = "Nutrient"),
      fed        = colDef(name = "Amount Daily", cell = nutrient_cell(data_sub)),
      fed_unit     = colDef(show = FALSE),
      relative_fed = colDef(name = "Prop Daily", cell = merge_column(data_sub, merged_name = "relative_unit")),
      relative_unit = colDef(show = FALSE)
    )
  )
  subtab_div(table_sub)
}

expand_subtables <- function(df, col) {
  df %>% 
    select(identifier, all_of(col)) %>% 
    unnest(col) %>% 
    mutate(relative_fed = if_else(relative_unit == "proportion", round(relative_fed*100, 2), round(relative_fed, 2)),
           relative_unit = if_else(
             relative_unit == "proportion", "%",
             str_replace_all(relative_unit, "_", "/")),
           nutrient = fct_recode(nutrient, !!!rename_nutrients))
}

