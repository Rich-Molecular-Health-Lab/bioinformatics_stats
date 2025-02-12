sort.taxa <- function(df) {
  df <- df %>%
    arrange(superkingdom,
            phylum,
            class,
            order,
            family,
            genus,
            species)
  return(df)
}

group.taxonomy <- function(df) {
  group_by(.data = df,
           superkingdom,
           phylum,
           class,
           order,
           family,
           genus,
           species)
}



#Lists for Environmental Variables & Dataframe Columns
create_variable_list <- function(subject_prefixes, variables, icons) {
  variable_list <- list()
  
  for (subject in names(subject_prefixes)) {
    subj_prefix  <- subject_prefixes[[subject]]
    subject_list <- list()
    
    for (var_name in names(variables)) {
      col_suffix <- variables[[var_name]]
      icon_name  <-     icons[[var_name]]
      
      icon_col   <- paste0("icon_"       , subj_prefix, "_", col_suffix)
      corr_col   <- paste0("Correlation_", subj_prefix, "_", col_suffix)
      sig_col    <- paste0("sig_"        , subj_prefix, "_", col_suffix)
      
      subject_list[[paste0(var_name, "_", subj_prefix)]] <- list(
        "columns" = list("icon"        = icon_col  ,
                         "correlation" = corr_col  ,
                         "sig"         = sig_col   ),
        "col_lab" = fontawesome::fa(icon_name))
    }
    
    variable_list[[subject]] <- subject_list
  }
  
  return(variable_list)
}


#Formatting Variables

subject_prefixes <- list(
  "Both"   = "B",
  "Culi"   = "C",
  "Warble" = "W"
)

variables <- list(
  "Subject"  = "subject",
  "Week"     = "study_week",
  "Diet"     = "diet",
  "Steroid"  = "steroid_dose",
  "Location" = "location",
  "Bristol"  = "bristol_mean",
  "Estrus"   = "estrus",
  "Pregnant" = "pregnant"
)

icons <- list(
  "Subject"  = "user",
  "Week"     = "calendar-alt",
  "Diet"     = "apple-alt",
  "Steroid"  = "prescription-bottle",
  "Location" = "home",
  "Bristol"  = "poop",
  "Estrus"   = "heart",
  "Pregnant" = "person-pregnant"
)

variables <- create_variable_list(subject_prefixes, variables, icons)

variables.append <- function(df, variable_list) {
  for (subject_name in names(variable_list)) {
    subject_info <- variable_list[[subject_name]]
    valid_vars   <- list()
    
    for (var_name in names(subject_info)) {
      var_info <- subject_info[[var_name]]
      columns  <- var_info$columns
      
      if (all(sapply(columns, function(x) x %in% colnames(df)))) {
        valid_vars[[var_name]] <- var_info
      } else {
        missing_cols <- names(columns)[!sapply(columns, function(x) x %in% colnames(df))]
      }
    }
    
    variable_list[[subject_name]] <- valid_vars
  }
  
  variable_list <- variable_list[lengths(variable_list) > 0]
  
  return(variable_list)
}

env.cols.culi <- c(
  "study_week",
  "diet",
  "steroid_dose",
  "location",
  "bristol_mean")

env.cols.warb <- c(
  "study_week",
  "location",
  "estrus",
  "pregnant")

env.cols.main <- c(
  "subject",
  "study_week",
  "diet",
  "steroid_dose",
  "location")



#Loading Microeco Datasets & Recalculating Abundances
read.microtable <- function(format, level, dataset){
  if (format == "tax"){
    dataset_path <- paste0(path$microeco$dataset, "$", prefix, "")
    phylo_tree  <-  read.tree(paste0(path$microeco$dataset, "$", prefix, "_dataset_", suffix, "/phylo_tree.tre"   )  )
    rep_fasta   <- read.fasta(paste0(params$local, "dataframes/", prefix, "_dataset_", suffix, "/rep_fasta.fasta"  )  )
    sample_tab  <- read.table(paste0(params$local, "dataframes/", prefix, "_dataset_", suffix, "/sample_table.tsv" )  ,  sep = "\t", header = T, row.names = 1)
    tax_table   <- read.table(paste0(params$local, "dataframes/", prefix, "_dataset_", suffix, "/tax_table.tsv"    )  ,  sep = "\t", header = T, row.names = 1)
    otu_table   <- read.table(paste0(params$local, "dataframes/", prefix, "_dataset_", suffix, "/feature_table.tsv")  ,  sep = "\t", header = T, row.names = 1)
    
    dataset  <- microtable$new(
      sample_table = sample_tab,
      otu_table    = otu_table,
      tax_table    = tax_table,
      phylo_tree   = phylo_tree,
      rep_fasta    = rep_fasta,
      auto_tidy    = T)
  }
  
  if (format == "function"){
    
    sample_tab  <- read.table(paste0(params$local, "dataframes/", prefix, "_dataset_", suffix, "/sample_table.tsv" )  ,  sep = "\t", header = T, row.names = 1)
    tax_table   <- read.table(paste0(params$local, "dataframes/", prefix, "_dataset_", suffix, "/tax_table.tsv"    )  ,  sep = "\t", header = T, row.names = 1)
    otu_table   <- read.table(paste0(params$local, "dataframes/", prefix, "_dataset_", suffix, "/feature_table.tsv")  ,  sep = "\t", header = T, row.names = 1)
    
    dataset  <- microtable$new(
      sample_table = sample_tab,
      otu_table    = otu_table,
      tax_table    = tax_table,
      auto_tidy    = T)
  }
  dataset$cal_abund()
  
  return(dataset)
}

#Calculating Difference in Abundance between Subjects
diff.abund  <- function(dataset, taxrank) {
  tmp.data  <- trans_diff$new(
    dataset      = dataset,
    method       = "wilcox",
    group        = "subject",
    taxa_level   = taxrank)
  
  abund.df       <- as.data.frame(tmp.data$res_abund)  %>%
    pivot_wider(id_cols     = "Taxa",
                names_from  = "Group",
                values_from = c("N", "Mean")) %>%
    mutate(Mean_B = (Mean_C + Mean_W)/2)
  
  diff.df        <- as.data.frame(tmp.data$res_diff) %>%
    select(Taxa, Enriched = Group, P.adj, Significance)
  
  df             <- diff.df %>% left_join(abund.df)  %>%
    mutate(Taxa = str_remove_all(Taxa, "\\w_")) %>%
    mutate(Taxa = str_remove_all(Taxa, fixed("_"))) %>%
    separate_wider_delim(cols    = "Taxa", delim = "|", 
                         names   = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                         too_few = "align_start") %>%
    filter(!is.na(Significance)) %>%
    mutate(Kingdom          = replace_na(Kingdom, ""),
           Phylum           = replace_na(Phylum,  ""),
           Class            = replace_na(Class,   ""),
           Order            = replace_na(Order,   ""),
           Family           = replace_na(Family,  ""),
           Genus            = replace_na(Genus,   ""),
           Species          = replace_na(Species, ""),
           Enriched         = if_else(P.adj > 0.05, "ns", Enriched)) %>%
    mutate(Enriched_icon = case_when(
      Enriched == "ns"                                 ~ "minus"                         ,
      Enriched == "W" & P.adj >  0.01                  ~ "w,caret-up"                    ,
      Enriched == "W" & P.adj <= 0.01 & P.adj > 0.001  ~ "w,caret-up,caret-up"           ,
      Enriched == "W" & P.adj <= 0.001                 ~ "w,caret-up,caret-up,caret-up"  ,
      Enriched == "C" & P.adj >  0.01                  ~ "c,caret-up"                    ,
      Enriched == "C" & P.adj <= 0.01 & P.adj > 0.001  ~ "c,caret-up,caret-up"           ,
      Enriched == "C" & P.adj <= 0.001                 ~ "c,caret-up,caret-up,caret-up"  )) %>%
    select(AdjP_diff        = P.adj,
           Abundance_W      = Mean_W,
           Abundance_C      = Mean_C,
           Abundance_B      = Mean_B,
           Kingdom    ,
           Phylum     ,
           Class      ,
           Order      ,
           Family     ,
           Genus      ,
           Species    ,
           starts_with("Enriched"))
  
  return(df)
}

diff.abund.function  <- function(dataset, taxrank) {
  tmp.data  <- trans_diff$new(
    dataset      = dataset,
    method       = "wilcox",
    group        = "subject",
    taxa_level   = taxrank)
  
  abund.df       <- as.data.frame(tmp.data$res_abund)  %>%
    pivot_wider(id_cols     = "Taxa",
                names_from  = "Group",
                values_from = c("N", "Mean")) %>%
    mutate(Mean_B = (Mean_C + Mean_W)/2)
  
  diff.df        <- as.data.frame(tmp.data$res_diff) %>%
    select(Taxa, Enriched = Group, P.adj, Significance)
  
  df             <- diff.df %>% left_join(abund.df)  %>%
    filter(!is.na(Significance)) %>% 
    mutate(Enriched         = if_else(P.adj > 0.05, "ns", Enriched)) %>%
    mutate(Enriched_icon = case_when(
      Enriched == "ns"                                 ~ "minus"                         ,
      Enriched == "W" & P.adj >  0.01                  ~ "w,caret-up"                    ,
      Enriched == "W" & P.adj <= 0.01 & P.adj > 0.001  ~ "w,caret-up,caret-up"           ,
      Enriched == "W" & P.adj <= 0.001                 ~ "w,caret-up,caret-up,caret-up"  ,
      Enriched == "C" & P.adj >  0.01                  ~ "c,caret-up"                    ,
      Enriched == "C" & P.adj <= 0.01 & P.adj > 0.001  ~ "c,caret-up,caret-up"           ,
      Enriched == "C" & P.adj <= 0.001                 ~ "c,caret-up,caret-up,caret-up"  )) %>%
    select(AdjP_diff        = P.adj,
           Abundance_W      = Mean_W,
           Abundance_C      = Mean_C,
           Abundance_B      = Mean_B,
           Taxa,
           starts_with("Enriched"))
  
  
  return(df)
}


#Calculating Spearman Correlations
correlation.df <- function(dataset, env.cols, taxrank, filter_thres) {
  
  tmp.env <- trans_env$new(dataset, env_cols = env.cols, character2numeric = T)
  
  tmp.env$cal_cor(use_data = taxrank, cor_method = "spearman", p_adjust_type = "Env",  filter_thres = filter_thres)
  
  df <- as.data.frame(tmp.env$res_cor)
  
  return(df)
}

#Cleaning & Formatting Dataframes
format.function.df <- function(df, level){
  df <- df %>%
    mutate(icon     = case_when(
      AdjPvalue  > 0.05     ~ "square", 
      Env == "study_week"   ~ "calendar-alt",
      Env == "diet"         ~ "apple-alt",
      Env == "steroid_dose" ~ "prescription-bottle",
      Env == "location"     ~ "home",
      Env == "bristol_mean" ~ "poop",
      Env == "estrus"       ~ "heart",
      Env == "pregnant"     ~ "person-pregnant",  
      Env == "subject"      ~ "user",   .default = "square"),
      sig    = if_else(AdjPvalue <= 0.05, "yes", "no")) %>%
    pivot_wider(id_cols     = "Taxa",
                names_from  = c("subject", "Env"),
                values_from = c("Correlation", "sig", "icon"),
                names_sep   = "_")
  
}

format.taxa.df <- function(df, level){
  df <- df %>%
    mutate(icon     = case_when(
      AdjPvalue  > 0.05     ~ "square", 
      Env == "study_week"   ~ "calendar-alt",
      Env == "diet"         ~ "apple-alt",
      Env == "steroid_dose" ~ "prescription-bottle",
      Env == "location"     ~ "home",
      Env == "bristol_mean" ~ "poop",
      Env == "estrus"       ~ "heart", 
      Env == "subject"      ~ "user",  
      Env == "pregnant"     ~ "person-pregnant",  
      .default = "square"),
      sig    = if_else(AdjPvalue <= 0.05, "yes", "no")) %>%
    pivot_wider(id_cols     = "Taxa",
                names_from  = c("subject", "Env"),
                values_from = c("Correlation", "sig", "icon"),
                names_sep   = "_") 
  if(level == "spe"){
    df <- df %>%
      separate_wider_delim(Taxa, "|", names = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) %>%
      mutate(across(all_of(c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")), 
                    ~ str_sub(.x, 4L, -1L))) %>%
      left_join(select(spe.abund.df, c("Species", starts_with("Abund"), starts_with("Enriched")))) %>% distinct() %>%
      mutate(level        = 6,
             tax          = "Species",
             lowest_level = as.character(Species))
  }
  if(level == "gen"){
    df <- df %>%
      separate_wider_delim(Taxa, "|", names = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")) %>%
      mutate(across(all_of(c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus")), 
                    ~ str_sub(.x, 4L, -1L))) %>%
      left_join(select(gen.abund.df, c("Genus", "Species", starts_with("Abund"), starts_with("Enriched")))) %>% distinct() %>%
      mutate(level        = 5,
             tax          = "Genus",
             lowest_level = as.character(Genus))
  }
  if(level == "fam"){
    df <- df %>%
      separate_wider_delim(Taxa, "|", names = c("Kingdom", "Phylum", "Class", "Order", "Family")) %>%
      mutate(across(all_of(c("Kingdom", "Phylum", "Class", "Order", "Family")), 
                    ~ str_sub(.x, 4L, -1L))) %>%
      left_join(select(fam.abund.df, c("Family", "Genus", "Species", starts_with("Abund"), starts_with("Enriched")))) %>% distinct() %>%
      mutate(level        = 4,
             tax          = "Family",
             lowest_level = as.character(Family))
  }
  
  if(level == "ord"){
    df <- df %>%
      separate_wider_delim(Taxa, "|", names = c("Kingdom", "Phylum", "Class", "Order")) %>%
      mutate(across(all_of(c("Kingdom", "Phylum", "Class", "Order")), 
                    ~ str_sub(.x, 4L, -1L))) %>%
      left_join(select(ord.abund.df, c("Order", "Family", "Genus", "Species", starts_with("Abund"), starts_with("Enriched")))) %>% distinct() %>%
      mutate(level        = 3,
             tax          = "Order",
             lowest_level = as.character(Order))
  }
  
  if(level == "cla"){
    df <- df %>%
      separate_wider_delim(Taxa, "|", names = c("Kingdom", "Phylum", "Class")) %>%
      mutate(across(all_of(c("Kingdom", "Phylum", "Class")), 
                    ~ str_sub(.x, 4L, -1L))) %>%
      left_join(select(cla.abund.df, c("Class", "Order", "Family", "Genus", "Species", starts_with("Abund"), starts_with("Enriched")))) %>% distinct() %>%
      mutate(level        = 2,
             tax          = "Class",
             lowest_level = as.character(Class))
  }
  
  if(level == "phy"){
    df <- df %>%
      separate_wider_delim(Taxa, "|", names = c("Kingdom", "Phylum")) %>%
      mutate(across(all_of(c("Kingdom", "Phylum")), 
                    ~ str_sub(.x, 4L, -1L))) %>%
      left_join(select(phy.abund.df, -Kingdom)) %>%
      distinct() %>%
      mutate(level        = 1,
             tax          = "Phylum",
             lowest_level = as.character(Phylum))
  }
  df <- df %>%
    mutate(
      Species      = factor(Species,levels = unique(spe.abund.df$Species), ordered = T ),
      Genus        = factor(Genus,  levels = unique(spe.abund.df$Genus  ), ordered = T ),
      Family       = factor(Family, levels = unique(spe.abund.df$Family ), ordered = T ),
      Order        = factor(Order,  levels = unique(spe.abund.df$Order  ), ordered = T ),
      Class        = factor(Class,  levels = unique(spe.abund.df$Class  ), ordered = T ),
      Phylum       = factor(Phylum, levels = unique(spe.abund.df$Phylum ), ordered = T ),
      Kingdom      = factor(Kingdom,levels = unique(spe.abund.df$Kingdom), ordered = T )) %>%
    
    arrange(Kingdom, Phylum, Class, Order, Family, Genus, Species)
  
  return(df)
  
}

clean.filter.table <- function(df){
  df <- df %>%
    filter_at(vars(starts_with("sig")), any_vars(. == "yes"))              %>%
    mutate(across(starts_with("sig"),         ~ replace_na(.x, "no")))     %>%
    mutate(across(starts_with("Correlation"), ~ replace_na(.x, 0)))        %>%
    mutate(across(where(is.numeric),          ~ replace_na(.x, 0)))        %>%
    mutate(across(starts_with("icon"),        ~ replace_na(.x, "square"))) %>%
    mutate(Enriched = str_replace_all(Enriched, "C", "Culi"),
           Enriched = str_replace_all(Enriched, "W", "Warble")) %>%
    mutate(Enriched = factor(Enriched, levels = c("Culi", "Warble", "ns"), ordered = T))
  return(df)
}

micro.visual <- function(df){
  df <- df %>%
    arrange(Phylum, Class, Order, Family, Genus, Species, level) %>%
    mutate(Phylum  = if_else(tax != "Phylum",                                                      str_glue("P ", "{Phylum}"  ), ""),
           Class   = if_else(tax == "Order" | tax == "Family" | tax == "Genus" | tax == "Species", str_glue("C ", "{Class}"   ), ""),
           Order   = if_else(                 tax == "Family" | tax == "Genus" | tax == "Species", str_glue("O ", "{Order}"   ), ""),
           Family  = if_else(                                   tax == "Genus" | tax == "Species", str_glue("F ", "{Family}"  ), ""),
           Genus   = if_else(                                                    tax == "Species", str_glue("G ", "{Genus}"   ), "")) %>%
    mutate(row_shade = case_when(
      tax == "Species" ~ "#FAFAFAFF",
      tax == "Genus"   ~ "#F5F5F5FF",
      tax == "Family"  ~ "#EEEEEEFF",
      tax == "Order"   ~ "#E0E0E0FF",
      tax == "Class"   ~ "#BDBDBDFF",
      tax == "Phylum"  ~ "#9E9E9EFF")) %>%
    select(Phylum, Class, Order, Family, Genus,
           tax,
           row_shade,
           lowest_level,
           starts_with("Abundance"),
           starts_with("Enrich"),
           starts_with("Correlation_"),
           starts_with("icon_"),
           starts_with("sig_")
    )
  
  return(df)
}



#Formatting Graphical Tables with GT
taxonomic.correlation.table <- function(gt_table, df, variable_list) {
  
  for (subject_name in names(variable_list)) {
    
    subject_info <- variable_list[[subject_name]]
    
    for (var_name in names(subject_info)){
      var_info <- subject_info[[var_name]]
      col_list  <-   var_info[["columns"]]
      
      required_cols <- c(
        col_list[["correlation"]], 
        col_list[["icon"]], 
        col_list[["sig"]])
      
      missing_cols  <- required_cols[!required_cols %in% colnames(df)]
      
      if (length(missing_cols) > 0) {
        cat("Missing columns for variable", var_name, "in Main level:", missing_cols, "\n")
        next
      }
      
      sig_column <- col_list[["sig"]]
      if (length(sig_column) == 0 || !(sig_column %in% colnames(df))) {
        cat("Column", sig_column, "is missing in dataframe for variable", var_name, "\n")
        next
      }
      
      
      sig_values   <-         df[[sig_column]]
      icon_col     <-   col_list[["icon"]]
      label_col    <-   var_info[["col_lab"]]
      
      
      gt_table <- gt_table %>%
        
        cols_label(!!icon_col := label_col) %>%
        
        tab_spanner(var_name, columns = icon_col) %>%
        
        tab_style(style = list( 
          cell_text(size     = "small",
                    weight   = "bold",
                    stretch  = "ultra-condensed",
                    v_align  = "bottom",
                    align    = "center")),
          locations = cells_column_spanners(spanners = c(var_name)))
      
      
      if ("yes" %in% sig_values) {
        gt_table <- gt_table %>%
          data_color(
            columns        = vars(!!sym(col_list[["correlation"]])),
            target_columns = vars(!!sym(col_list[["icon"]])),
            rows           = sig_values == "yes",
            direction      = "column",
            palette        = "futurevisions::kepler186",
            domain         = c(-1, 1),
            reverse        = T,
            alpha          = 0.9
          )
      }
      
      
      if ("no" %in% sig_values) {
        gt_table <- gt_table %>%
          data_color(
            columns        = vars(!!sym(col_list[["correlation"]])),
            target_columns = vars(!!sym(col_list[["icon"]])),
            rows           = sig_values == "no",
            direction      = "column",
            palette        = switch(subject_name,
                                    "Culi"   = "#44486DFF",
                                    "Warble" = "#8B4F82FF",
                                    "Both"   = "futurevisions::kepler186"),
            apply_to       = "fill",
            alpha          = 0.2)  %>% 
          data_color(
            columns        = vars(!!sym(col_list[["correlation"]])),
            target_columns = vars(!!sym(col_list[["icon"]])),
            rows           = sig_values == "no"         ,
            direction      = "column"                   ,
            palette        = "white"                    , 
            apply_to       = "text"                     ,
            alpha          =  0.2                       )
      }
      gt_table <- gt_table %>%
        
        tab_style(style = list(
          cell_fill(color   = switch(subject_name,
                                     "Both"   = "white",
                                     "Culi"   = "#44486DFF",
                                     "Warble" = "#8B4F82FF")),
          cell_borders(sides   = c("left", "right", "top", "bottom"),
                       weight  = px(0)),
          cell_text(color   = switch(subject_name,
                                     "Both"   = "black",
                                     "Culi"   = "white",
                                     "Warble" = "white")),
          stretch = "ultra-condensed",
          weight  = "bold"),
          locations = cells_column_labels(columns = icon_col)) %>%
        
        tab_style(style = list( 
          cell_borders(color    = switch(subject_name,
                                         "Both"   = "white",
                                         "Culi"   = "#44486DFF",
                                         "Warble" = "#8B4F82FF")),
          cell_fill(color    = switch(subject_name,
                                      "Both"   = "white",
                                      "Culi"   = "#44486DFF",
                                      "Warble" = "#8B4F82FF")),
          cell_text(size     = "small",
                    weight   = "bold",
                    color    = switch(subject_name,
                                      "Both"   = "black",
                                      "Culi"   = "white",
                                      "Warble" = "white")),
          v_align  = "bottom",
          stretch  = "ultra-condensed",
          align    = "center"),
          locations = cells_column_spanners(spanners = c(var_name))) %>%
        
        text_replace(locations   = cells_column_spanners(spanners = c(var_name)),
                     pattern     = "_\\w",
                     replacement = "") 
      
    }
  }
  gt_table <- gt_table %>%
    opt_table_lines(extent = "none") %>%
    cols_label(tax = "Level", lowest_level = "Microbial Taxon") %>%
    
    fmt_icon(columns = c(starts_with("icon"), "Enriched_icon")) %>%
    
    data_color(columns        = "Enriched",
               rows           = Enriched != "ns",
               target_columns = "Enriched_icon",
               palette        = c("#8B4F82FF",
                                  "#44486DFF"),
               levels         = c("Warble",
                                  "Culi")) %>%
    
    tab_style(style = list(
      cell_fill(color = from_column("row_shade")),
      cell_text(color = "#616161FF",
                align = "center")),
      locations = cells_body(columns = Enriched_icon,
                             rows    = Enriched == "ns")) %>%
    
    cols_align("center", columns = c(starts_with("icon"))) %>%
    
    cols_label(Enriched_icon = md("Relative<br>Enrichmt")) %>%
    
    tab_style(style = list(
      cell_text(size     = "xx-small",
                weight   = "bold",
                v_align  = "bottom",
                stretch  = "ultra-condensed",
                align    = "center")),
      locations = cells_column_labels(columns = Enriched_icon)) %>%
    
    tab_style(style = list(
      cell_text(weight = "bold",
                align  = "center")),
      locations = cells_column_labels(columns = starts_with("icon"))) %>%
    
    tab_style(style = list(
      cell_fill(color   = from_column("row_shade")),
      cell_text(align   = "left",
                weight  = "bold",
                stretch = "ultra-condensed",
                style   = "italic")),
      locations = cells_body(columns = lowest_level)) %>%
    
    tab_style(style = list(
      cell_text(align  = "center", 
                weight = "bold", 
                size   = "large")),
      locations = cells_body(columns = starts_with("icon"))) %>%
    
    tab_style(style = list(
      cell_text(align  = "left", 
                weight = "bold", 
                size   = "x-small")),
      locations = cells_body(columns = Enriched_icon)) %>%
    
    tab_style(
      style     = list(
        cell_fill(color = from_column("row_shade")), 
        cell_text(align     = "left",
                  size      = "x-small",
                  stretch   = "ultra-expanded",
                  style     = "oblique",
                  transform = "uppercase")),
      locations = cells_body(columns = tax))  %>%
    
    tab_style(style     = list(
      cell_borders(color     = "#616161FF",
                   sides     = c("top", "bottom")),
      cell_text(size      = "x-small",
                stretch   = "ultra-expanded",
                style     = "oblique",
                transform = "uppercase")),
      locations = cells_row_groups())  %>%
    
    tab_style(style = list(
      cell_text(weight    = "bold",
                stretch     = "expanded",
                align       = "left")),
      locations = cells_column_labels(columns = c("lowest_level"))) %>%
    
    tab_style(style = list(
      cell_text(size        = "small",
                style       = "oblique",
                transform   = "uppercase",
                weight      = "bold",
                align       = "left")),
      locations = cells_column_labels(columns = c("tax"))) %>%
    
    
    cols_hide(columns = c(starts_with("Correlation"), starts_with("sig"), "row_shade", "Enriched")) %>%
    
    cols_move_to_end(columns = starts_with("icon_")) %>%
    
    tab_style(style     =  list(
      cell_borders(color = "#616161FF", sides = "bottom"),
      cell_text(size    = "large",
                stretch = "ultra-condensed",
                weight  = "bold")),
      locations =  cells_column_labels(columns = starts_with("icon"))) %>%
    
    
    tab_header(title    = table.title,
               subtitle = table.subtitle) %>%
    
    tab_style(
      style     = list(
        cell_fill(color    = "#E0E0E0FF"),
        cell_text(size     = "large",
                  align    = "left",
                  weight   = "bold")),
      locations = cells_title(groups = "title")) %>%
    tab_style(
      style     = list(
        cell_fill(color    = "#E0E0E0FF"),
        cell_text(size     = "small",
                  align    = "left",
                  style    = "italic",
                  stretch  = "condensed")),
      locations = cells_title(groups = "subtitle")) %>%
    
    cols_width(tax                    ~ px(50),
               lowest_level           ~ px(275),
               Enriched_icon          ~ px(50),
               c(starts_with("icon")) ~ px(75)) %>%
    
    tab_options(table.width = pct(100)) %>%
    
    
    tab_footnote(
      "Enrichment implies significantly greater relative abundance relative to the other subject's abundance (Wilcoxon Rank Sum Test, alpha = 0.05). Letter indicates subject for which the taxon was significantly enriched, and carrots indicate significance level (^ = 0.05, ^^ = 0.01, ^^^ = 0.001).",
      locations = cells_column_labels(columns = Enriched_icon)) %>%
    
    tab_footnote(
      "Color scaled by spearman correlation coefficient with darker blues indicating greater negative correlation and darker reds indicating greater positive correlation.",
      locations = cells_title(groups = "subtitle")) %>%
    tab_footnote(
      md("Icons for variables only displayed if the adjusted p-value for that row/column was significant *(alpha = 0.05)*.<br>*Squares and transparent fill represent non-significant values. P-values were adjusted independently by variable using fdr correction.*"),
      locations = cells_title(groups = "subtitle"))
  
  return(gt_table)
}

function.correlation.table <- function(gt_table, df, variable_list) {
  
  for (subject_name in names(variable_list)) {
    
    subject_info <- variable_list[[subject_name]]
    
    for (var_name in names(subject_info)){
      var_info <- subject_info[[var_name]]
      col_list  <-   var_info[["columns"]]
      
      required_cols <- c(
        col_list[["correlation"]], 
        col_list[["icon"]], 
        col_list[["sig"]])
      
      missing_cols  <- required_cols[!required_cols %in% colnames(df)]
      
      if (length(missing_cols) > 0) {
        cat("Missing columns for variable", var_name, "in Main level:", missing_cols, "\n")
        next
      }
      
      sig_column <- col_list[["sig"]]
      if (length(sig_column) == 0 || !(sig_column %in% colnames(df))) {
        cat("Column", sig_column, "is missing in dataframe for variable", var_name, "\n")
        next
      }
      
      
      sig_values   <-         df[[sig_column]]
      icon_col     <-   col_list[["icon"]]
      label_col    <-   var_info[["col_lab"]]
      
      
      gt_table <- gt_table %>%
        
        cols_label(!!icon_col := label_col) %>%
        
        tab_spanner(var_name, columns = icon_col) %>%
        
        tab_style(style = list( 
          cell_text(size     = "small",
                    weight   = "bold",
                    stretch  = "ultra-condensed",
                    v_align  = "bottom",
                    align    = "center")),
          locations = cells_column_spanners(spanners = c(var_name)))
      
      if ("yes" %in% sig_values) {
        gt_table <- gt_table %>%
          data_color(
            columns        = vars(!!sym(col_list[["correlation"]])),
            target_columns = vars(!!sym(col_list[["icon"]])),
            rows           = sig_values == "yes",
            direction      = "column",
            palette        = "futurevisions::kepler186",
            domain         = c(-1, 1),
            reverse        = T,
            alpha          = 0.9
          )
      }
      
      if ("no" %in% sig_values) {
        gt_table <- gt_table %>%
          data_color(
            columns        = vars(!!sym(col_list[["correlation"]])),
            target_columns = vars(!!sym(col_list[["icon"]])),
            rows           = sig_values == "no",
            direction      = "column",
            palette        = switch(subject_name,
                                    "Culi"   = "#44486DFF",
                                    "Warble" = "#8B4F82FF",
                                    "Both"   = "futurevisions::kepler186"),
            apply_to       = "fill",
            alpha          = 0.2)  %>% 
          data_color(
            columns        = vars(!!sym(col_list[["correlation"]])),
            target_columns = vars(!!sym(col_list[["icon"]])),
            rows           = sig_values == "no"         ,
            direction      = "column"                   ,
            palette        = "white"                    , 
            apply_to       = "text"                     ,
            alpha          =  0.2                       )
      }
      gt_table <- gt_table %>%
        
        tab_style(style =  list(
          cell_fill(color = switch(subject_name,
                                   "Both"   = "#E0E0E0FF",
                                   "Culi"   = "#44486DFF",
                                   "Warble" = "#8B4F82FF")),
          cell_borders(color = "#616161FF", sides = "bottom"),
          cell_text(align    = "center",
                    size     = "large",
                    stretch  = "ultra-condensed",
                    weight   = "bold",
                    color    = switch(subject_name,
                                      "Both"   = "black",
                                      "Culi"   = "white",
                                      "Warble" = "white"))),
          locations =  cells_column_labels(columns = icon_col)) %>%
        
        
        tab_style(style = list( 
          cell_borders(color    = switch(subject_name,
                                         "Both"   = "#E0E0E0FF",
                                         "Culi"   = "#44486DFF",
                                         "Warble" = "#8B4F82FF")),
          cell_fill(color    = switch(subject_name,
                                      "Both"   = "#E0E0E0FF",
                                      "Culi"   = "#44486DFF",
                                      "Warble" = "#8B4F82FF")),
          cell_text(size     = "small",
                    weight   = "bold",
                    color    = switch(subject_name,
                                      "Both"   = "black",
                                      "Culi"   = "white",
                                      "Warble" = "white")),
          v_align  = "bottom",
          stretch  = "ultra-condensed",
          align    = "center"),
          locations = cells_column_spanners(spanners = c(var_name))) %>%
        
        text_replace(locations   = cells_column_spanners(spanners = c(var_name)),
                     pattern     = "_\\w",
                     replacement = "")
      
    }
  }
  
  gt_table <- gt_table %>%
    
    opt_table_lines(extent = "none") %>%
    
    fmt_icon(columns = c(starts_with("icon"), "Enriched_icon"))
  
  if (any(df$Enriched != "ns")) {
    
    gt_table <- gt_table %>%
      
      data_color(columns        = "Enriched",
                 rows           = Enriched != "ns",
                 target_columns = "Enriched_icon",
                 palette        = c("#8B4F82FF",
                                    "#44486DFF"),
                 levels         = c("Warble",
                                    "Culi")) %>%
      
      tab_style(style = 
                  cell_text(size  = "x-small"),
                locations = cells_body(columns = Enriched_icon,
                                       rows    = Enriched != "ns"))
  }
  gt_table <- gt_table %>%
    
    cols_align("center", columns = c(starts_with("icon"))) %>%
    
    cols_label(Enriched_icon = md("Relative<br>Enrichmt")) %>%
    
    tab_style(style = list(
      cell_fill(color = "#E0E0E0FF",
                alpha = 0.5),
      cell_text(size  = "x-small",
                align = "center",
                color = "white")),
      locations = cells_body(columns = Enriched_icon,
                             rows    = Enriched == "ns")) %>%
    
    tab_style(style = list(
      cell_fill("#E0E0E0FF"),
      cell_text(size     = "xx-small",
                weight   = "bold",
                v_align  = "middle",
                stretch  = "ultra-condensed",
                align    = "center")),
      locations = cells_column_labels(columns = Enriched_icon)) %>%
    
    tab_style(style = list(
      cell_text(align  = "center", 
                weight = "bold", 
                size   = "large")),
      locations = cells_body(columns = starts_with("icon"))) %>%
    
    tab_style(style     = list(
      cell_fill(color     = "#616161FF"),
      cell_borders(color     = "#616161FF",
                   sides     = c("top", "bottom")),
      cell_text(color     = "white",
                weight    = "bold",
                stretch   = "ultra-expanded",
                style     = "oblique",
                transform = "uppercase")),
      locations = cells_row_groups())  %>%
    
    cols_hide(columns = c(starts_with("Correlation"), starts_with("sig"), "Enriched")) %>%
    
    cols_move_to_end(columns = starts_with("icon_")) %>%
    
    tab_header(title    = table.title,
               subtitle = table.subtitle) %>%
    
    tab_style(
      style     = list(
        cell_fill(color    = "#E0E0E0FF"),
        cell_text(size     = "large",
                  align    = "left",
                  weight   = "bold")),
      locations = cells_title(groups = "title")) %>%
    tab_style(
      style     = list(
        cell_fill(color    = "#E0E0E0FF"),
        cell_text(size     = "small",
                  align    = "left",
                  style    = "italic",
                  stretch  = "condensed")),
      locations = cells_title(groups = "subtitle")) %>%
    
    cols_width(stub                      ~ px(200),
               Enriched_icon             ~ px(50),
               c(starts_with("icon"))    ~ px(75)) %>%
    
    tab_options(table.width = pct(100)) %>%
    
    tab_footnote(
      "Enrichment implies significantly greater relative abundance relative to the other subject's abundance (Wilcoxon Rank Sum Test, alpha = 0.05). Letter indicates subject for which the taxon was significantly enriched, and carrots indicate significance level (^ = 0.05, ^^ = 0.01, ^^^ = 0.001).",
      locations = cells_column_labels(columns = Enriched_icon)) %>%
    
    tab_footnote(
      "Color scaled by spearman correlation coefficient with darker blues indicating greater negative correlation and darker reds indicating greater positive correlation.",
      locations = cells_title(groups = "subtitle")) %>%
    tab_footnote(
      md("Icons for variables only displayed if the adjusted p-value for that row/column was significant *(alpha = 0.05)*.<br>*Squares and transparent fill represent non-significant values. P-values were adjusted independently by variable using fdr correction.*"),
      locations = cells_title(groups = "subtitle"))
  
  return(gt_table)
}

nutrients_wide <- function(df) {
  df %>%
    pivot_wider(id_cols     = "identifier",
                names_from  = "nutrient",
                values_from = "fed") %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))
}
