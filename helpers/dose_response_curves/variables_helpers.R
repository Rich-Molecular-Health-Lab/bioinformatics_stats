treat_vars <- c(
  "treat_type",
  "treat_subtype",
  "treat",
  "treat_name",
  "treat_desc"
)

group_qc <- c(
  "plate",
  "genus", 
  "subj", 
  "receptor",
  "model",
  treat_vars
)

group_pooled <- c(
  "genus", 
  "subj", 
  "receptor",
  "model",
  treat_vars
)

assay_img <- function(basename) {
  paste0("https://raw.githubusercontent.com/Rich-Molecular-Health-Lab/bioinformatics_stats/a4a076e3d790ded0aed7a3c352ccb22291b467b8/images/", basename, ".png")
}

logo_tag <- function(basename) {
  tagList(img(src = assay_img(basename), 
              style="width:10%; height:10%;"))
}

dr_hover <- function() {
  paste0("dose = %{x:.2f}<br>response = %{y:.2f}")
}

dose_pal <- function(data, variable) {
  n_colors <- length(unique(pull(data, variable)))
  pal <-  as.character(paletteer_c("harrypotter::lunalovegood", n = n_colors))
  return(pal)
}

