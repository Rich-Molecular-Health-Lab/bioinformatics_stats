assay_img <- function(basename) {
  paste0("https://raw.githubusercontent.com/Rich-Molecular-Health-Lab/bioinformatics_stats/a4a076e3d790ded0aed7a3c352ccb22291b467b8/images/", basename, ".png")
}


boxplot_treatments <- function(data, yval) {
  hovert <- paste0("%{text}<br>Treatment: %{x}<br>", yval, " = %{y:.2f}")
  estrogens <- filter(data, groups_treatment == "Estrogen")
  psms      <- filter(data, groups_treatment == "PSM")
  microbial <- filter(data, groups_treatment == "Microbial")
  diet      <- filter(data, groups_treatment == "Diet Extract")
  plot <- plot_ly(data = data) %>%
    add_trace(
      data      = estrogens,
      x         = ~labs_treatment,
      y         = estrogens[[yval]],
      text      = ~label,
      type      = "box",
      boxpoints = "suspectedoutliers",
      fillcolor = "#BDA14D80",
      line      = list(color = "#000000FF", width = 0.5),
      marker    = list(
        size         = 7,
        opacity      = 0.8,
        outliercolor = "#D5114EFF",
        line         = list(color = "#000000FF", width = 0.5)
      ),
      hovertemplate = hovert
    ) %>%
    add_trace(
      data      = psms,
      x         = ~labs_treatment,
      y         = psms[[yval]],
      text      = ~label,
      type      = "box",
      boxpoints = "suspectedoutliers",
      fillcolor = "#3EBCB680",
      line      = list(color = "#000000FF", width = 0.5),
      marker    = list(
        size         = 7,
        opacity      = 0.8,
        outliercolor = "#D5114EFF",
        line         = list(color = "#000000FF", width = 0.5)
      ),
      hovertemplate = hovert
    ) %>%
    add_trace(
      data      = microbial,
      x         = ~labs_treatment,
      y         = microbial[[yval]],
      text      = ~label,
      type      = "box",
      boxpoints = "suspectedoutliers",
      fillcolor = "#0169C480",
      line      = list(color = "#000000FF", width = 0.5),
      marker    = list(
        size         = 7,
        opacity      = 0.8,
        outliercolor = "#D5114EFF",
        line         = list(color = "#000000FF", width = 0.5)
      ),
      hovertemplate = hovert
    ) %>%
    add_trace(
      data      = diet,
      x         = ~labs_treatment,
      y         = diet[[yval]],
      text      = ~label,
      type      = "box",
      boxpoints = "suspectedoutliers",
      fillcolor = "#A56EB6FF",
      line      = list(color = "#000000FF", width = 0.5),
      marker    = list(
        size         = 7,
        opacity      = 0.8,
        outliercolor = "#D5114EFF",
        line         = list(color = "#000000FF", width = 0.5)
      ),
      hovertemplate = hovert
    ) %>%
    layout(
      images      = list(
        source    = assay_img(first(data[["receptor"]])),
        x         = 0.05,
        y         = 0.95,
        xref      = "paper",
        yref      = "paper",
        sizex     = 0.2,  
        sizey     = 0.2,
        xanchor   = "left",
        yanchor   = "top",
        opacity   = 0.8
      ),
      showlegend   = FALSE
    ) %>%
    hide_colorbar()
  return(plot)
  
}

subplot_1row <- function(plot_list) {
  subplot(
    plot_list,
    nrows  = 1,
    shareY = FALSE
  )
}

layout_bottom <- function(plot, ylab) {
  layout(
    p            = plot,
    xaxis        = list(
      title          = "Treatment",
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T,
      ticks          = "inside",
      tickfont       = list(size = 10)
    ),
    yaxis        = list(
      title          = ylab,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T,
      nticks         = 5,
      ticks          = "inside",
      tickfont       = list(size = 10),
      hoverformat    = ".2f"
    )
  )
}

layout_top <- function(plot, ylab) {
  layout(
    p            = plot,
    xaxis        = list(
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = F,
      ticks          = "inside"
    ),
    yaxis        = list(
      title          = ylab,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T,
      nticks         = 5,
      ticks          = "inside",
      tickfont       = list(size = 10),
      hoverformat    = ".2f"
    )
  )
}

receptor_boxplot_grid <- function(data, groupby, yval, file_suffix = NULL) {
  
  if (is.null(file_suffix)) { file_suffix <- "" }
  
  if (str_detect(groupby, "plate")) {
    data <- data %>%
      left_join(meta_working, by = groupby) %>%
      mutate(label = as.character(str_glue("Plate {plate} ({date})")))
  } else if (!str_detect(groupby, "plate")) {
    data <- data %>%
      left_join(select(meta_working, -c(plate, date, group_plate)), by = groupby) %>%
      mutate(label = "All Plates Pooled")
  }
  data_list <- data %>%
    arrange(receptor, groups_treatment, treatment) %>%
    group_by(receptor) %>%
    group_split() %>%
    set_names(., map(., \(x) paste0(first(x[["receptor"]]))))
  
  plots_all <- map(data_list, \(x) boxplot_treatments(x, yval))
  
  plots_alpha <- keep_at(plots_all, ~str_ends(.x, "alpha")) %>%
    map(\(x) layout_top(x, yval)) %>%
    subplot_1row()
  plots_beta  <- keep_at(plots_all, ~str_ends(.x, "beta")) %>%
    map(\(x) layout_bottom(x, yval)) %>%
    subplot_1row()
  
  grid_coefs <- subplot(
    plots_alpha,
    plots_beta ,
    nrows = 2,
    shareX = FALSE
  )
  save_html(grid_coefs, paste0("visuals/receptor_assays/boxplots_", yval, file_suffix, ".html"))
  
  return(grid_coefs)
}

