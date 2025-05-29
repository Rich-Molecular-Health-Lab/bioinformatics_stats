col.pal <- function(data, variable) {
  n_colors <- length(unique(pull(data, paste0(variable))))
  pal      <-  as.character(paletteer_c("harrypotter::lunalovegood", n = n_colors))
  return(pal)
}

boxplot_ed50 <- function(data) {
  colors <- unlist(data[["color_key"]][1])
  plot <- plot_ly() %>%
    add_trace(
      data      = data,
      x         = ~groups_treatment,
      y         = ~ED50,
      type      = "box",
      boxpoints = "all",
      color     = ~labs_genus,
      colors    = colors,
      text      = ~labs_genus,
      line      = list(
        color = "#000000FF", width = 0.5
      ),
      marker    = list(
        size         = 7,
        opacity      = 0.8,
        outliercolor = "#D5114EFF",
        line         = list(
          color = "#000000FF", width = 0.5
        )
      )
    ) %>%
    layout(
      showlegend = FALSE,
      boxmode    = "group",
      xaxis      = list(
        title = "Treatment Type",
        zeroline = F,
        showgrid = F,
        showline = T,
        showticklabels = T
      ),
      yaxis      = list(
        title = "ED50",
        zeroline = F,
        showgrid = F,
        showline = T,
        showticklabels = T,
        tickformat     = ".0%",
        hoverformat    = ".0%"
      )
    )
  

  return(plot)
  
}

card_ed50 <- function(data, genus) {
  
  plot <- boxplot_ed50(data)
  img_name <- as.character(str_extract_all(data[["receptor"]][1], "^\\w+(?=_)"))
  logo_icon <- tagList(img(src = assay_img(img_name), style="width:5%; height:5%;"))
  
  card <- card(
    card_header(logo_icon, HTML(genus)),
    card_body(plot, padding = c(0, 10, 10, 0)),
    full_screen = TRUE,
    fill        = FALSE
  )

    return(card)
}

panel_ed50 <- function(data_list, er) {
  
  cards  <- imap(data_list, \(x, idx) card_ed50(x, idx))
  layout <- do.call(bscols, c(list(widths = rep(4, length(cards))), cards))
  panel  <- nav_panel(title = er, layout)

  return(panel)
}

navset_ed50 <- function(data_list) {
  panels <- imap(data_list, \(x, idx) panel_ed50(x, idx)) %>%
    set_names(., "")
  out <- navset_tab(!!!panels)
  return(out)
}



boxplot_treatments <- function(data, yval) {
  hovert <- paste0("%{text}<br>Treatment: %{x}<br>", yval, " = %{y:.2f}")
  estrogens <- filter(data, groups_treatment == "Estrogen")
  psms      <- filter(data, groups_treatment == "PSM")
  microbial <- filter(data, groups_treatment == "Microbial")
  diet      <- filter(data, groups_treatment == "Diet Extract")
  logo     <- assay_img(paste0(data[["receptor"]][1]))
  plot <- plot_ly(data = data) %>%
    add_trace(
      data      = estrogens,
      x         = ~labs_treatment,
      y         = estrogens[[yval]],
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
        source    = ,
        x         = 0,
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

receptor_boxplot_grid <- function(data, groupby, yval) {
  data_list <- data %>%
    left_join(meta_working, by = groupby) %>%
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
  save_html(grid_coefs, paste0("visuals/receptor_assays/boxplots_", yval, ".html"))
  
  return(grid_coefs)
}

