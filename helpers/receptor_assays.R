
assay_img <- function(basename) {
  paste0("https://raw.githubusercontent.com/Rich-Molecular-Health-Lab/bioinformatics_stats/a4a076e3d790ded0aed7a3c352ccb22291b467b8/images/", basename, ".png")
}

data_check_logos <- function(idx) {
  img_layer <- list(
    source   = assay_img(idx),
    xref     = "paper", 
    yref     = "paper",
    x         = 1,
    y         = 0.9,
    sizex    = 0.25,     
    sizey    = 0.25,
    xanchor  = "right",  
    yanchor  = "top",
    layer    = "above",
    opacity  = 0.7
  )
  return(img_layer)
}

drc_logos <- function(idx) {
  img_layer <- list(
    source   = assay_img(idx),
    xref     = "paper", 
    yref     = "paper",
    x         = 1,
    y         = 0.9,
    sizex    = 0.25,     
    sizey    = 0.25,
    xanchor  = "right",  
    yanchor  = "top",
    layer    = "above",
    opacity  = 0.7
  )
  return(img_layer)
}

drc_hover <- function() {
  paste0("log10(dose) = %{x:.2f}<br>activation = %{y:.2f}")
}

subplot_titles <- function(data, var) {
  title <- unique(pull(data, var)) %>% str_to_title()
  list(
    x         = 1,
    y         = 0.9,
    text      = title,
    xref      = "paper",
    yref      = "paper",
    showarrow = FALSE,
    xanchor   = "right",  
    yanchor   = "bottom",
    font      = list(size = 10),
    layer     = "below"
  )
}

annotate_subplots <- function(plot, data) {
  title <- unique(pull(data, var)) %>% str_to_title()
  file <- unique(pull(data, model))
  out <- add_image(
    p        = plot,
    source   = assay_img(file),
    xref     = "paper", 
    yref     = "paper",
    x         = 1,
    y         = 0.9,
    sizex    = 0.25,     
    sizey    = 0.25,
    xanchor  = "right",  
    yanchor  = "top",
    layer    = "above",
    opacity  = 0.7
  ) %>%
  add_annotations(
    x         = 1,
    y         = 0.9,
    text      = title,
    xref      = "paper",
    yref      = "paper",
    showarrow = FALSE,
    xanchor   = "right",  
    yanchor   = "bottom",
    font      = list(size = 10),
    layer     = "below"
  )
  return(out)
}

data_check_box <- function(df, xvar, yvar, group) {
  data_plot <- df %>%
    arrange(genus, receptor, treat_type, treat_subtype, treat, plate) %>%
    mutate(across(where(is.factor), ~as.character(.))) %>%
    filter(!is.na(dose)) %>%
    group_by(genus, receptor, treat_type) %>%
    group_split() %>%
    set_names(map(., \(x) first(x$model)))
  
  plots <- imap(data_plot,
                \(x, idx) box_trace(x, idx, xvar, yvar, group))
  subplots <- subplot(plots, 
                      nrows  = 3, 
                      shareX = FALSE, 
                      shareY = FALSE) %>%
    add_annotations(
      x         = 0.5,
      y         = -0.01,
      text      = xvar,
      xref      = "paper",
      yref      = "paper",
      showarrow = FALSE,
      xanchor   = "center",  
      yanchor   = "top",
      font      = list(size = 12)
    ) %>%
    add_annotations(
      x         = -0.01,
      y         = 0.5,
      text      = yvar,
      xref      = "paper",
      yref      = "paper",
      showarrow = FALSE,
      xanchor   = "right",  
      yanchor   = "center",
      font      = list(size = 12),
      textangle = -90
    )
  return(subplots)
}

box_trace <- function(data, idx, xvar, yvar, group) {
  n_colors <- length(unique(pull(data, group)))
  img_layer <- list(
    source   = assay_img(idx),
    xref     = "paper", 
    yref     = "paper",
    x         = 1,
    y         = 0.9,
    sizex    = 0.25,     
    sizey    = 0.25,
    xanchor  = "right",  
    yanchor  = "top",
    layer    = "above",
    opacity  = 0.7
  )
  subplottitle <- subplot_titles(data, "treat_type") 
  col_pal <- sample(as.character(paletteer_d("khroma::muted")), 
                    size    = n_colors, 
                    replace = TRUE)
  plot <- plot_ly(data = data) %>%
    add_trace(
      x         = data[[xvar]],
      y         = data[[yvar]],
      text      = ~date,
      color     = data[[group]],
      colors    = col_pal,
      type      = "box",
      boxpoints = "all",
      line      = list(
        color = "#000000",
        width = 0.7
      ),
      marker   = list(
        size = 3,
        line = list(
          color = "#000000",
          width = 0.7
        ),
        opacity = 0.3
      )
    ) %>%
    layout(
      showlegend = FALSE,
      annotations = subplottitle,
      images = img_layer,
      xaxis = list(
        zeroline = F,
        showline = T,
        showticklabels = F
      ),
      yaxis = list(
        zeroline = F,
        showline = T,
        showticklabels = F
      )
    )
  return(plot)
}

drc_conf_bands <- function(plot, data) {
  add_ribbons(
    p             = plot,
    data          = data,
    x             = ~log10_dose,
    ymin          = ~lower,
    ymax          = ~upper,
    fillcolor     = "#6D7F7F33",
    name          = ~label,
    line          = list(color = "#172530FF", width = 0.5),
    hovertemplate = drc_hover()
  )
}

drc_trace_real <- function(plot, data) {
  add_trace(p             = plot,
            data          = data,
            x             = ~log10_dose,
            y             = ~fold_act,
            type          = "scatter",
            mode          = "markers",
            name          = ~label,
            marker        = list(size  = 8,
                                 color = "#5A2D2780",
                                 line  = list(color = "#172530FF", 
                                              width = 1.5)),
            showlegend    = FALSE,
            hovertemplate = drc_hover()
  )
}

drc_trace_pred <- function(plot, data) {
  add_trace(p             = plot,
            data          = data,
            x             = ~log10_dose,
            y             = ~resp,
            type          = "scatter",
            mode          = "lines+markers",
            name          = ~label,
            line          = list(color = "#172530FF", width = 2),
            marker        = list(size  = 1,
                                 color = "#5A2D2780",
                                 line  = list(color = "#172530FF", 
                                              width = 0.5)),
            showlegend    = FALSE,
            hovertemplate = drc_hover()
  )   %>% 
    drc_conf_bands(data)
}

drc_plot_each <- function(data_real, data_pred) {
  plot_ly() %>%
    drc_trace_pred(data_pred) %>%
    drc_trace_real(data_real) %>%
    annotate_subplots(data_real) %>%
    layout(
      showlegend   = FALSE,
      xaxis        = list(
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = F,
        nticks         = 3
      ),
      yaxis        = list(
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = F,
        nticks         = 4
      )
    )
}

drc_plot_map <- function() {
  df_real <- df %>%
    select(-predict) %>%
    unnest(data)
  
  df_pred <- df %>%
    select(-data) %>%
    unnest(predict) %>%
    left_join(select(df_real, plate:treat_desc), 
              by = join_by(plate, genus, receptor, treat)) %>%
    mutate(log10_dose = log10(dose))
    
}


dr_curve_trace <- function(data, grid = FALSE) {
  if (isFALSE(grid)) { 
    mark_size <- 8 
  } else if (isTRUE(grid)) {
    mark_size <- 1.5
  }
  trace <- plot_ly(data = data) %>%
    add_trace(x             = ~log10(dose + .1),
              y             = ~resp,
              type          = "scatter",
              mode          = "lines+markers",
              name          = data$label,
              line          = list(color = "#172530FF", width = 2),
              marker        = list(size  = mark_size,
                                   color = "#5A2D2780",
                                   line  = list(color = "#172530FF", 
                                                width = 1.5)),
              showlegend    = FALSE,
              hovertemplate = drc_hover()
    )   %>% 
    drc_conf_bands(data)
  return(trace)
}

dr_curve_layout <- function(plot, data, grid = FALSE) {
  xlab <- list(
    text = "log10(Dose)",
    font = list(size = 10))
  ylab <- list(
    text = paste0("ER", first(data$receptor), " Response"),
    font = list(size = 10))
  annot <- list(
    x         = 0.9,
    y         = 0.9,
    text      = first(data$treat_name),
    xref      = "paper",
    yref      = "paper",
    showarrow = FALSE,
    xanchor   = "left",  
    yanchor   = "top"
  )
  img_layer <- drc_logos(first(data$filename))  
  if (isFALSE(grid)) {
    ymode <- list(
      title          = ylab,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      nticks         = 4,
      tickfont       = list(size = 8)
    )
    xmode <- list(
      title          = xlab,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      nticks         = 3,
      tickfont       = list(size = 8)
    )
  } else if (isTRUE(grid)) {
    ymode <- list(
      title          = ylab,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = F,
      nticks         = 4
    )
    xmode <- list(
      title          = xlab,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = F,
      nticks         = 3
    )
  }
  layout_plot <-  layout(
    p            = plot,
    showlegend   = FALSE,
    xaxis        = xmode,
    yaxis        = ymode
  )
  
  return(layout_plot)
}

dr_plot <- function(data, grid = FALSE) {
  dr_curve_trace(data , grid = grid) %>%
  dr_curve_layout(data, grid = grid)
}

dr_card <- function(data) {
card(
    full_screen = TRUE,
    id          = first(data$plot_id),
    card_header(
      card_image(
        src = assay_img(first(data$filename)), width = "10%"
        ),
      HTML(first(data$treat_name))
      ),
    card_body(fillable = FALSE, dr_plot(data))
  )
}

dr_grid <- function(data_list, cols = 3) {
  width <- floor(12/cols)
  cards <- map(data_list, \(x) dr_card(x)) %>%
    tagList() %>%
    list_flatten()
  layout <- do.call(
    bscols,
    c(
      list(widths = rep(width, length(cards))),
      cards
    )
  )
  return(layout)
}



dr_subplot <- function(data_list) {
  plots <- map(data_list, \(x) dr_plot(x, grid = TRUE))
  n  <- length(plots)
  nc <- 11
  nr <- ceiling(n / nc)
  
  grid <- subplot(plots,
                      nrows      = nr,
                      shareX     = FALSE,
                      shareY     = FALSE,
                      titleX     = FALSE,
                      titleY     = FALSE,
                      margin     = 0.001)
  return(grid)
}