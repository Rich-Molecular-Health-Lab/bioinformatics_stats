map_qqnorm <- function(x){
  res   <- x[[".resid"]]
  dose  <- x[["dose"]]
  group <- x[["group"]]
  qq  <- qqnorm(res, plot.it = FALSE)
  tibble(
    theoretical = qq$x,              
    sample      = qq$y
  ) %>%
    bind_cols(x)
}

col_list <- function(df, column, palette = NULL) {
  if (is.null(palette)) { palette <-  "unikn::pal_unikn_pref"}
  
  values   <- unique(pull(df, as.character(column)))
  n_colors <- length(values)
  
  colors <- sample(as.character(paletteer_d(palette)), n_colors, replace = TRUE) %>%
    set_names(values)
  
  data <- df %>% add_column(color_key = list(colors))
  
  return(data)
}


dose_response_list <- function(df) {
  df %>%
    mutate(hover = as.character(str_glue(
      "{labs_plate}<br>{labs_treatment}"
    ))) %>%
    mutate(treatment = as.character(str_glue(
      "{as.character(groups_treatment)}_{as.character(treatment)}"
    ))) %>%
    arrange(receptor, dose_type, treatment) %>%
    group_by(receptor) %>%
    group_split() %>%
    set_names(., map(., \(x) paste0(first(x$receptor)))) %>%
    map_depth(1, \(x) nest(x, .by = "labs_treatment")) %>%
    map_depth(1, \(x) deframe(x)) 
}

dose_response_list_backup <- function(df) {
  df %>%
    mutate(hover = as.character(str_glue(
      "{labs_plate}<br>{labs_treatment}"
    ))) %>%
    mutate(treatment = as.character(str_glue(
      "{as.character(groups_treatment)}_{as.character(treatment)}"
    ))) %>%
    arrange(receptor, dose_type, treatment) %>%
    group_by(receptor) %>%
    group_split() %>%
    set_names(., map(., \(x) paste0(first(x$receptor)))) %>%
    map(\(x) group_by(x, treatment)) %>%
    map(\(x) group_split(x)) %>%
    map_depth(1, \(x) set_names(
      x, map(x, \(y) paste0(first(y$labs_treatment)))
    )) %>%
    map_depth(2, \(x) nest(x, .by = "group_pooled")) %>%
    map_depth(2, \(x) deframe(x)) 
}


dose_response_markers <- function(size = 7, opacity = 0.6) {
  list(size    = size,
       opacity = opacity,
       line    = list(color = "#000000", width = 1)
  )
}


curve_trace <- function(plot, data, name) {
  colors  <- unlist(data[["color_key"]][1])
  trace <- add_trace(
    p          = plot,
    data       = filter(data, value_type == "predicted"),
    x          = ~dose,
    y          = ~response,
    text       = ~hover,
    name       = name,
    type       = "scatter",
    mode       = "lines",
    color      = ~labs_plate,
    colors     = colors,
    zorder     = 1,
    line       = list(width = 2.5, shape = "spline"),
    showlegend = FALSE
  ) %>%
    add_ribbons(
      data       = filter(data, value_type == "predicted"),
      x          = ~dose,
      ymin       = ~response_lower,
      ymax       = ~response_upper,
      text       = ~hover,
      name       = "95% conf for predicted values",
      color      = ~labs_plate,
      colors     = colors,
      zorder     = 0,
      opacity    = 0.3,
      line       = list(width = 0.1, shape = "spline"),
      hoveron      = "fills",
      showlegend = FALSE
    ) %>%
    add_trace(
      data       = filter(data, value_type == "fitted"),
      x          = ~dose,
      y          = ~response,
      text       = ~hover,
      name       = name,
      zorder     = 2,
      type       = "scatter",
      mode       = "markers",
      color      = ~labs_plate,
      colors     = colors,
      marker     = dose_response_markers(),
      showlegend = FALSE
    )
  return(trace)
}

curve_loop <- function(data, name, log = FALSE) {
  dose_type <- data[["dose_type"]][1]
  plot <- plot_ly() %>%
    curve_trace(data, name) %>%
    dose_response_layout(log, dose_type)
  return(plot)
}

curve_loop_backup <- function(list_traces, log = FALSE) {
  p   <- plot_ly()
  for(i in 1:length(list_traces)) {
    data   <- pluck(list_traces, i)
    dose_type <- data[["dose_type"]][1]
    p      <- curve_trace(p, data)
  }
  
  out <- dose_response_layout(p, log, dose_type)
  return(out)
}


raw_scatter_trace <- function(plot, data, name) {
  colors  <- unlist(data[["color_key"]][1])
  add_trace(
    p          = plot,
    data       = data,
    x          = ~dose,
    y          = ~response,
    text       = ~hover,
    name       = name,
    type       = "scatter",
    mode       = "markers",
    color      = ~labs_plate,
    colors     = colors,
    marker     = dose_response_markers(),
    showlegend = FALSE
  )
}

raw_scatter_loop <- function(data, name, log = FALSE) {
  dose_type <- data[["dose_type"]][1]
  plot <- plot_ly() %>%
    raw_scatter_trace(data, name) %>%
    dose_response_layout(log, dose_type)
  return(plot)
}

res_trace <- function(plot, data, name) {
  colors  <- unlist(data[["color_key"]][1])
  add_trace(
    p          = plot,
    data       = data,
    x          = ~response,
    y          = ~resid,
    text       = ~hover,
    name       = name,
    type       = "scatter",
    mode       = "markers",
    color      = ~labs_plate,
    colors     = colors,
    marker     = dose_response_markers(),
    showlegend = FALSE
  )
}

qq_trace <- function(plot, data, name) {
  colors  <- unlist(data[["color_key"]][1])
  res   <- data[["resid"]]
  i25   <- which.min(abs(data[["theoretical"]] - quantile(data[["theoretical"]], .25)))
  i75   <- which.min(abs(data[["theoretical"]] - quantile(data[["theoretical"]], .75)))
  slope <- diff(data[["sample"]][c(i25,i75)])/diff(data[["theoretical"]][c(i25,i75)])
  int   <- data[["sample"]][i25] - slope * data[["theoretical"]][i25]
  x0    <- min(data[["theoretical"]]); x1 <- max(data[["theoretical"]])
  y0    <- slope * x0 + int; y1 <- slope * x1 + int
  plot <- add_trace(
    p          = plot,
    data       = data,
    x          = ~theoretical,
    y          = ~sample,
    type       = "scatter",
    mode       = "markers",
    text       = ~hover,
    color      = ~labs_plate,
    colors     = colors,
    marker     = dose_response_markers(),
    showlegend = FALSE
  ) %>%
    add_segments(
      data       = data,
      text       = name,
      x          = x0, 
      xend       = x1,
      y          = y0, 
      yend       = y1,
      line       = list(width = 2, color = "#000000FF"),
      showlegend = FALSE
    )
  return(plot)
}

resid_loop <- function(data, name) {
  plot <- plot_ly() %>%
    res_trace(data, name) %>%
    residual_layout()
  return(plot)
}

qq_loop <- function(data, name) {
  plot <- plot_ly() %>%
    qq_trace(data, name) %>%
    residual_layout(qqnorm = TRUE)
  return(plot)
}

dose_response_layout <- function(plot, log = FALSE, dose_type) {
  if (isTRUE(log)) {
    shapes <- list(
      type = "line",
      line = list(width = 0.75),
      x0   = 0,
      y0   = 0,
      x1   = 0,
      y1   = 1,
      xref = "paper",
      yref = "paper"
    )
  } else if (isFALSE(log)) {
    shapes <- NULL
  }
  
  if (dose_type == "mg/ml" & isFALSE(log)) {
    xlab <- "Dose (mg/ml)"
    xaxis <- list(
      title          = list(
        font = list(size = 9),
        text = xlab
        ),
      type           = "linear",
      range          = list(0, 2.1),
      tickformat     = ".1f",
      hoverformat    = ".1f",
      tick0          = 0.2,
      dtick          = 0.5,
      ticks          = "inside",
      tickfont       = list(size = 8),
      tickangle      = 30,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T
    )
    
  } else if (dose_type == "mg/ml" & isTRUE(log)) {
    xlab <- "Log10 Dose (mg/ml)"
    xaxis <- list(
      title          = list(
        font = list(size = 9),
        text = xlab
      ),
      type           = "log",
      range          = list(log10(0), log10(2.1)),
      tickformat     = ".1f",
      hoverformat    = ".1f",
      tick0          = log10(0.2),
      dtick          = 0.5,
      ticks          = "inside",
      tickfont       = list(size = 8),
      tickangle      = 30,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T
    )
  } else if (dose_type == "molar" & isFALSE(log)) {
    xlab <- "Dose (M)"
    xaxis <- list(
      title          = list(
        font = list(size = 9),
        text = xlab
      ),
      tickformat     = ".0e",
      hoverformat    = ".0e",
      ticks          = "inside",
      tickfont       = list(size = 8),
      tickangle      = 30,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T
    )
  } else if (dose_type == "molar" & isTRUE(log)) {
    xlab <- "Log 10 Dose (M)"
    xaxis <- list(
      title          = list(
        font = list(size = 9),
        text = xlab
      ),
      type           = "log",
      exponentformat = "power",
      tickformat     = ".0e",
      hoverformat    = ".0e",
      tick0          = log10(1e-9),
      dtick          = 2,
      ticks          = "inside",
      tickfont       = list(size = 8),
      tickangle      = 30,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T
    )
  }
  plot_out <-   plot %>%
    layout(
      p            = plot,
      showlegend   = FALSE,
      shapes       = shapes,
      xaxis        = xaxis,
      yaxis        = list(
        title          = list(
          font = list(size = 9),
          text = "Response (% Baseline)"
        ),
        rangemode      = "nonnegative",
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = T,
        ticks          = "inside",
        tickfont       = list(size = 8),
        tickformat     = ".0%",
        hoverformat    = ".0%"
      )) %>%
    config(responsive = TRUE)
  return(plot_out)
}

residual_layout <- function(plot, qqnorm = FALSE) {
  
  if (isFALSE(qqnorm)) {
    resid_line <- list(
      type = "line",
      x0   = 0,
      x1   = 1,
      y0   = 0.5,
      y1   = 0.5,
      xref = "paper",
      yref = "paper",
      line = list(
        dash  = "dot",
        width = 2,
        color = "#000000BF"
      )
    )
    xlab <- "Fitted Values"
    ylab <- "Residual Values"
    yside <- "left"
  } else if (isTRUE(qqnorm)) {
    resid_line <- NULL
    xlab <- "Theoretical Quantiles"
    ylab <- "Sample Quantiles"
    yside <- "right"
  }
  
  plot_out <- plot %>%
  layout(
    showlegend = FALSE,
    shapes       = resid_line,
    xaxis        = list(
      title          = list(
        font = list(size = 9),
        text = xlab
        ),
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T,
      nticks         = 4,
      ticks          = "inside",
      tickfont       = list(size = 8),
      tickformat     = ".1f",
      hoverformat    = ".1f"
    ),
    yaxis        = list(
      title          = list(
        font = list(size = 9),
        text = ylab
      ),
      side           = yside,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T,
      nticks         = 4,
      ticks          = "inside",
      tickfont       = list(size = 8),
      tickformat     = ".1f",
      hoverformat    = ".1f"
    )
  ) %>%
    config(responsive = TRUE)
  return(plot_out)
}

dose_response_card <- function(data, title_plot, plot_function) {
  
  if (plot_function == "plot_curve") {
    plot_A     <- curve_loop(data, title_plot)
    plot_B     <- curve_loop(data, title_plot, log = TRUE)
    ylab       <- "Response (fold act)"
    shareY     <- TRUE
  } else if (plot_function == "plot_raw") {
    plot_A     <- raw_scatter_loop(data, title_plot)
    plot_B     <- raw_scatter_loop(data, title_plot, log = TRUE)
    ylab       <- "Response (fold act)"
    shareY     <- TRUE
  } else if (plot_function == "plot_resid_qq") {
    plot_A     <- resid_loop(data, title_plot)
    plot_B     <- qq_loop(data, title_plot)
    ylab       <- "Residual Values"
    shareY     <- FALSE
  } 
  
  plot_out <- subplot(
    plot_A,
    plot_B,
    nrows = 1,
    shareX = FALSE,
    shareY = shareY,
    titleX = TRUE,
    titleY = TRUE
  ) %>%
    config(responsive = TRUE)

  
  card <- card(
    card_header(title_plot),
    card_body(plot_out, 
              padding = c(0, 10, 10, 0)),
    full_screen = TRUE,
    max_height  = "190px"
  )
  return(card)
}

dose_response_grid <- function(list_plots, receptor, plot_function) {
  
  plots <- imap(list_plots, \(x, idx) dose_response_card(x, idx, plot_function))
  
  layout <- do.call(bscols, c(list(widths = rep(3, length(plots))), plots))
  
  logo_icon <- tagList(img(src = assay_img(receptor), style="width:15%; height:15%;"))
  
  if (str_ends(receptor, "alpha")) { er <-  "ER \u03B1"} else { er <- "ER \u03B2" }
  
  species <- str_to_title(str_extract(receptor, "^\\w+(?=_)"))
  title   <- paste(er, species)
  out     <- nav_panel(HTML(title), layout, icon = logo_icon)
  
  return(out)
}

db_xaxis <- function(data) {
  if (data$dose_type[1] == "mg/ml") {
    list(
      title = "Effective Dose (mg/ml)",
      tickformat  = ".2f",
      hoverformat = ".2f",
      zeroline       = F,
      showgrid       = T,
      gridcolor      = "#88888866",
      showline       = T
    )
  } else if (data$dose_type[1] == "molar") {
    list(
      title = "Effective Dose (M)",
      tickformat     = ".0e",
      hoverformat    = ".0e",
      exponentformat = "power",
      zeroline       = F,
      showgrid       = T,
      gridcolor      = "#88888866",
      showline       = T
    )
  }
}

db_yaxis <- function(data) {
  if (data$receptor[1] == "alpha") {
    list(
      title          = "Treatment",
      side           = "left",
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      mirror         = T,
      ticks          = "outside",
      ticktext       = as.list(pull(data, ticklab_y)),
      tickvals       = as.list(pull(data, dummy_y))
    )
  } else if (data$receptor[1] == "beta") {
    list(
      title          = "Treatment",
      side           = "right",
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      mirror         = T,
      ticks          = "outside",
      ticktext       = as.list(pull(data, ticklab_y)),
      tickvals       = as.list(pull(data, dummy_y))
    )
  }
}


db_shapes <- function(data) {
  group_by(data, labs_treatment) %>%
    summarize(
      y_start = min(dummy_y),
      y_end   = max(dummy_y)
    ) %>%
    rowwise() %>%
    mutate(y_start = y_start - 1,
           y_end   = y_end   + 1) %>%
    rowwise() %>%
    group_split() %>%
    map(\(x) list(
      type       = "rect",
      fillcolor  = "#eeeeee",
      layer      = "below",
      opacity    = 0.3,
      showlegend = F,
      line       = list(width = 0),
      x0         = 0,
      x1         = 1,
      xref       = "paper",
      y0         = x$y_start, 
      y1         = x$y_end, 
      name       = x$labs_treatment,
      yref       = "y"
    ))
}

db_plot <- function(data) {
  colors  <- c(loris = "#882255FF", gorilla = "#332288FF", human = "#117733FF")
  markers <- list(opacity = 0.7, size = 8, line = list(color = "#00000FFF", width = 1))
  plot <- plot_ly() %>%
    add_segments(
      data = data$data,
      x    = ~Estimate_ED40,
      xend = ~Estimate_ED60,
      y    = ~dummy_y,
      yend = ~dummy_y,
      split = ~subject,
      color = ~subject,
      colors = colors,
      showlegend = FALSE
    )  %>%
    add_markers(
      data = data$data,
      x      = ~Estimate_ED40,
      y      = ~dummy_y,
      name   = ~str_to_title(subject),
      split = ~subject,
      color = ~subject,
      colors = colors,
      marker = markers,
      showlegend = FALSE
    )  %>%
    add_markers(
      data = data$data,
      x      = ~Estimate_ED60,
      y      = ~dummy_y,
      name   = ~str_to_title(subject),
      split = ~subject,
      color = ~subject,
      colors = colors,
      marker = markers,
      showlegend = FALSE
    )  %>%
    layout(
      shapes = data$shapes,
      xaxis  = data$xaxis,
      yaxis  = data$yaxis
    ) %>%
    config(responsive = TRUE)
  
  return(plot)
}

db_layout_receptors <- function(list_receptors) {
  plots <- map(list_receptors, \(x) db_plot(x))
  subplots <- subplot(
    plots,
    nrows = 1,
    shareX = F,
    shareY = F
  )
  return(subplots)
}
