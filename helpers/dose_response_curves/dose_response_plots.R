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


dose_response_markers <- function(size = 7, opacity = 0.6) {
  list(size    = size,
       opacity = opacity,
       line    = list(color = "#000000", width = 1)
  )
}


curve_trace <- function(plot, data) {
  colors  <- unlist(data[["color_key"]][1])
  trace <- add_trace(
    p          = plot,
    data       = filter(data, value_type == "predicted"),
    x          = ~dose,
    y          = ~response,
    text       = ~hover,
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

curve_loop <- function(data, log = FALSE) {
  dose_type <- data[["dose_type"]][1]
  plot <- plot_ly() %>%
    curve_trace(data) %>%
    dose_response_layout(log, dose_type)
  return(plot)
}

resid_loop <- function(data) {
  plot <- plot_ly() %>%
    res_trace(data) %>%
    residual_layout()
  return(plot)
}

qq_loop <- function(data) {
  plot <- plot_ly() %>%
    qq_trace(data) %>%
    residual_layout(qqnorm = TRUE)
  return(plot)
}


raw_scatter_trace <- function(plot, data) {
  colors  <- unlist(data[["color_key"]][1])
  add_trace(
    p          = plot,
    data       = data,
    x          = ~dose,
    y          = ~response,
    text       = ~hover,
    type       = "scatter",
    mode       = "markers",
    color      = ~labs_plate,
    colors     = colors,
    marker     = dose_response_markers(),
    showlegend = FALSE
  )
}

raw_scatter_loop <- function(data, log = FALSE) {
  dose_type <- data[["dose_type"]][1]
  plot <- plot_ly() %>%
    raw_scatter_trace(data) %>%
    dose_response_layout(log, dose_type)
  return(plot)
}

res_trace <- function(plot, data) {
  colors  <- unlist(data[["color_key"]][1])
  add_trace(
    p          = plot,
    data       = data,
    x          = ~response,
    y          = ~resid,
    text       = ~hover,
    type       = "scatter",
    mode       = "markers",
    color      = ~labs_plate,
    colors     = colors,
    marker     = dose_response_markers(),
    showlegend = FALSE
  )
}

qq_trace <- function(plot, data) {
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
      x          = x0, 
      xend       = x1,
      y          = y0, 
      yend       = y1,
      line       = list(width = 2, color = "#000000FF"),
      showlegend = FALSE
    )
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
  plot_out <-   plot %>%
    layout(
      p            = plot,
      showlegend   = FALSE,
      shapes       = shapes,
      xaxis        = dose_xaxis(log, dose_type),
      yaxis        = response_yaxis()) %>%
    config(responsive = TRUE)
  return(plot_out)
}

resid_xaxis <- function(qqnorm = FALSE) {
  if (isFALSE(qqnorm)) {
    xlab <- "Fitted Values"
  } else if (isTRUE(qqnorm)) {
    xlab <- "Theoretical Quantiles"
  }
  xaxis <- list(
    title          =  xlab,
    zeroline       = F,
    showgrid       = F,
    showline       = T,
    showticklabels = T,
    nticks         = 4,
    ticks          = "inside",
    tickformat     = ".1f",
    hoverformat    = ".1f"
  )
  return(xaxis)
}

resid_yaxis <- function(qqnorm = FALSE) {
  if (isFALSE(qqnorm)) {
    ylab <- "Fitted Values"
    yside <- "left"
  } else if (isTRUE(qqnorm)) {
    ylab <- "Theoretical Quantiles"
    yside <- "right"
  }
  yaxis <- list(
    title          = ylab,
    side           = yside,
    zeroline       = F,
    showgrid       = F,
    showline       = T,
    showticklabels = T,
    nticks         = 4,
    ticks          = "inside",
    tickformat     = ".1f",
    hoverformat    = ".1f"
  )
  return(yaxis)
}


dose_xaxis <- function(log = FALSE, dose_type) {
  if (dose_type == "molar" & isFALSE(log)) {
    list(
      title          = "Dose (M)",
      tickformat     = ".0e",
      hoverformat    = ".0e",
      ticks          = "inside",
      tickangle      = 30,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T
    )
    
  } else if (dose_type == "molar" & isTRUE(log)) {
    list(
      title          = "Log10 Dose (M)",
      type           = "log",
      exponentformat = "power",
      tickformat     = ".0e",
      hoverformat    = ".0e",
      tick0          = log10(1e-9),
      dtick          = 2,
      ticks          = "inside",
      tickangle      = 30,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T
    )
  } else if (dose_type == "mg/ml" & isFALSE(log)) {
    list(
      title          = "Dose (mg/ml)",
      type           = "linear",
      range          = list(0, 2.1),
      tickformat     = ".1f",
      hoverformat    = ".1f",
      tick0          = 0.2,
      dtick          = 0.5,
      ticks          = "inside",
      tickangle      = 30,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T
    )
  } else if (dose_type == "mg/ml" & isTRUE(log)) {
    list(
      title          = "Log10 Dose (mg/ml)",
      type           = "log",
      range          = list(log10(0), log10(2.1)),
      tickformat     = ".1f",
      hoverformat    = ".1f",
      tick0          = log10(0.2),
      dtick          = 0.5,
      ticks          = "inside",
      tickangle      = 30,
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T
    )
  }
}

response_yaxis <- function() {
  list(
    title          = "Response (% Baseline)",
    rangemode      = "nonnegative",
    zeroline       = F,
    showgrid       = F,
    showline       = T,
    showticklabels = T,
    ticks          = "inside",
    tickformat     = ".0%",
    hoverformat    = ".0%"
  )
}


resid_line <- function(qqnorm = FALSE) {
  if (isFALSE(qqnorm)) {
   line <- list(
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
  } else if (isTRUE(qqnorm)) {
    line <- NULL
  }
  return(line)
}


residual_layout <- function(plot, qqnorm = FALSE) {
  plot_out <- plot %>%
  layout(
    showlegend = FALSE,
    shapes       = resid_line(qqnorm),
    xaxis        = resid_xaxis(qqnorm),
    yaxis        = resid_yaxis(qqnorm)
  ) %>%
    config(responsive = TRUE)
  return(plot_out)
}


dose_response_subplot <- function(data, plot_function) {
  if (plot_function == "plot_curve") {
    plot_A     <- curve_loop(data)
    plot_B     <- curve_loop(data, log = TRUE)
    ylab       <- "Response (fold act)"
    shareY     <- TRUE
  } else if (plot_function == "plot_raw") {
    plot_A     <- raw_scatter_loop(data)
    plot_B     <- raw_scatter_loop(data, log = TRUE)
    ylab       <- "Response (fold act)"
    shareY     <- TRUE
  } else if (plot_function == "plot_resid_qq") {
    plot_A     <- resid_loop(data)
    plot_B     <- qq_loop(data)
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
  
  return(plot_out)
}

dose_response_card <- function(data, title_plot, plot_function, card_icon = FALSE) {
  
  if (isTRUE(card_icon)) {
    file <- paste0(str_to_lower(title_plot), "_", data$receptor[1])
    logo <- tagList(img(src = assay_img(file), style="width:10%; height:10%;"))
    header <- card_header(logo, title_plot)
  } else if (isFALSE(card_icon)) {
    header <- card_header(title_plot)
  }
  
  plot_out <- dose_response_subplot(data, plot_function)
  
  card <- card(
    header,
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

card_by_treatment <- function(list, treatment, plot_function) {
  plots_alpha <- imap(list[[1]], \(x, idx) dose_response_card(x, idx, plot_function, card_icon = TRUE))
  plots_beta  <- imap(list[[2]], \(x, idx) dose_response_card(x, idx, plot_function, card_icon = TRUE))
  
  layout_alpha <- do.call(bscols, c(list(widths = rep(4, length(plots_alpha))), plots_alpha))
  layout_beta <- do.call(bscols, c(list(widths = rep(4, length(plots_beta))), plots_beta))
  
  treatment_card <- card(
    card_header(str_to_title(treatment)),
    card_body(
      layout_column_wrap(
       width = 1, layout_alpha, layout_beta
      )
    )
  )
  return(treatment_card)
  
  
  return(layout_c)
}

panel_treat_class <- function(list, treat_class, plot_function) {
  treatments <- imap(list, \(x, idx) card_by_treatment(x, idx, plot_function))
  
  out <- nav_panel(
    title = str_to_title(treat_class),
    treatments
  )
  
  return(out)
}

navset_treat_class <- function(list, plot_function) {
  panels <- imap(list, \(x, idx) panel_treat_class(x, idx, plot_function))
  
  out <- navset_pill_list(!!!panels)
  
  return(out)
}

fitted_data <- function(drc_object) {
  map(drc_object, \(x) augment(
    x, x[["origData"]], conf.int = TRUE
    )) %>%
    map(\(x) map_qqnorm(x)) %>%
    imap(\(x, idx) mutate(x, id_curve = paste0(idx))) %>%
    list_rbind() %>%
    rename_with(~str_remove_all(., "\\.")) %>%
    select(
      id_curve,
      resid,
      theoretical,
      dose,
      response = fitted,
      sample,
      cooksd
      ) %>%
    mutate(value_type = "fitted")
}


predicted_data <- function(drc_object) {
 map(drc_object, \(x) predict(
    x, data.frame(
      dose = seq(
        min(x[["dataList"]][["dose"]]), 
        max(x[["dataList"]][["dose"]]), 
        length.out = 200)),
    interval = "confidence"
  )) %>%
    map2(drc_object, \(x, y) bind_cols(
      as_tibble(x), 
      tibble(
        dose = c(seq(
          min(y[["dataList"]][["dose"]]), 
          max(y[["dataList"]][["dose"]]), 
          length.out = 200))
        )
    )) %>%
    imap(\(x, idx) mutate(x, id_curve = paste0(idx))) %>%
    list_rbind() %>%
    rename(response       = Prediction,
           response_lower = Lower,
           response_upper = Upper) %>%
    select(
      id_curve,
      dose,
      response,
      response_lower,
      response_upper
    ) %>%
    mutate(value_type = "predicted")
}

resid_data <- function(drc_object) {
  fitted_data(drc_object) %>%
    select(
      id_curve,
      dose,
      resid,
      response,
      theoretical,
      sample
    )
}

curve_data <- function(drc_object) {
  fitted <- fitted_data(drc_object) %>%
    select(
      id_curve,
      dose,
      response,
      theoretical,
      sample,
      value_type
    )
  data <- predicted_data(drc_object) %>%
    bind_rows(fitted) %>%
    arrange(id_curve, dose, value_type)
  return(data)
}

dashboard_treat_class <- function(list_raw, list_fitted, list_predicted) {
  navset_raw   <- navset_treat_class(list_raw, "plot_raw")
  navset_resid <- navset_treat_class(list_fitted, "plot_resid_qq")
  navset_curve <- navset_treat_class(list_predicted, "plot_curve")
  
  dashboard <- navset_tab(
    nav_panel("Raw Dose-Response"   , navset_raw),
    nav_panel("Residuals"           , navset_resid),
    nav_panel("Dose-Response Curves", navset_curve)
  )
  
  return(dashboard)
}



coef_plot_tbl <- function(data, x_type) {
  data %>%
    left_join(meta_pooled, by = "group_pooled") %>%
    select(-c(color_key, group_pooled)) %>%
    mutate(subject   = factor(as.character(str_extract_all(receptor, "^\\w+(?=_\\w+$)")), levels = c("loris", "gorilla", "human")),
           dose_type = factor(dose_type, levels = c("molar", "mg/ml")),
           receptor  = factor(as.character(str_remove_all(receptor, "\\w+_(?=\\w)")), levels = c("alpha", "beta"))) %>%
    arrange(receptor, desc(dose_type), desc(treatment), desc(subject)) %>%
    mutate(dummy_y = row_number(), 
           adjust_y = (consecutive_id(treatment) - 1)*3,
           .by = c("groups_treatment", "receptor")) %>%
    rowwise() %>%
    mutate(dummy_y = sum(dummy_y, adjust_y), .keep = "unused") %>%
    ungroup() %>%
    arrange(receptor, dose_type, treatment, subject) %>%
    mutate(across(where(is.factor), ~as.character(.))) %>%
    mutate(ticklab_y = if_else(
      row_number() == 1, 
      as.character(str_glue("{labs_treatment} {str_to_title(subject)}")),
      as.character(str_to_title(subject))
    ), .by = c("receptor", "treatment")) %>%
    mutate(
      title = labs_er,
      hover = as.character(str_glue(
        "{labs_treatment}: {str_to_title(subject)}"
      ))
    ) %>%
    nest(.by = c("groups_treatment", "labs_er")) %>%
    mutate(shapes   = map(data, \(x) db_shapes(x))) %>%
    mutate(xaxis    = map(data, \(x) db_xaxis(x, x_type))) %>%
    mutate(annotate = map(data, \(x) db_annotate(x))) %>%
    nest(.by = c("groups_treatment", "labs_er")) %>%
    pivot_wider(names_from = "labs_er", values_from = "data") %>%
    mutate(across(!groups_treatment, ~map(., \(x) as.list(x)))) %>%
    group_by(groups_treatment) %>%
    group_split() %>%
    set_names(., map(., \(x) paste0(x$groups_treatment[1]))) %>%
    map_depth(1, \(x) select(x, -groups_treatment)) %>%
    map_depth(1, \(x) as.list(x)) %>%
    map_depth(1, \(x) list_flatten(x)) %>%
    map_depth(2, \(x) list_flatten(x))
}

list_by_treatment <- function(df) {
  df %>%
    mutate(hover = as.character(str_glue(
      "{labs_plate}<br>{labs_treatment}"
    ))) %>%
    mutate(
      subject = as.character(str_to_title(str_remove_all(
        receptor, "(?<=\\w)_\\w+$"
      ))),
      receptor = as.character(str_extract_all(
        receptor, "(?<=\\w_)\\w+$"
      )),
      class_treatment = as.character(str_remove_all(
        treatment, "(?<=\\w)_\\w+$"
      ))
    ) %>%
    mutate(
      treatment = as.character(str_extract_all(
        treatment, "(?<=\\w_).+$"
      ))
    ) %>%
    mutate(
      subject = factor(
        subject, levels = c("Loris", "Gorilla", "Human"), ordered = TRUE
      ),
      class_treatment = factor(
        class_treatment, levels = c("estrogen", "metabolite", "biscuit"),
        ordered = TRUE
      )
    ) %>%
    arrange(receptor, subject, class_treatment, treatment) %>%
    group_by(class_treatment) %>%
    group_split() %>%
    set_names(., map(., \(x) paste0(first(x$class_treatment)))) %>%
    map_depth(1, \(x) nest(x, .by = "treatment")) %>%
    map_depth(1, \(x) deframe(x)) %>%
    map_depth(2, \(x) nest(x, .by = "labs_er")) %>%
    map_depth(2, \(x) deframe(x)) %>%
    map_depth(3, \(x) nest(x, .by = "subject")) %>%
    map_depth(3, \(x) deframe(x))
}


db_annotate <- function(data) {
  list(
    text      = paste0(data$title[1]),
    x         = 0.5,
    y         = 1.01,
    xref      = "paper",
    yref      = "paper",
    xanchor   = "center",
    yanchor   = "bottom",
    showarrow = F
  )
}

db_xaxis <- function(data, x_type) {
  if (x_type == "ED" & data$dose_type[1] == "mg/ml") {
    list(
      title          = "Effective Dose (mg/ml)",
      tickformat     = ".0f",
      hoverformat    = ".0f",
      autorange      = "reversed",
      zeroline       = F,
      mirror         = T,
      showgrid       = F,
      showline       = T,
      ticks          = "inside"
    )
  } else if (x_type == "ED" & data$dose_type[1] == "molar") {
    list(
      title          = "Effective Dose (M)",
      tickformat     = ".0e",
      hoverformat    = ".0e",
      exponentformat = "power",
      autorange      = "reversed",
      zeroline       = F,
      mirror         = T,
      showgrid       = F,
      showline       = T,
      ticks          = "inside"
    )
  } else if (x_type == "slope") {
    list(
      title          = "Hill Slope",
      tickformat     = ".2f",
      hoverformat    = ".2f",
      autorange      = F,
      zeroline       = F,
      mirror         = T,
      showgrid       = F,
      showline       = T,
      ticks          = "inside",
      range          = list(15, -15)
    )
  }
}


db_shapes <- function(data) {
  filter(data, subject == "loris") %>%
    select(labs_treatment, dummy_y) %>%
    rowwise() %>%
    mutate(y_line = dummy_y + 2, .keep = "unused") %>%
    group_split() %>%
    map(\(x) list(
      type       = "line",
      line       = list(
        color   = "#00000033",
        width   = 4
      ),
      showlegend = F,
      x0         = 0,
      x1         = 1,
      xref       = "paper",
      y0         = x$y_line, 
      y1         = x$y_line, 
      name       = x$labs_treatment,
      yref       = "y"
    ))
}

db_ed_markers <- function(plot, data, x) {
  colors  <- c(loris = "#882255FF", gorilla = "#332288FF", human = "#117733FF")
  if (x == ~ED25) {
    xlab   <- "ED25"
    marker <- list(
      opacity = 0.8, 
      size    = 7, 
      symbol  = "circle-dot",
      line    = list(color = "#00000FFF", width = 1)
    )
  } else if (x == ~ED75) {
    xlab   <- "ED75"
    marker <- list(
      opacity = 0.8, 
      size    = 7, 
      symbol  = "circle-dot",
      line    = list(color = "#00000FFF", width = 1)
    )
  } else if (x == ~ED50) {
    xlab   <- "ED50"
    marker <- list(
      opacity = 0.8, 
      size    = 7, 
      symbol  = "star-diamond-dot",
      line    = list(color = "#00000FFF", width = 1)
    )
  }
  
  if (data$dose_type[1] == "mg/ml") {
    hovertemp <- paste0("%{text}<br>", xlab, ": %{x:.0f} mg/ml")
  } else if (data$dose_type[1] == "molar") {
    hovertemp <- paste0("%{text}<br>", xlab, ": %{x:.0e} M")
  }
  out <- plot %>%
    add_markers(
    data          = data,
    x             = x,
    y             = ~dummy_y,
    name          = ~str_to_title(subject),
    text          = ~hover,
    split         = ~subject,
    color         = ~subject,
    colors        = colors,
    marker        = marker,
    showlegend    = FALSE,
    hovertemplate = hovertemp
  )
  return(out)
}

slope_markers <- function(plot, data, x) {
  colors  <- c(loris = "#882255FF", gorilla = "#332288FF", human = "#117733FF")
  if (x == ~slope_lower) {
    xlab   <- "95% Lower Limit"
    marker <- list(
      opacity = 0.8, 
      size    = 6, 
      symbol  = "line-ns",
      line    = list(color = "#00000FFF", width = 1)
    )
  } else if (x == ~slope_upper) {
    xlab   <- "95% Upper Limit"
    marker <- list(
      opacity = 0.8, 
      size    = 6, 
      symbol  = "line-ns",
      line    = list(color = "#00000FFF", width = 1)
    )
  } else if (x == ~slope) {
    xlab   <- "Hill Slope"
    marker <- list(
      opacity = 0.8, 
      size    = 8, 
      symbol  = "circle-dot",
      line    = list(color = "#00000FFF", width = 1)
    )
  }
  out <- plot %>%
    add_markers(
      data          = data,
      x             = x,
      y             = ~dummy_y,
      name          = ~str_to_title(subject),
      text          = ~hover,
      split         = ~subject,
      color         = ~subject,
      colors        = colors,
      marker        = marker,
      showlegend    = FALSE,
      hovertemplate = paste0("%{text}<br>", xlab, ": %{x:.2f}")
    )
  return(out)
}



db_layout <- function(plot, data) {
  plot %>%
  layout(
    shapes      = data$shapes,
    xaxis       = data$xaxis,
    annotations = data$annotate,
    yaxis       = list(
      title          = "Treatment",
      side           = "left",
      zeroline       = F,
      showgrid       = T,
      gridcolor      = "#eeeeee",
      showline       = T,
      mirror         = T,
      ticktext       = as.list(pull(data$data, ticklab_y)),
      tickvals       = as.list(pull(data$data, dummy_y))
    ),
    margin = list(l = 10, r = 50, t = 50, b = 80)
  ) %>%
    config(
      displaylogo = F,
      modeBarButtonsToRemove = c(
        "autoScale2d", 
        "autoscale", 
        "hovercompare", 
        "lasso",
        "zoomIn",
        "zoomOut",
        "resetScal",
        "drawline",
        "drawopenpath",
        "drawclosedpath",
        "drawcircle",
        "drawrect",
        "eraseshap",
        "hoverClosestGl2d",
        "hoverClosestPie",
        "toggleHover",
        "resetViews",
        "toImage",
        "sendDataToCloud",
        "toggleSpikelines"
      )
    )
}

edx_plot <- function(data) {
  colors  <- c(loris = "#882255FF", gorilla = "#332288FF", human = "#117733FF")
  plot <- plot_ly() %>%
    add_segments(
      data       = data$data,
      x          = ~ED25,
      xend       = ~ED75,
      y          = ~dummy_y,
      yend       = ~dummy_y,
      text       = ~hover,
      split      = ~subject,
      color      = ~subject,
      colors     = colors,
      line       = list(width = 1.5),
      showlegend = FALSE
    )  %>%
    db_ed_markers(data = data$data, x = ~ED25)  %>%
    db_ed_markers(data = data$data, x = ~ED50)  %>%
    db_ed_markers(data = data$data, x = ~ED75) %>%
    db_layout(data = data)
  
  return(plot)
}

slope_plot <- function(data) {
  colors  <- c(loris = "#882255FF", gorilla = "#332288FF", human = "#117733FF")
  plot <- plot_ly() %>%
    add_segments(
      data       = data$data,
      x          = ~slope_lower,
      xend       = ~slope_upper,
      y          = ~dummy_y,
      yend       = ~dummy_y,
      text       = ~hover,
      split      = ~subject,
      color      = ~subject,
      colors     = colors,
      opacity    = 0.7,
      line       = list(width = 1),
      showlegend = FALSE
    )  %>%
    slope_markers(data = data$data, x = ~slope_lower)  %>%
    slope_markers(data = data$data, x = ~slope_upper)  %>%
    slope_markers(data = data$data, x = ~slope)  %>%
    db_layout(data = data)
  
  return(plot)
}


db_layout_receptors <- function(list_receptors, groups_treatment, plot_function) {
  if (plot_function == "edx_plot") {
    plots    <- map(list_receptors, \(x) edx_plot(x))
  } else if (plot_function == "slope_plot") {
    plots    <- map(list_receptors, \(x) slope_plot(x))
  }
  subplots <- subplot(
    plots,
    nrows = 1,
    shareX = F,
    shareY = F,
    titleX = T,
    titleY = F,
    margin = 0.1
  ) %>%
    layout(
      margin     = list(l = 90, r = 40, t = 25, b = 30),
      showlegend = T,
      legend     = list(
        x       = -3, 
        y       = 0.5,
        xref    = "paper",
        yref    = "paper",
        xanchor = "right",
        yanchor = "center"
        )
    ) %>%
    config(responsive = TRUE)
  
  card <- card(full_screen = TRUE, 
               card_header(paste0(groups_treatment)),
               card_body(subplots))
  return(card)
}

db_layout_cards <- function(list, plot_function) {
  cards <- imap(list, \(x, idx) db_layout_receptors(x, idx, plot_function = plot_function))

  layout <- card(
    layout_column_wrap(
      width  = 1/2,
      height = 800,
      cards[["Mazuri Biscuit Extract"]],
      layout_column_wrap(
        width         = 1,
        height        = 700,
        cards[["Metabolite"]],
        layout_column_wrap(
          width         = 1,
          height        = 100,
          cards[["Estrogen"]]
        )
      )
    )
  )
  
  return(layout)
}

coef_navset <- function(slopes, edx) {
  out <- navset_tab(
    nav_panel(title = "Effective Doses", edx),
    nav_panel(title = "Hill Slope", slopes)
  )
  return(out)
}
