dr_trace_raw <- function(plot, data, log = FALSE) {
  if (isTRUE(log)) { 
    x <- data$log10_dose
    col_pal <- dose_pal(data, "log10_dose")
  } else if (isFALSE(log)) {
    x <- data$dose
    col_pal <- dose_pal(data, "dose")
  }
  add_trace(p             = plot,
            data          = data,
            x             = x,
            y             = ~fold_act,
            type          = "scatter",
            mode          = "markers",
            color         = x,
            colors        = col_pal,
            marker        = list(size       = 8,
                                 opacity    = 0.8,
                                 line  = list(
                                   color = "#000000FF", 
                                   width = 1)),
            showlegend    = FALSE,
            hovertemplate = dr_hover()
  )
}

residuals_trace <- function(plot, data) {
  col_pal <- dose_pal(data, "log10_dose")
  add_trace(p             = plot,
            data          = data,
            x             = ~.fitted,
            y             = ~.resid,
            text          = ~log10_dose,
            type          = "scatter",
            mode          = "markers",
            color         = ~log10_dose,
            colors        = col_pal,
            marker        = list(size       = 8,
                                 opacity    = 0.8,
                                 line  = list(
                                   color = "#000000FF", 
                                   width = 1)),
            showlegend    = FALSE,
            hovertemplate = paste0("fitted = %{x:.2f}<br>resid = %{y:.2f}<br>log10(dose) = %{text: .2f}")
  )
}

qq_trace <- function(plot, data) {
  col_pal <- dose_pal(data, "log10_dose")
  qq_start <- min(pull(data, theoretical))
  qq_end   <- max(pull(data, theoretical))
  add_trace(p             = plot,
            data          = data,
            x             = ~theoretical,
            y             = ~sample,
            text          = ~log10_dose,
            type          = "scatter",
            mode          = "markers",
            color         = ~log10_dose,
            colors        = col_pal,
            marker        = list(size       = 7,
                                 opacity    = 0.8,
                                 line  = list(
                                   color = "#000000FF", 
                                   width = 1)),
            showlegend    = FALSE,
            hovertemplate = paste0("theoretical = %{x:.2f}<br>sample = %{y:.2f}<br>log10(dose) = %{text: .2f}")
  ) %>%
    add_segments(
      x    = qq_start,
      y    = qq_start,
      xend = qq_end,
      yend = qq_end,
      data = data,
      line = list(color = "#000000A6", 
                  width = 3),
      showlegend = FALSE
    )
}

dr_conf_bands <- function(plot, data) {
  add_ribbons(
    p             = plot,
    data          = data,
    x             = ~log10_dose,
    ymin          = ~lower,
    ymax          = ~upper,
    fillcolor     = "#09090C1A",
    line          = list(color = "#09090C1A", 
                         width = 0.1,
                         shape = "spline"),
    labels        = "95% conf for predicted values"
  )
}

dr_trace_pred <- function(plot, data) {
  add_trace(p             = plot,
            data          = data,
            x             = ~log10_dose,
            y             = ~resp,
            type          = "scatter",
            mode          = "lines",
            line          = list(color = "#4B1C57FF", 
                                 width = 3,
                                 shape = "spline"),
            showlegend    = FALSE,
            hovertemplate = dr_hover()
  )   %>% 
    dr_conf_bands(data)
}

dr_trace_real <- function(plot, data) {
  add_trace(p             = plot,
            data          = data,
            x             = ~log10_dose,
            y             = ~resp,
            type          = "scatter",
            mode          = "markers",
            marker        = list(size  = 7,
                                 color = "#BDA14DFF",
                                 line  = list(
                                   color = "#153460FF", 
                                   width = 1)),
            showlegend    = FALSE,
            hovertemplate = dr_hover()
  )
}

annotate_plots <- function(plot, title, stats) {
  x <- 0.5
  y <- 1
  text <- paste0("<b>", title, "</b><br><i>", stats, "</i>")
  out <-  add_annotations(
    p         = plot,
    x         = x,
    y         = y,
    text      = text,
    hovertext = text,
    hoverlabel = list(
      font = list(size = 15)
    ),
    xref      = "paper",
    yref      = "paper",
    showarrow = FALSE,
    xanchor   = "center",  
    yanchor   = "top",
    font      = list(size = 8),
    layer       = "below",
    bgcolor     = "#ffffff"
  )
  return(out)
}
