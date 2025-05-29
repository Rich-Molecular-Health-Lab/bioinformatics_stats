curve_trace <- function(plot, data) {
  colors  <- unlist(data[["color_key"]][1])
  trace <- add_trace(
    p          = plot,
    data       = filter(data, value_type == "predicted"),
    x          = ~dose,
    y          = ~fold_act,
    text       = ~hover,
    name       = ~labs_treatment,
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
    ymin       = ~fold_act_lower,
    ymax       = ~fold_act_upper,
    text       = ~hover,
    name       = "95% conf for predicted values",
    color      = ~labs_plate,
    colors     = colors,
    zorder     = 0,
    opacity    = 0.3,
    line       = list(width = 0.1, shape = "spline"),
    showlegend = FALSE
  ) %>%
    add_trace(
      data       = filter(data, value_type == "fitted"),
      x          = ~dose,
      y          = ~fold_act,
      text       = ~hover,
      name       = ~labs_treatment,
      zorder     = 2,
      type       = "scatter",
      mode       = "markers",
      color      = ~labs_plate,
      colors     = colors,
      marker     = list(size       = 7,
                        opacity    = 0.3,
                        line  = list(
                          color = "#000000",
                          width = 1)
      ),
      showlegend = FALSE
    )
  return(trace)
}

curve_loop <- function(list_traces, log = FALSE) {
  p   <- plot_ly()
  for(i in 1:length(list_traces)) {
    data   <- pluck(list_traces, i)
    p      <- curve_trace(p, data)
  }
  
  out <- dose_response_layout(p, log)
  return(out)
}
