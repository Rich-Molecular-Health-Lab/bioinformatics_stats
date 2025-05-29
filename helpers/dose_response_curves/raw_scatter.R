raw_scatter_trace <- function(plot, data) {
  colors  <- unlist(data[["color_key"]][1])
    add_trace(
      p          = plot,
      data       = data,
      x          = ~dose,
      y          = ~fold_act,
      text       = ~hover,
      name       = ~labs_treatment,
      type       = "scatter",
      mode       = "markers",
      color      = ~labs_plate,
      colors     = colors,
      marker     = list(size       = 8,
                        opacity    = 0.6,
                        line  = list(color = "#000000", width = 1)
      ),
      showlegend = FALSE
    )
}

raw_scatter_loop <- function(list_traces, log = FALSE) {
  p   <- plot_ly()
  for(i in 1:length(list_traces)) {
    data   <- pluck(list_traces, i)
    p      <- raw_scatter_trace(p, data)
  }
  out <- dose_response_layout(p, log)
  return(out)
}



