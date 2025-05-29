dose_response_layout <- function(plot, log = FALSE) {
  if (isTRUE(log)) { 
    xlab <- "Log10 Dose"
    xtype <- "log"
  } else if (isFALSE(log)) { 
    xlab <- "Dose"
    xtype <- "linear"
  }
  add_annotations(
    p              = plot,
    text           = xlab,
    x              = 0.5,
    y              = -0.05,
    xref           = "paper",
    yref           = "paper",
    xanchor        = "center",
    yanchor        = "top",
    font           = list(size = 9),
    showarrow      = FALSE
  ) %>%
    layout(
      p            = plot,
      showlegend   = FALSE,
      xaxis        = list(
        title          = xlab,
        type           = xtype,
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = T,
        nticks         = 3,
        ticks          = "inside",
        tickfont       = list(size = 8),
        tickformat     = ".1e",
        hoverformat    = ".1e"
        
      ),
      yaxis        = list(
        title          = "Response",
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = T,
        nticks         = 3,
        ticks          = "inside",
        tickfont       = list(size = 8),
        ticksuffix     = "%",
        hoverformat    = ".2f"
      ))
}

dose_response_card <- function(list_traces, title_plot, plot_function) {
  
  if (plot_function == "plot_curve") {
    plot     <- curve_loop(list_traces)
    plot_log <- curve_loop(list_traces, log = TRUE)
  } else if (plot_function == "plot_raw") {
    plot     <- raw_scatter_loop(list_traces)
    plot_log <- raw_scatter_loop(list_traces, log = TRUE)
  } 
  
  plot_out <- subplot(
    plot,
    plot_log,
    nrows = 1,
    shareX = FALSE,
    shareY = FALSE,
    margin = 0.02
  ) %>%
    add_annotations(
      text         = "Fold Act",
      x            = -0.05,
      y            = 0.5,
      xref         = "paper",
      yref         = "paper",
      xanchor      = "right",
      yanchor      = "center",
      font         = list(size = 9),
      showarrow    = FALSE,
      textangle    = -90
    ) %>%
    layout(margin = list(t = 10, b = 35, l = 30, r = 15))
  
  
  card <- card(
    card_header(title_plot),
    card_body(plot_out, padding = 0),
    full_screen = TRUE,
    fill        = FALSE,
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



