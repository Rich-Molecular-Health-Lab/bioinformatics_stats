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


color_col <- function(df, column, palette = NULL) {
  if (is.null(palette)) { palette <-  "unikn::pal_unikn_pref"}
  
  values <- unique(df[[paste0(column)]])
  n_colors <- length(values)
  
  color <-  sample(as.character(paletteer_d(palette)), n_colors, replace = TRUE) %>%
    set_names(., values) %>%
    enframe(name = paste0(column), value = "color") %>%
    mutate(across(where(is.character), ~factor(.)))
  
  result <- left_join(df, color, by = column) %>%
    mutate(color = as.character(color))
  
  return(result)
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


res_trace <- function(plot, data) {
  colors  <- unlist(data[["color_key"]][1])
  add_trace(
    p          = plot,
    data       = data,
    x          = ~fitted,
    y          = ~resid,
    text       = ~hover,
    name       = ~labs_treatment,
    type       = "scatter",
    mode       = "markers",
    color      = ~labs_plate,
    colors     = colors,
    marker     = dose_response_markers(),
    showlegend = FALSE,
    hovertemplate = "x: %{x:.1f}, y: %{y:.1f}<br>%{text}"
  )
}

resid_layout <- function(plot) {
  layout(
    p    = plot,
    showlegend = FALSE,
    shapes     = list(
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
    ),
    xaxis        = list(
      title          = "Fitted",
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T,
      nticks         = 3,
      ticks          = "inside",
      tickfont       = list(size = 8),
      hoverformat    = ".2f"
    ),
    yaxis        = list(
      title          = "Residual",
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T,
      nticks         = 3,
      ticks          = "inside",
      tickfont       = list(size = 8),
      hoverformat    = ".2f"
    ))
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
    labels     = ~hover_line,
    color      = ~labs_plate,
    colors     = colors,
    marker     = list(size       = 6,
                      opacity    = 0.8,
                      line  = list(
                        color = "#000000",
                        width = 1)
    ),
    showlegend = FALSE,
    hovertemplate = "x: %{x:.1f}, y: %{y:.1f}<br>%{text}"
  ) %>%
    add_segments(
      data       = data,
      text       = ~hover_line,
      labels     = ~hover_line,
      x          = x0, 
      xend       = x1,
      y          = y0, 
      yend       = y1,
      color      = ~labs_plate,
      colors     = colors,
      line       = list(width = 2),
      showlegend = FALSE
    )
  return(plot)
}



qq_layout <- function(plot) {
  layout(
    p          = plot,
    showlegend = FALSE,
    xaxis        = list(
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T,
      nticks         = 3,
      ticks          = "inside",
      tickfont       = list(size = 8),
      hoverformat    = ".2f"
    ),
    yaxis        = list(
      zeroline       = F,
      showgrid       = F,
      showline       = T,
      showticklabels = T,
      nticks         = 3,
      ticks          = "inside",
      tickfont       = list(size = 8),
      hoverformat    = ".2f"
    ),
    margin = list(t = 10, b = 10, l = 20, r = 10)
  )
}


resid_loop <- function(list_traces) {
  p   <- plot_ly()
  for(i in 1:length(list_traces)) {
    data   <- pluck(list_traces, i)
    p      <- res_trace(p, data)
  }
  out <- resid_layout(p)
  return(out)
}

qq_loop <- function(list_traces) {
  p   <- plot_ly()
  for(i in 1:length(list_traces)) {
    data   <- pluck(list_traces, i)
    p      <- qq_trace(p, data)
  }
  out <- qq_layout(p)
  return(out)
}

plot_resid_qq <- function(list_traces, title_plot) {
  plot_resid <- resid_loop(list_traces)
  plot_qq    <- qq_loop(list_traces)
  
  plot_out <- subplot(
    plot_resid,
    plot_qq,
    nrows = 1,
    shareX = FALSE,
    shareY = FALSE,
    margin = 0.02
  ) %>%
    layout(margin = list(t = 10, b = 20, l = 5, r = 10))
  
  card <- card(
    card_header(title_plot),
    card_body(plot_out, padding = 0),
    full_screen = TRUE,
    fill        = FALSE,
    max_height  = "190px"
  )
  return(card)
}

grid_resid_qq <- function(list_plots, receptor) {
  plots <- imap(list_plots, \(x, idx) plot_resid_qq(x, idx))
  
  layout <- do.call(bscols, c(list(widths = rep(3, length(plots))), plots))
  
  logo_icon <- tagList(img(src = assay_img(receptor), style="width:15%; height:15%;"))
  
  if (str_ends(receptor, "alpha")) { er <-  "ER \u03B1"} else { er <- "ER \u03B2" }
  
  species <- str_to_title(str_extract(receptor, "^\\w+(?=_)"))
  title   <- paste(er, species)
  out     <- nav_panel(HTML(title), layout, icon = logo_icon)
  
  return(out)
}

