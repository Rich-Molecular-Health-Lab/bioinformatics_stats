dr_grid <- function(data_list, cols = 3) {
  width <- floor(12/cols)
  cards <- map(data_list, \(x) dr_card(x)) %>%
    tagList() %>%
    list_flatten()
  layout <- do.call(bscols, c(list(widths = rep(width, length(cards))), cards))
  return(layout)
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


add_trace(
  data      = data,
  x         = ~labs_treatment,
  y         = ~Dmax_treatment_plate,
  color     = ~plate,
  text      = ~paste0("Plate: ", plate, "(", date, ")"),
  label     = ~plate,
  colors    = col.pal(data, "plate"),
  type      = "scatter",
  mode      = "markers",
  marker    = list(
    size    = 6,
    opacity = 0.4,
    line    = list(color = "#000000FF", width = 0.5)
  ),
) %>%