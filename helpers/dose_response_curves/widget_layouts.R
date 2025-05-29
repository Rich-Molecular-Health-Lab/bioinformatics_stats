
dr_subplot_card <- function(x, title, version = NULL) {
  if (length(x) < 4) { 
    ncols <- 2
  } else if (length(x) >= 4) { 
    ncols <- 3
  }
  if (version == "residuals") {
    xlab <- "Fitted"
    ylab <- "Residuals"
  } else if (version == "qqnorm") {
    xlab <- "Theoretical"
    ylab <- "Sample"
  } else if (version == "curve" | version == "raw_log") {
    xlab <- "Dose (log10)"
    ylab <- "Response (fold_act)"
  } else if (version == "raw") {
    xlab <- "Dose"
    ylab <- "Response (fold_act)"
  }
  plots <-  subplot(
    x,
    nrows    = ceiling(length(x)/ncols),
    shareX   = FALSE,
    shareY   = FALSE,
    titleX   = FALSE,
    titleY   = FALSE,
    margin   = c(0.01, 0.09, 0.01, 0.09)
  ) %>%
    add_annotations(
      x         = 0.5,
      y         = -0.01,
      text      = xlab,
      xref      = "paper",
      yref      = "paper",
      showarrow = FALSE,
      xanchor   = "center",  
      yanchor   = "top",
      font      = list(size = 12)
    ) %>%
    add_annotations(
      x         = -0.05,
      y         = 0.5,
      text      = ylab,
      xref      = "paper",
      yref      = "paper",
      showarrow = FALSE,
      xanchor   = "right",  
      yanchor   = "center",
      font      = list(size = 12),
      textangle = -90
    )
  card_out <- card(
    full_screen = TRUE,
    card_header(paste0(title)),
    card_body(
      fillable = FALSE, 
      fill     = TRUE,
      width    = "100%",
      plots
    )
  )
  return(card_out)
}

grid_panel <- function(x, idx, type = NULL) {
  if (startsWith(idx, "gorilla")) {
    genus <- "Gorilla"
  } else if (startsWith(idx, "loris")) {
    genus <- "Xanthonycticebus"
  } else if (startsWith(idx, "human")) {
    genus <- "Homo"
  }
  if (endsWith(idx, "alpha")) {
    receptor <- "\u03B1"
  } else if (endsWith(idx, "beta")) {
    receptor <- "\u03B2"
  }
  title <- paste0("ER ", receptor, " <em>", genus, "</em>")
  header <- card_image(src = assay_img(idx), width = "10%")
  layout <- do.call(bscols, c(list(widths = rep(6, length(x))), x))
  
  if (type == "accordion") {
    out <- accordion_panel(HTML(title), layout, icon = logo_tag(idx))
  } else if (startsWith(type, "nav") | is.null(type)) {
    out    <- nav_panel(HTML(title), layout, icon = logo_tag(idx))
  }
  
  return(out)
}

