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
      x         = -0.1,
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
  title <- unique(pull(data, "treat_type")) %>% str_to_title()
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
    annotate_subplots(title) %>%
    layout(
      showlegend = FALSE,
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

dr_model_cards <- function(x, idx) {
  header <- card_image(src = assay_img(idx), width = "10%")
  layout <- do.call(bscols, c(list(widths = rep(6, length(x))), x))
  out <- card(
    full_screen = TRUE,
    card_header(header),
    card_body(
      fillable = FALSE, 
      fill     = TRUE,
      width    = "100%",
      layout
    )
  )
  return(out)
}

