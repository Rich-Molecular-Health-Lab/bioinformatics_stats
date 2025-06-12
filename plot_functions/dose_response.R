
scatter_mark <- function(size = 9, opacity = 0.7) {
  list(size    = size,
       opacity = opacity,
       line    = list(color = "#000000", width = 1)
  )
}

color_by_plate <- function(data) {
  plates <- unique(pull(data, plate_id))
  colors <- paletteer_c("pals::ocean.phase", n = length(plates)) %>%
    as.character() %>%
    set_names(., plates)
  return(colors)
}

response_yaxis <- function(log = FALSE) {
  if (isFALSE(log)) {
    labs <- TRUE
    title <- "Response (% Baseline)"
  } else if (isTRUE(log)) {
    labs <-FALSE
    title <- ""
  }
  list(
    title          = title,
    rangemode      = "nonnegative",
    zeroline       = F,
    showgrid       = F,
    showline       = T,
    showticklabels = labs,
    ticks          = "inside",
    tickformat     = ".0%",
    hoverformat    = ".0%",
    automargin     = TRUE
  )
}

dose_xaxis <- function(data, log = FALSE) {
  dose_type <- data$dose_type[1]
  if (dose_type == "mg/ml" & isFALSE(log)) {
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
  } else if (dose_type == "molar" & isFALSE(log)) {
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
  }
}

dose_response_layout  <- function(plot, data, log = FALSE) {
  plot %>%
    layout(
      xaxis = dose_xaxis(data, log),
      yaxis = response_yaxis(log)
    ) %>%
    hide_legend() %>%
    config(responsive = TRUE)
}

dose_response_scatter <- function(data, colors) {
  title <- paste(str_to_title(data$subject[1]))
  file  <- paste0(data$subject[1], "_", data$receptor[1])
  logo  <- tagList(card_image(src = assay_img(file), container = card_header(), width = "5%"))
  plot_linear <- plot_ly(data = data) %>%
    add_trace(
      x      = ~dose,
      y      = ~response,
      split  = ~plate_id,
      color  = ~plate_id,
      colors = colors,
      text   = ~date,
      name   = ~plate_id,
      type   = "scatter",
      mode   = "markers",
      marker = scatter_mark()
    ) %>%
    dose_response_layout(data)

  plot_log <- plot_ly(data = data) %>%
    add_trace(
      x      = ~dose,
      y      = ~response,
      split  = ~plate_id,
      color  = ~plate_id,
      colors = colors,
      text   = ~date,
      name   = ~plate_id,
      type   = "scatter",
      mode   = "markers",
      marker = scatter_mark()
    ) %>%
    dose_response_layout(data, log = TRUE)
  
  plotx2 <- subplot(
    nrows = 1,
    plot_linear,
    plot_log,
    shareX = FALSE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE
  )

  card <- card(
    card_header(logo, title),
    card_body(plotx2, padding = 0),
    full_screen = TRUE,
    fill = FALSE
  )
  
  return(card)
}

treatment_layout <- function(data, colors) {
  title <- paste0(str_to_title(data$treatment[1]))
  data_list <- data %>%
    mutate(across(where(is.factor), ~as.character(.))) %>%
    group_by(subject) %>%
    group_split()
  
  ncols <- n_distinct(data$subject)
  
  cards <- map(
    data_list, \(x) tagList(dose_response_scatter(x, colors))
    )
  
  panel <- accordion_panel(
    title = title,
      layout_column_wrap(
        !!!cards,
        fill = FALSE
        )
  )
  
  return(panel)
}

treatment_class_layout <- function(data) {
  colors <- color_by_plate(data)
  treatment_class <- data$treatment_class[1]
  data_list <- data %>%
    mutate(across(where(is.factor), ~as.character(.))) %>%
    group_by(treatment) %>%
    group_split()
  panels <- map(
    data_list, \(x) tagList(treatment_layout(x, colors))
    )
  navpanel <- nav_panel(
    paste0(str_to_title(treatment_class)),
      accordion(open = TRUE, !!!panels)
    )
  return(navpanel)
}

receptor_tabset <- function(data) {
  alpha_list <- filter(data, receptor == "alpha") %>%
    mutate(across(where(is.factor), ~as.character(.))) %>%
    group_by(treatment_class) %>%
    group_split()
  beta_list <- filter(data, receptor == "beta") %>%
    mutate(across(where(is.factor), ~as.character(.))) %>%
    group_by(treatment_class) %>%
    group_split()
  alpha_panels <- map(
    alpha_list, \(x) treatment_class_layout(x)
    ) %>%
    set_names(., "")
  
  alpha_navset <- navset_pill_list(
    !!!alpha_panels,
    widths = c(2, 10)
    )
  
  beta_panels <- map(
    beta_list, \(x) treatment_class_layout(x)
  ) %>%
    set_names(., "")
  
  beta_navset <- navset_pill_list(
    !!!beta_panels,
    widths = c(1, 11)
    )
  
  tabset <- navset_tab(
    nav_panel("ER \u03B1", alpha_navset),
    nav_panel("ER \u03B2", beta_navset)
  )
  return(tabset)
}

alpha_list <- data_plot %>%
  mutate(across(where(is.factor), ~as.character(.))) %>%
  filter(receptor == "alpha" & treatment == "estradiol") %>%
  group_by(treatment_class) %>%
  group_split()

single_treatment_tabset <- function(data, filter) {

  alpha_list <- data %>%
    mutate(across(where(is.factor), ~as.character(.))) %>%
    filter(receptor == "alpha" & treatment %in% c(filter)) %>%
    group_by(treatment_class) %>%
    group_split()
  beta_list <- data %>%
    mutate(across(where(is.factor), ~as.character(.))) %>%
    filter(receptor == "beta" & treatment %in% c(filter)) %>%
    group_by(treatment_class) %>%
    group_split()
  alpha_panels <- map(
    alpha_list, \(x) treatment_class_layout(x)
  ) %>%
    set_names(., "")
  
  beta_panels <- map(
    beta_list, \(x) treatment_class_layout(x)
  ) %>%
    set_names(., "")
  
  tabset <- navset_tab(
    nav_panel("ER \u03B1", !!!alpha_panels),
    nav_panel("ER \u03B2", !!!beta_panels)
  )
  return(tabset)
  
}

