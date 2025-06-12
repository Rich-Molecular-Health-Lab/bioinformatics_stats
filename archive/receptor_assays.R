
treat_vars <- c(
  "treat_type",
  "treat_subtype",
  "treat",
  "treat_name",
  "treat_desc"
)

group_qc <- c(
  "plate",
  "genus", 
  "subj", 
  "receptor",
  "model",
  treat_vars
)

group_pooled <- c(
  "genus", 
  "subj", 
  "receptor",
  "model",
  treat_vars
)

drm_default <- function(x) {
  drm(fold_act ~ dose, data = x, fct  = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
}

drm_qq <- function(x){
  res <- x[[".resid"]]
  log10_dose <- x[["log10_dose"]]
  qq  <- qqnorm(res, plot.it = FALSE)
  tibble(
    theoretical = qq$x,              
    sample      = qq$y,
    log10_dose  = log10_dose
  )
}

dr_predicted <- function(df) {
  df %>%
    mutate(
      grid  = map(data, \(x) seq(min(x$dose), max(x$dose), length.out = 200)),
      preds = map2(
        drm_result, grid, \(x, y) predict(x, data.frame(dose = y), interval = "confidence")
      )
    ) %>%
    mutate(data_pred = map2(grid, preds, \(x, y) tibble(
      dose       = x,
      resp       = y[,"Prediction"],
      lower      = y[,"Lower"],
      upper      = y[,"Upper"]))
    ) %>%
    mutate(data_pred = map(data_pred, \(x) mutate(x, log10_dose = log10(dose)))) %>%
    rename(data_real = data) %>%
    mutate(data_real = map(data_real, \(x) select(x, log10_dose, resp = fold_act)),
           data_pred = map(data_pred, \(x) select(x, log10_dose, resp, lower, upper)))
}

dr_model_fit <- function(df) {
  df  %>%
  mutate(drm_fit     = map(drm_result, \(x) modelFit(x))) %>%
  hoist("drm_fit"    , fit_pval = list("p value", 2L), .remove = FALSE) %>%
  hoist("drm_fit"    , fit_df   = list("Df"     , 2L), .remove = FALSE) %>%
  hoist("drm_fit"    , fit_rss  = list("RSS"    , 2L), .remove = FALSE) %>%
  hoist("drm_summary", res_se   = list("rseMat" , 1L), .remove = FALSE) %>%
  hoist("drm_summary", res_df   = list("rseMat" , 2L), .remove = FALSE)
}


drm_nested <- function(df, group_vars) {
  result <- df   %>%
    nest(.by = c(group_vars)) %>%
    mutate(drm_result  = map(data, \(x) drm_default(x))) %>%
    mutate(drm_summary = map(drm_result, \(x) summary(x)))  %>%
    dr_model_fit() %>%
    mutate(data_resid  = map(data, \(x) augment(drm_default(x), x)))  %>%
    mutate(data_qq     = map(data_resid, \(x) drm_qq(x))) %>%
    mutate(annot_resid = as.character(str_glue("SE: {round(res_se, digits = 3)} ({res_df} Df)")), 
           annot_curve = as.character(str_glue("p = {round(fit_pval, digits = 3)} ({fit_df} Df)"))) %>%
    dr_predicted() %>%
    select(c(group_vars),
           data_real,
           data_pred,
           data_resid,
           data_qq,
           drm_result,
           drm_summary,
           fit_pval,
           fit_df,
           fit_rss,
           res_se,
           res_df,
           annot_resid,
           annot_curve
           )
  return(result)
}

assay_img <- function(basename) {
  paste0("https://raw.githubusercontent.com/Rich-Molecular-Health-Lab/bioinformatics_stats/a4a076e3d790ded0aed7a3c352ccb22291b467b8/images/", basename, ".png")
}

logo_tag <- function(basename) {
  tagList(img(src = assay_img(basename), 
              style="width:10%; height:10%;"))
}

dr_hover <- function() {
  paste0("dose = %{x:.2f}<br>response = %{y:.2f}")
}

dr_logos <- function(basename) {
  img_layer <- list(
    source   = assay_img(basename),
    xref     = "paper", 
    yref     = "paper",
    x         = 0,
    y         = 1,
    sizex    = 0.25,     
    sizey    = 0.25,
    xanchor  = "left",  
    yanchor  = "top",
    layer    = "above",
    opacity  = 0.7
  )
  return(img_layer)
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

dose_pal <- function(data, variable) {
  n_colors <- length(unique(pull(data, variable)))
  pal <-  as.character(paletteer_c("harrypotter::lunalovegood", n = n_colors))
  return(pal)
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

residuals_plot <- function(data_resid, title, stats) {
  plot_ly() %>%
    residuals_trace(data_resid) %>%
    annotate_subplots(title, stats) %>%
    layout(
      showlegend   = FALSE,
      xaxis        = list(
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = T,
        nticks         = 3,
        ticks          = "inside",
        tickfont       = list(size = 7)
      ),
      yaxis        = list(
        zeroline       = T,
        showgrid       = F,
        showline       = T,
        showticklabels = T,
        nticks         = 3,
        ticks          = "inside",
        tickfont       = list(size = 7)
      )
    ) %>%
    hide_colorbar()
  
}

qq_plot <- function(data_resid, title, stats) {
  plot_ly() %>%
    qq_trace(data_resid) %>%
    annotate_subplots(title, stats) %>%
    layout(
      showlegend   = FALSE,
      xaxis        = list(
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = F
      ),
      yaxis        = list(
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = F
      )
    ) %>%
    hide_colorbar()
}

plot_raw <- function(data_resid, title, stats, log = FALSE) {
  plot_ly() %>%
    dr_trace_raw(data_resid, log) %>%
    annotate_subplots(title, stats) %>%
    layout(
      showlegend   = FALSE,
      xaxis        = list(
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = T,
        nticks         = 4,
        ticks          = "inside"
      ),
      yaxis        = list(
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = T,
        nticks         = 4,
        ticks          = "inside"
      )
    ) %>%
    hide_colorbar()
  
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

dr_plot_each <- function(data_real, data_pred, title, stats) {
  plot_ly() %>%
    dr_trace_pred(data_pred) %>%
    dr_trace_real(data_real) %>%
    annotate_subplots(title, stats) %>%
    layout(
      showlegend   = FALSE,
      xaxis        = list(
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = F,
        nticks         = 2
      ),
      yaxis        = list(
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = T,
        nticks         = 3,
        ticks          = "inside"
      )
    ) %>%
    hide_colorbar()
}

annotate_subplots <- function(plot, title, stats) {
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

dr_subplots <- function(x, title, version = NULL) {
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

dr_plate_cards <- function(x, version = NULL) {
  imap(x, \(y, idy) dr_subplots(y, idy, version))
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

dr_accord_panel <- function(x, idx) {
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
  
  out <- accordion_panel(HTML(title), layout, icon = logo_tag(idx))
  
  return(out)
}

dr_nav_panel <- function(x, idx, type = NULL) {
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
  title  <- paste0("ER ", receptor, " <em>", genus, "</em>")
  header <- card_image(src = assay_img(idx), width = "10%")
  layout <- do.call(bscols, c(list(widths = rep(6, length(x))), x))
  out    <- nav_panel(HTML(title), layout, icon = logo_tag(idx))
  
  return(out)
}

dr_data_list <- function(df, pooled = TRUE) {
  if (isTRUE(pooled)) {
    data <- df %>%
      mutate(plot_name    = as.character(str_glue("{model}_{treat}")),
             sub_name     = str_to_title(treat_subtype))
  } else if (isFALSE(pooled)) {
    data <- df %>%
      mutate(
        plot_name   = as.character(str_glue("{model}_{plate}_{treat}")),
        sub_name    = as.character(str_glue("Plate {plate}")))
  }
  

  data_list <- data %>%
    mutate(receptor = case_match(
      receptor, "alpha" ~ "\u03B1", "beta" ~ "\u03B2"
      )) %>%
    mutate(treat_label = if_else(
        treat_subtype %in% c("biscuit", "estrogen"),
        str_to_upper(treat), str_to_title(treat)),
        sub_name = as.character(str_glue("{sub_name} {receptor}"))
        ) %>%
    select(model,
           receptor,
           treat_type,
           treat_label,
           treat_desc,
           treat_name,
           sub_name,
           plot_name,
           annot_resid,
           annot_curve,
           data_real,
           data_pred,
           data_resid,
           data_qq) %>%
    group_by(
      model,
      receptor,
      treat_type,
      treat_label,
      treat_desc,
      treat_name,
      plot_name,
      sub_name,
      annot_resid,
      annot_curve
    ) %>%
    group_split() %>% 
    set_names(., map(., \(x) unique(pull(x, "plot_name"))))
  
  return(data_list)
}

dr_plot_names <- function(data_list) {
  
  names <- enframe(data_list) %>%
    unnest(value) %>%
    select(model, sub_name, plot_name) %>%
    arrange(model, sub_name, plot_name) %>%
    nest(.by = c("model", "sub_name"), .key = "plot_name") %>%
    mutate(plot_name = map(plot_name, \(x) as.list(deframe(x)))) %>%
    nest(.by = c("model"), .key = "sub_name") %>%
    mutate(sub_name = map(sub_name, \(x) as.list(deframe(x)))) %>%
    rowwise() %>%
    group_split() %>%
    set_names(., map(., \(x) unique(pull(x, "model")))) %>%
    map_depth(1, \(x) select(x, sub_name)) %>%
    map_depth(1, \(x) as.list(deframe(x))) %>%
    map_depth(1, \(x) list_flatten(x, name_spec = "{inner}")) %>%
    map_depth(2, \(x) set_names(x, x))
  
  return(names)
}

dr_plot_list <- function(df, pooled = TRUE, version = NULL) {
  data_list     <- dr_data_list(df, pooled)
  subplot_names <- dr_plot_names(data_list)
  if (version == "residuals") {
    subplots <- map(data_list, \(x) residuals_plot(x$data_resid[[1]], x$treat_label, x$annot_resid))
  } else if (version == "qqnorm") {
    subplots <- map(data_list, \(x) qq_plot(x$data_qq[[1]], x$treat_label, x$annot_resid))
  } else if (version == "curve") {
    subplots <- map(data_list, \(x) dr_plot_each(x$data_real[[1]], x$data_pred[[1]], x$treat_label, x$annot_curve))
  } else if (version == "raw") {
    subplots <- map(data_list, \(x) plot_raw(x$data_resid[[1]], x$treat_label, x$annot_resid))
  } else if (version == "raw_log") {
    subplots <- map(data_list, \(x) plot_raw(x$data_resid[[1]], x$treat_label, x$annot_resid, log = TRUE))
  }
  subplot_list <- map_depth(subplot_names, 3, \(x) subplots[x]) %>%
    map_depth(2, \(x) list_flatten(x))
  
  return(subplot_list)
}

dr_subplot_cards <- function(subplot_list, version = NULL) {
  map_depth(subplot_list, 1, \(x) dr_plate_cards(x, version)) %>%
    tagList() %>%
    list_flatten(name_spec = "{inner}")
}

dr_accord_set <- function(layout_list, version = NULL) {
  imap(layout_list, \(x, idx) dr_accord_panel(x, idx)) %>%
    tagList()
}

dr_panel_set <- function(layout_list, version = NULL) {
  imap(layout_list, \(x, idx) dr_nav_panel(x, idx)) %>%
    tagList() %>%
    list_flatten() %>%
    set_names(., "")
}

dr_plot_array <- function(df, pooled = TRUE, version = NULL, type = NULL) {
  
  if (is.null(type)) { type <- "nav_pill" }
  
  plots <- dr_plot_list(df, pooled, version) %>%
    dr_subplot_cards(version)
  
  accord_set <- dr_accord_set(plots)
  panel_set  <- dr_panel_set(plots)
  
  if (type == "accordion") {
    panels <- card(accordion(!!!accord_set, open = FALSE, multiple = FALSE))
  } else if (type == "nav_pill") {
    panels <- navset_pill_list(!!!panel_set, widths = c(2, 10))
  } else if (type == "nav_card") {
    panels <- navset_card_tab(!!!panel_set)
  } else if (type == "nav_tab") {
    panels <- navset_tab(!!!panel_set)
  }
  
  return(panels)
  
}

dr_vis_summary <- function(df, pooled = TRUE) {
  
  raw_logdose <- dr_plot_array(df, 
                             pooled,
                             version = "raw_log",
                             type    = "nav_pill")
  
  raw <- dr_plot_array(df, 
                               pooled,
                               version = "raw",
                               type    = "nav_pill")
  
  residuals <- dr_plot_array(df, 
                             pooled,
                             version = "residuals",
                             type    = "nav_pill")
  
  qqnorms <- dr_plot_array(df, 
                           pooled,
                           version = "qqnorm",
                           type    = "nav_pill")
  
  
  curves <- dr_plot_array(df, 
                          pooled,
                          version = "curve",
                          type    = "nav_pill")
  
  
  out <- navset_tab(
    nav_panel("Raw Data", navset_pill(
      nav_panel("Raw Dose", raw),
      nav_panel("log10(Dose)", raw_logdose)
    )),
    nav_panel("Dose-Response Curves", curves),
    nav_panel("Residuals", residuals),
    nav_panel("Q-Q Normals", qqnorms)
  )
  return(out)
}


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



