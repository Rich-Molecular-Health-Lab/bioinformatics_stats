dr_plot_array <- function(df, pooled = TRUE, version = NULL, type = NULL) {
  
  if (is.null(type)) { type <- "nav_pill" }
  
  plots <- dr_plot_list(df, pooled, version) %>% map_subplots(version)
  
  panel_set  <- grid_panelset(plots, type)
  
  if (type == "accordion") {
    panels <- card(accordion(!!!panel_set, open = FALSE, multiple = FALSE))
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
  
  raw_notes <- accordion(
    open = FALSE, multiple = FALSE,
    accordion_panel(
      "Analysis Notes",
      ul(
        li("Log-logistic is the most common sigmoid curve"),
      )
    ),
    accordion_panel(
      "Plot Notes",
      ul(
        li("Markers colored by log-transformed dose.")
      )
    )
  )
  
  
  residuals_notes <- accordion(
    open = FALSE, multiple = FALSE,
    accordion_panel(
      "Analysis Notes",
      ul(
        li("Plot shows the distribution of residuals against the fitted value (variance homogeneity)"),
        li("Ideal models should maximize the homogeneity of the residuals"),
        li("We assume that the observations for doses are independent of each other.")
        )
      ),
    accordion_panel(
      "Plot Notes",
      ul(
        li("Markers colored by log-transformed dose.")
      )
    )
  )

  qq_notes <- accordion(
    open = FALSE, multiple = FALSE,
    accordion_panel(
      "Analysis Notes",
      ul(
        li("Plot shows normal q-q plot illustrating whether the residuals are normally distributed (normally distributed measurement errors)"),
        li("Close-fitting points along the line suggests no serious deviation from the assumption of normal distribution of residuals"),
      )
    ),
    accordion_panel(
      "Plot Notes",
      ul(
        li("Markers colored by log-transformed dose.")
      )
    )
  )
  
  
  curve_notes <- accordion(
    open = FALSE, multiple = FALSE,
    accordion_panel(
      "Analysis Notes",
      ul(
        li("The p-values tell us if the curve-fitting parameters are different from zero."),
        li("Non-significant indicates the regression analysis was as efficient as ANOVA at describing the data.")
      )
    ),
    accordion_panel(
      "Plot Notes",
      ul(
        li("Markers represent observed values."),
        li("Line represents predicted values based on fitted model."),
        li("Shading is for 95% confidence interval around the predicted values.")
      )
    )
  )
  
  
    
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
      nav_panel("Raw Data", 
                navset_pill(
                  nav_panel("Raw Dose", raw),
                  nav_panel("log10(Dose)", raw_logdose)
                  )
                ),
    nav_panel("Dose-Response Curves", 
              curve_notes,
              curves
              ),
    nav_panel("Residuals", 
              fit_notes,
              residuals
              ),
    nav_panel("Q-Q Normals", 
              qqnorms
              )
  )
  return(out)
}
