names_2 <-  c("slope","ED50")
names_3 <-  c("slope","lower","upper")
names_4 <-  c("slope","lower","upper","ED50")
names_5 <-  c("slope","lower","upper","ED50", "Asymmetry")
names_2p <- c("slope1","lower1","upper1","ED501", "slope2", "upper2", "ED502")
names_m  <- c("slope1", "slope2", "slope3", "upper", "ED50")




guess_ED50 <- function(data){
  i <- which.min(abs(data$response - 0.5))
  data$dose[i]
}

ll4_base <-  function(data, grp, f_params){
  safe_drm <- safely(drm)
  ctrl     <- drmc(maxIt  = 5000,   # increase max iterations
                   relTol = 1e-8,   # tighten convergence tolerance
                   trace  = FALSE)
  
  start_vals <- c(
    EC50  = guess_ED50(data),
    Slope = 1
  )
  
 result <- safe_drm(
    formula = response ~ dose,
    data    = data,
    fct     = LL.4(
      fixed = f_params,
      names = names_4
    ),
    start   = start_vals,
    control = ctrl
  )
 
 
 if(!is.null(result$error)){
   warning("Fit failed for ", grp, ": ", result$error$message)
   return(NULL)
 }
 result$result
 
 return(result)
}

ll4_safely <- function(data_list, f_params) {
  initial <- imap(data_list, \(x, idx) ll4_base(x, idx, f_params))
  
  results <- compact(keep(initial, \(x) length(x) > 0)) %>%
    map_depth(1, \(x) compact(x)) %>%
    list_flatten(name_spec = "{outer}")
  
  errors <- as.list(names(discard(initial, \(x) length(x) > 0))) %>%
    set_names(., map(., \(x) paste0(x)))
  
  out <- list(results = results, errors = errors)
  
  return(out)
}

ll4_fixed <- function(data, f_params = c(NA, 0, 1, NA), names_4 = c("slope","lower","upper","ED50")) {
  start_vals <- c(
    EC50  = guess_ED50(data),
    Slope = 1
  )
  
  result <- drm(
    formula = response ~ dose,
    data    = data,
    fct     = LL.4(
      fixed = f_params,
      names = names_4
    ),
    start   = start_vals,
    control = drmc(maxIt  = 5000,   
                   relTol = 1e-8,   
                   trace  = FALSE)
  )
  return(result)
}

ll4_global_base <- function(d, nm) {
  assign(nm, d, envir = .GlobalEnv)
  drm(formula = response ~ dose,
      data    = get(nm, envir = .GlobalEnv))
}

msel_loop <- function(drc_list, candidates) {
  msel_list <- list()
  for (nm in names(drc_list)) {
    msel_list[[nm]] <- mselect(drc_list[[nm]], fctList = candidates)
  }
}

msel_tbl <- function(msel_obj, obj_name) {
  as_tibble(msel_obj, rownames = "model") %>% 
    mutate(group    = paste0(obj_name), 
           logLik   = `logLik`, 
           AIC      = IC,
           ResVar   = `Res var`,
           LoF_pval = round(`Lack of fit`, 5)) %>%
    select(group, 
           model, 
           logLik, 
           AIC, 
           ResVar,
           LoF_pval)
}





plot_msel <- function(data, color) {
  data <-  mutate(data, hover = as.character(str_glue(
    "{model} - {group}<br>AIC = {round(AIC, 1)}<br>logLik = {round(logLik, 2)}<br>ResVar = {round(ResVar, 3)}<br>p = {round(LoF_pval, 3)}"
  )))  
  plot <- plot_ly(data = data) %>%
    add_trace(
      data      = data,
      x         = ~model,
      y         = ~AIC,
      type      = "box",
      text      = ~AIC,
      line      = list(color = "#000000FF", width = 0.5),
      fillcolor = color
    ) %>%
    add_trace(
      data      = filter(data, LoF_pval > 0.05),
      x         = ~model,
      y         = ~AIC,
      color     = ~group,
      colors    = col.pal(data, "group"),
      type      = "scatter",
      mode      = "markers",
      text      = ~hover,
      marker    = list(
        size    = 5,
        opacity = 0.4,
        line    = list(color = "#000000FF", width = 0.5)
      )
    ) %>%
    add_trace(
      data      = filter(data, LoF_pval <= 0.05),
      x         = ~model,
      y         = ~AIC,
      text      = ~hover,
      color     = ~group,
      colors    = col.pal(data, "group"),
      type      = "scatter",
      mode      = "markers",
      marker    = list(
        symbol  = "star",
        size    = 5,
        opacity = 0.8,
        line    = list(color = "#000000FF", width = 0.5)
      )
    ) %>%
    layout(
      showlegend   = FALSE,
      xaxis        = list(
        title          = "DRC Model",
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = T,
        ticks          = "inside",
        tickfont       = list(size = 10)
      ),
      yaxis        = list(
        title          = "AIC",
        zeroline       = F,
        showgrid       = F,
        showline       = T,
        showticklabels = T,
        nticks         = 5,
        ticks          = "inside",
        tickfont       = list(size = 10)
      )
    ) %>%
    hide_colorbar()
  return(plot)
}
