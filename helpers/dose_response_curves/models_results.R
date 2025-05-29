model_coef_tbl <- function(drm_result, label) {
  enframe(coef(drm_result)) %>%
    mutate(param = str_extract(name, "^\\w+|\\w+\\s\\w+(?=[:punct:])"),
           group = str_extract(name, "(?<=[:punct:])\\w+.$"),
           .keep = "unused") %>%
    pivot_wider(
      names_from = "param",
      values_from = "value"
    ) %>%
    mutate(model = label)
}



model_dose_response <- function(df, model) {
  if (model == "linear") {
    data <- mutate(df, drm_result  = map(data, \(x) dr_linear(x)))
  } else if (model == "default") {
    data <- mutate(df, drm_result  = map(data, \(x) drm_default(x)))
  }
  
  result <- mutate(data, drm_summary = map(drm_result, \(x) summary(x)))
  return(result)
}


drm_qq <- function(x){
  res <- x[[".resid"]]
  log_dose <- x[["log_dose"]]
  qq  <- qqnorm(res, plot.it = FALSE)
  tibble(
    theoretical = qq$x,              
    sample      = qq$y,
    log_dose    = log_dose
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

