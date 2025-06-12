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
