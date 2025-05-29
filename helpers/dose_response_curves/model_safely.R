drm_default <- function(x, separate = FALSE) {
  drm(
    fold_act ~ dose,
    curveid = treat,
    data    = x, 
    fct     = LL.4(names = c("Slope", 
                             "Lower Limit", 
                             "Upper Limit", 
                             "ED50")),
    separate = separate
  )
} 

model_safely <- function(list, default, separated) {
  
  safe_drm <- safely(default)
  
  drm_results <- map(list, \(x) safe_drm(x))
  
  errors <- map_depth(drm_results, 1, \(x) compact(keep_at(x, "error"))) %>%
    list_flatten(name_spec = "{outer}")
  
  drm_separated <- map(keep_at(list, c(names(errors))), separated)
  
  drm_pooled <- map_depth(drm_results, 1, \(x) compact(keep_at(x, "result"))) %>%
    compact() %>%
    list_flatten(name_spec = "{outer}")
  
  results <- list_assign(drm_separated, !!!drm_pooled)
  return(results)
}


drm_default <- function(x, separate = FALSE) {
  drm(
    fold_act ~ dose,
    curveid = treat,
    data    = x, 
    fct     = LL.4(names = c("Slope", 
                             "Lower Limit", 
                             "Upper Limit", 
                             "ED50")),
    separate = separate
  )
} 
