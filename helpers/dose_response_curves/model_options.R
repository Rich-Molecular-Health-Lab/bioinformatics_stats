drm_default <- function(x) {
  drm(resp ~ dose, data = x, fct  = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
}

drm_base <- function(x) {
  drm(resp ~ dose, data = x, fct  = LL.2(names = c("Slope", "ED50")))
}


dr_linear <- function(x) {
  lm(resp ~ dose, data = x)
}

drm_select <- function(x) {
  mselect(
    x,
    fctList = list(
      LL.2(),
      LL.3(),    
      LL.4(),    
      W1.4(),    
      W2.4()
      ),
    linreg = TRUE
    )
}

map_drm_default <- function(data_list) {
  map_depth(data_list, 1, \(x) drm_default(x))
}
