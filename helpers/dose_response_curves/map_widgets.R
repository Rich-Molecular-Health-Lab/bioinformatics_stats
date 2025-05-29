map_plots <- function(x, version = NULL) {
  imap(x, \(y, idy) dr_subplot_card(y, idy, version))
}

map_subplots <- function(subplot_list, version = NULL) {
  map_depth(subplot_list, 1, \(x) map_plots(x, version)) %>%
    tagList() %>%
    list_flatten(name_spec = "{inner}")
}

grid_panelset <- function(layout_list, type = NULL) {
  panel_set <- imap(layout_list, \(x, idx) grid_panel(x, idx, type)) %>%
    tagList()
  
  if (type == "accordion") {
    out <- panel_set
  } else if (startsWith(type, "nav") | is.null(type)) {
    out <- panel_set %>%
      list_flatten() %>%
      set_names(., "")
  }
  
  return(out)
}