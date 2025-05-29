
data_bygroup <- function(df, group_var, pooled = TRUE) {
  
  df$group_var <- df[[paste0(group_var)]]
  data <- mutate(df, group = factor(as.character(str_glue("{as.character(group_var)}"))))
  
  if (isFALSE(pooled)) {
    data_group <- data %>%
      rename(group_curve = group_plate)
  } else if (isTRUE(pooled)) {
    data_group <- data %>%
      rename(group_curve = group_pooled) %>%
      select(-group_plate)
  }
    result <- data_group %>%
    select(starts_with("labs_"),
           ends_with("treatment"),
           groups_treatment,
           receptor,
           starts_with("group"),
           log_dose,
           dose,
           resp = rel_resp
    ) %>%
    mutate(across(c(receptor, group, group_curve, treatment), ~factor(.))) %>%
    mutate(n_doses = n_distinct(dose), .by = "group_curve") %>%
    filter(n_doses > 2) %>%
    group_by(group) %>%
    group_split() %>%
    set_names(., map(., \(x) as.character(unique(pull(x, "group")))))
    return(result)
}

data_nested <- function(df, pooled = TRUE) {
  if (isFALSE(pooled)) {
    data <- df %>%
      rename(group = group_plate)
  } else if (isTRUE(pooled)) {
    data <- df %>%
      rename(group = group_pooled) %>%
      select(-group_plate)
  }
  
  result <- data   %>%
    nest(.by = "group")
  
}


drm_nested <- function(df, pooled = TRUE) {
  if (isFALSE(pooled)) {
    data <- df %>%
      rename(group = group_plate)
  } else if (isTRUE(pooled)) {
    data <- df %>%
      rename(group = group_pooled) %>%
      select(-group_plate)
  }
  
  result <- data   %>%
    nest(.by = "group") %>%
    mutate(drm_result  = map(data, \(x) drm_default(x))) %>%
    mutate(drm_summary = map(drm_result, \(x) summary(x)))  %>%
    dr_receptor_fit() %>%
    mutate(data_resid  = map(data, \(x) augment(drm_default(x), x)))  %>%
    mutate(data_qq     = map(data_resid, \(x) drm_qq(x))) %>%
    mutate(annot_resid = as.character(str_glue("SE: {round(res_se, digits = 3)} ({res_df} Df)")), 
           annot_curve = as.character(str_glue("p = {round(fit_pval, digits = 3)} ({fit_df} Df)"))) %>%
    dr_predicted() %>%
    select(starts_with("group"),
           starts_with("data_"),
           starts_with("drm_"),
           starts_with("fit_"),
           starts_with("res_"),
           starts_with("annot_")
    )
  return(result)
}

dr_data_list <- function(df, pooled = TRUE) {
  
  if (isFALSE(pooled)) {
    data <- df %>%
      rename(group = group_plate) %>%
      mutate(sub_name = as.character(str_glue("Plate {plate}")))
  } else if (isTRUE(pooled)) {
    data <- df %>%
      rename(group = group_pooled) %>%
      select(-group_plate) %>%
      mutate(sub_name = as.character(str_glue("{groups_treatment}")))
  }
  
  data_list <- data %>%
    mutate(plot_name = group) %>%
    select(receptor,
           group,
           treatment,
           labs_treatment,
           groups_treatment,
           sub_name,
           plot_name,
           starts_with("annot"),
           starts_with("data")) %>%
    group_by(receptor,
             group,
             treatment,
             labs_treatment,
             groups_treatment,
             sub_name,
             plot_name
    ) %>%
    group_split() %>% 
    set_names(., map(., \(x) unique(pull(x, "plot_name"))))
  
  return(data_list)
}

dr_plot_names <- function(data_list) {
  
  names <- enframe(data_list) %>%
    unnest(value) %>%
    select(receptor, sub_name, plot_name) %>%
    arrange(receptor, sub_name, plot_name) %>%
    nest(.by = c("receptor", "sub_name"), .key = "plot_name") %>%
    mutate(plot_name = map(plot_name, \(x) as.list(deframe(x)))) %>%
    nest(.by = c("receptor"), .key = "sub_name") %>%
    mutate(sub_name = map(sub_name, \(x) as.list(deframe(x)))) %>%
    rowwise() %>%
    group_split() %>%
    set_names(., map(., \(x) unique(pull(x, "receptor")))) %>%
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
    subplots <- map(data_list, \(x) plot_residuals(x$data_resid[[1]], x$treatment_label, x$annot_resid))
  } else if (version == "qqnorm") {
    subplots <- map(data_list, \(x) plot_qqnorm(x$data_qq[[1]], x$treatment_label, x$annot_resid))
  } else if (version == "curve") {
    subplots <- map(data_list, \(x) plot_curve(x$data_real[[1]], x$data_pred[[1]], x$treatment_label, x$annot_curve))
  } else if (version == "raw") {
    subplots <- map(data_list, \(x) plot_raw(x$data_resid[[1]], x$treatment_label, x$annot_resid))
  } else if (version == "raw_log") {
    subplots <- map(data_list, \(x) plot_raw(x$data_resid[[1]], x$treatment_label, x$annot_resid, log = TRUE))
  }
  subplot_list <- map_depth(subplot_names, 3, \(x) subplots[x]) %>%
    map_depth(2, \(x) list_flatten(x))
  
  return(subplot_list)
}

