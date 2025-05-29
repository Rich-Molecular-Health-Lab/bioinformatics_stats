
plot_residuals <- function(data_resid, title, stats) {
  plot_ly() %>%
    residuals_trace(data_resid) %>%
    annotate_plots(title, stats) %>%
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

plot_qqnorm <- function(data_resid, title, stats) {
  plot_ly() %>%
    qq_trace(data_resid) %>%
    annotate_plots(title, stats) %>%
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
    annotate_plots(title, stats) %>%
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

plot_curve <- function(data_real, data_pred, title, stats) {
  plot_ly() %>%
    dr_trace_pred(data_pred) %>%
    dr_trace_real(data_real) %>%
    annotate_plots(title, stats) %>%
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
