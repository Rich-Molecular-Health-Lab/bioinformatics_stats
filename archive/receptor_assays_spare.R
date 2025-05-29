## Run Model Selection Algorithm

### Create new objects for successful runs

```{r}
plates_working <- keep_at(data_plate, names(ll4_plates$results))
plates_drc     <- imap(plates_working, \(x, idx) ll4_global_base(x, idx))
pooled_working <- keep_at(data_pooled, names(ll4_pooled$results))
pooled_drc     <- imap(pooled_working, \(x, idx) ll4_global_base(x, idx))
```

```{r}
f_params_4 <- c(NA, 0, 1, NA)
f_params_5 <- c(NA, 0, 1, NA, NA)
candidates <- list( 
  LL.4(    names = names_4, fixed = f_params_4),   
  LL.5(    names = names_5, fixed = f_params_5),   
  BC.4(    names = names_4, fixed = f_params_4),   
  BC.5(    names = names_5, fixed = f_params_5),    
  CRS.4a(  names = names_4),  
  LN.4(    names = names_4, fixed = f_params_4),   
  gammadr( names = names_4, fixed = f_params_4),
  twophase(names = names_2p, fixed = c(NA, 0, 1, NA, NA, 1, NA))
)

```


### By Species + Receptor + Plate + Treatment


```{r warning=FALSE, message = FALSE, comment = "", echo = FALSE}
msel_plates <- list()
for (nm in names(plates_drc)) {
  msel_plates[[nm]] <- mselect(plates_drc[[nm]], fctList = candidates)
}

```

```{r}
msel_plates_tbl <- imap_dfr(msel_plates, \(x, idx) msel_tbl(x, idx))

selection_plates <- msel_plates_tbl %>%
  group_by(group) %>%
  filter(!is.na(AIC)) %>%
  slice_min(AIC, n = 1) %>%
  ungroup()

```

### By Species + Receptor + Treatment (Plates Pooled)

```{r warning=FALSE, message = FALSE, comment = "", echo = FALSE}

msel_pooled <- list()
for (nm in names(pooled_drc)) {
  msel_pooled[[nm]] <- mselect(pooled_drc[[nm]], fctList = candidates)
}

```

```{r}
msel_pooled_tbl <- imap_dfr(msel_pooled,  \(x, idx) msel_tbl(x, idx))

selection_pooled <- msel_pooled_tbl %>%
  group_by(group) %>%
  filter(!is.na(AIC)) %>%
  slice_min(AIC, n = 1) %>%
  ungroup()

```

## Visuals to Help with Model Selection


```{r}
msel_plot_pooled   <- plot_msel(filter(msel_pooled_tbl, !str_detect(group, "standard")), "#A56EB680")
msel_plot_plates   <- plot_msel(filter(msel_plates_tbl, !str_detect(group, "standard")), "#3EBCB680")
msel_plot_e2pooled <- plot_msel(filter(msel_pooled_tbl, str_detect(group, "standard")), "#4B1C5733")
msel_plot_e2plates <- plot_msel(filter(msel_plates_tbl, str_detect(group, "standard")), "#0169C480")

msel_subplot_plates <- subplot(
  msel_plot_plates,
  msel_plot_e2plates,
  nrows    = 1,
  shareY   = TRUE
) %>%
  add_annotations(
    x         = 0.5,
    y         = 1,
    text      = "Species + Receptor + Plate + Treatment",
    xref      = "paper",
    yref      = "paper",
    showarrow = FALSE,
    xanchor   = "center",  
    yanchor   = "top",
    font      = list(size = 12),
    layer       = "below",
    bgcolor     = "#ffffff"
  )
msel_subplot_pooled <- subplot(
  msel_plot_pooled,
  msel_plot_e2pooled,
  nrows    = 1,
  shareY   = TRUE
) %>%
  add_annotations(
    x         = 0.5,
    y         = 1,
    text      = "Species + Receptor + Treatment (Plates Pooled)",
    xref      = "paper",
    yref      = "paper",
    showarrow = FALSE,
    xanchor   = "center",  
    yanchor   = "top",
    font      = list(size = 12),
    layer       = "below",
    bgcolor     = "#ffffff"
  )


msel_plots <- subplot(
  msel_subplot_plates,
  msel_subplot_pooled,
  nrows    = 2,
  shareX   = FALSE,
  shareY   = FALSE
) %>%
  add_annotations(
    x         = 0,
    y         = 1,
    text      = "<i>Treatments (No E Standard)</i>",
    xref      = "paper",
    yref      = "paper",
    showarrow = FALSE,
    xanchor   = "left",  
    yanchor   = "top",
    font      = list(size = 10),
    layer       = "below",
    bgcolor     = "#ffffff"
  ) %>%
  add_annotations(
    x         = 1,
    y         = 1,
    text      = "<i>E Standards Only</i>",
    xref      = "paper",
    yref      = "paper",
    showarrow = FALSE,
    xanchor   = "right",  
    yanchor   = "top",
    font      = list(size = 10),
    layer       = "below",
    bgcolor     = "#ffffff"
  )

save_html(msel_plots, "visuals/receptor_assays/model_selection_plots.html")
```

```{r}
msel_plots
```


LL.4 looks like the best fit.
### By Species + Receptor + Plate + Treatment

```{r}
ll4_plates <- ll4_safely(data_plate, f_params = c(NA, 0, 1, NA))
```


### By Species + Receptor + Treatment (Plates Pooled)

```{r}
ll4_pooled <- ll4_safely(data_pooled, f_params = c(NA, 0, 1, NA))
```
