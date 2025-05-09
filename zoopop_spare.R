```{r}
span  <- 5
year_max <- (year(today()) + 1)
age_max  <- max(census$age)
year  <- seq(min(census$year_birth), year(today()), by = 1)
start <- seq(min(born), (year_max - 1), by = span)
end   <- start + (span - 1)

cohorts <- tibble(start = start) %>%
  mutate(cohort = row_number()) %>%
  mutate(born   = pmap(list(start), \(x) seq(x, sum(x, (span - 1)), by = 1))) %>%
  unnest(born) %>%
  select(cohort, born)

years  <- expand_grid(year, cohorts) %>%
  arrange(born, year) %>%
  rowwise() %>%
  mutate(age = year - born) %>%
  ungroup() %>%
  filter(between(age, 0, age_max))
```


```{r}
cohort_labs <- tibble(cohort_start = start, cohort_end = end) %>%
  mutate(cohort       = row_number(),
         cohort_range = as.character(str_glue("{cohort_start} - {cohort_end}")),
         cohort_years = pmap(list(cohort_start, cohort_end), \(x, y) seq(x, y, by = 1))) %>%
  select(cohort, cohort_start, cohort_end, cohort_range, cohort_years)

```

```{r}
studbook_cohorts <- studbook %>%
  filter(Sex != "U") %>%
  distinct(
    ID,
    Date_birth,
    Date_last,
    Sex
  ) %>%
  mutate(born = year(Date_birth),
         end  = year(Date_last), .keep = "unused") %>%
  left_join(cohorts, by = "born") %>%
  mutate(year = pmap(list(born, end), \(x, y) seq(x, y, by = 1))) %>%
  unnest(year) %>%
  select(cohort, born, ID, Sex, year) %>%
  arrange(cohort, year) %>%
  rowwise() %>%
  mutate(age = year - born) %>%
  ungroup()
```

```{r}
parents <- studbook %>%
  filter(Sex != "U") %>%
  distinct(
    ID,
    Date_birth,
    Sex,
    Sire,
    Dam
  ) %>%
  mutate(year = year(Date_birth), .keep = "unused") %>%
  rename(birth = ID, birth_sex = Sex) %>%
  pivot_longer(
    c(Sire, Dam),
    names_to  = "parent",
    values_to = "ID"
  ) %>%
  filter(ID != 0) %>%
  select(-parent) %>%
  left_join(studbook_cohorts, by = join_by(ID, year)) %>%
  distinct(
    cohort,
    Sex,
    age,
    year,
    ID,
    birth,
    birth_sex
  ) %>%
  arrange(cohort, age, birth) %>%
  group_by(cohort, Sex, age, year, ID, birth_sex) %>%
  reframe(births = n()) %>%
  ungroup() %>%
  pivot_wider(
    names_from   = "birth_sex",
    names_prefix = "births_",
    values_from  = "births",
    values_fill  = 0
  ) %>%
  rowwise() %>%
  mutate(births = sum(births_M, births_F)) %>%
  ungroup() %>%
  right_join(studbook_cohorts, by = join_by(cohort, ID, Sex, age, year)) %>%
  mutate(across(starts_with("births"), ~replace_na(., 0))) %>%
  arrange(cohort, born, age)
```

```{r}
repro_output <- filter(parents, Sex == "F") %>%
  group_by(cohort, ID) %>%
  summarize(
    lifetime_births = sum(births),
    R0              = sum(births_F)
  ) %>%
  ungroup() %>%
  group_by(cohort) %>%
  summarize(N_F           = n(),
            cohort_births = sum(lifetime_births),
            R0            = mean(R0)) %>%
  ungroup() %>%
  right_join(distinct(cohorts, cohort), by = "cohort") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  arrange(cohort)
```

```{r}
survival_births <- parents %>%
  distinct(
    cohort,
    Sex,
    age,
    ID,
    births
  ) %>%
  group_by(cohort, Sex, age) %>%
  summarize(
    births = sum(births),
    Sx     = n()
  ) %>%
  ungroup() %>%
  right_join(expand_grid(distinct(years, cohort, age), Sex = c("M", "F")), 
             by = join_by(cohort, age, Sex)) %>%
  arrange(Sex, cohort, age) %>%
  mutate(across(c(births, Sx), ~replace_na(., 0))) %>%
  mutate(Dx = if_else(
    age < age_max,
    Sx - lead(Sx),
    Sx), .by = c(cohort, Sex)) %>%
  mutate(Dx = replace_na(Dx, 0)) %>%
  rowwise() %>%
  mutate(bx = if_else(Sx > 0, births/Sx, 0)) %>%
  ungroup()  %>%
  pivot_wider(
    names_from   = "Sex",
    names_sep    = "",
    values_from  = c("Sx", "Dx", "bx", "births"),
    values_fill  = 0
  ) %>%
  rowwise() %>%
  mutate(Sx     = sum(SxM, SxF),
         Dx     = sum(DxM, DxF),
         births = sum(birthsM, birthsF),
         bx     = if_else(Sx > 0, births/Sx, 0)) %>%
  ungroup() %>%
  rename(x = age)
```

```{r}
vitals <- survival_births %>%
  group_by(cohort) %>%
  summarize(
    x   = list(x),
    SxF = list(SxF),
    SxM = list(SxM),
    Sx  = list(Sx),
    DxF = list(DxF),
    DxM = list(DxM),
    Dx  = list(Dx),
    bxF = list(bxF),
    bxM = list(bxM),
    bx  = list(bx)
  ) %>%
  ungroup() %>%
  left_join(cohort_labs, by = "cohort") %>%
  relocate(starts_with("cohort_"), .after = cohort) %>%
  slice_head(n = (nrow(.)) - 1)
```

```{r}
life <- survival_births %>%
  left_join(cohort_labs, by = "cohort") %>%
  relocate(starts_with("cohort_"), .after = cohort) %>%
  nest(!starts_with("cohort"), .by = c(starts_with("cohort")))  %>%
  slice_head(n = (nrow(.)) - 1)
```

```{r}
cohort_lifetab <- function(df) {
  mutate(df,
         lt = map(data, \(i) life.table(
           x       = i$x,
           nDx     = i$Dx,
           nKx     = i$Sx,
           type    = "kf",
           iwidth  = 1,
           width12 = c(1,1)
         ))
  ) %>%
    mutate(
      lt = map(lt, \(x) mutate(x, across(where(is.numeric), ~ replace_na(., 0))))
    ) %>%
    mutate(
      life = map2(data, lt, \(i, j) left_join(i, j, by = "x"))
    ) %>%
    mutate(
      les = map(life, \(i) summarize(
        i,
        x   = list(x),
        nLx = list(nLx),
        lx  = list(lx),
        mx  = list(bx)
      ))
    ) %>%
    hoist(les, 
          x   = list("x"),
          nLx = list("nLx"), 
          lx  = list("lx"), 
          mx  = list("mx"), .remove = FALSE) %>%
    hoist(
      life,
      Sx = list("Sx"),
      .remove = FALSE
    )
}
```

```{r}
lifetab <- cohort_lifetab(life)
```

```{r}
cohorts <- pull(lifetab, cohort_start)
leslie <- map(seq(1:nrow(lifetab)), \(i) list(
  x   = lifetab$x[[i]], 
  nLx = lifetab$nLx[[i]], 
  lx  = lifetab$lx[[i]], 
  mx  = lifetab$mx[[i]]
)) %>%
  set_names(cohorts) %>%
  map_depth(., 1, \(x) list_flatten(x, name_spec = "outer"))
```

```{r}
list_life <- function(df) {
  cohorts <- pull(df, cohort_start)
  result <- map(seq(1:nrow(df)), \(i) list(
    x   = df$x[[i]], 
    Sx  = df$Sx[[i]], 
    nLx = df$nLx[[i]], 
    lx  = df$lx[[i]], 
    mx  = df$mx[[i]]
  )) %>%
    set_names(cohorts) %>%
    map_depth(., 1, \(x) list_flatten(x, name_spec = "outer"))
  return(result)
}

base_leslie <- function(i) {
  age    <- i$x
  stages <- c(seq(from = min(age), to = max(age)))
  lx     <- i$nLx
  mx     <- i$mx
  matrix(
    leslie.matrix(
      lx           = lx,
      mx           = mx,
      peryear      = 1,
      infant.class = FALSE
    ),
    nrow = length(age),
    ncol = length(age),
    dimnames = list(stages, stages)
  )
}

safe_leslie <- safely(base_leslie)

fix_leslie <- function(l) {
  
  error_msg <- function(x) x[["error"]]$message
  errors    <- map(l, \(x) error_msg(x)) %>% keep(\(x) !is.null(x))
  
  result_A <- function(x) x[["result"]]
  mats <- map(l, \(x) result_A(x)) %>% keep(\(x) !is.null(x))
  
  leslies <- list(errors, mats) %>%
    list_flatten(name_spec = "inner")
  
  return(leslies)
} 

cohorts_leslie <- function(df) {
  list_life(df) %>%
    imap(\(x, idx) safe_leslie(x)) %>%
    fix_leslie()
}
```
