diet_trials <- list(
  list(begin = ymd(path$day1   ), days = 07, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(NULL)),
  list(begin = ymd("2023-11-02"), days = 05, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(probiotic = list_assign(supplements$probiotic, dose = 5.0))),
  list(begin = ymd("2023-11-07"), days = 09, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(probiotic = list_assign(supplements$probiotic, dose = 10.0))),
  list(begin = ymd("2023-11-23"), days = 14, diet = list_assign(diets$oatgel, phase_name = "oatgel"),  supplement = list(probiotic = list_assign(supplements$probiotic, dose = 10.0))),
  list(begin = ymd("2023-12-14"), days = 14, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(steroid   = list_assign(supplements$steroid  , dose = 0.1))),
  list(begin = ymd("2024-01-04"), days = 14, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(steroid   = list_assign(supplements$steroid  , dose = 0.2))),
  list(begin = ymd("2024-01-25"), days = 26, diet = list_assign(diets$oatgel, phase_name = "oatgel"), supplement = list(probiotic = list_assign(supplements$probiotic, dose = 10.0), steroid = list_assign(supplements$steroid  , dose = 0.2))),
  list(begin = ymd("2024-02-20"), days = 03, diet = list_assign(diets$oatgel, phase_name = "oatgel"), supplement = list(steroid   = list_assign(supplements$steroid  , dose = 0.2))),
  list(begin = ymd("2024-02-23"), days = 66, diet = list_assign(diets$oatgel, phase_name = "oatgel"), supplement = list(probiotic = list_assign(supplements$probiotic, dose = 10.0), steroid = list_assign(supplements$steroid  , dose = 0.2))),
  list(begin = ymd("2024-04-29"), days = 07, diet = list_assign(diets$oatgel, phase_name = "oatgel"), supplement = list(probiotic = list_assign(supplements$probiotic, dose = 10.0), steroid = list_assign(supplements$steroid  , dose = 0.2), antibiotic = supplements$antibiotic)),
  list(begin = ymd("2024-05-06"), days = 23, diet = list_assign(diets$oatgel, phase_name = "oatgel"), supplement = list(probiotic = list_assign(supplements$probiotic, dose = 10.0), steroid = list_assign(supplements$steroid  , dose = 0.2))),
  list(begin = ymd("2024-05-29"), days = 17, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(NULL)),
  list(begin = ymd("2024-06-15"), days = 14, diet = list_assign(diets$biscuit_elim, phase_name = "biscuit_elim"), supplement = list(NULL)),
  list(begin = ymd("2024-06-29"), days = 14, diet = list_assign(diets$lessBug_moreEgg, phase_name = "lessBug_moreEgg"), supplement = list(NULL)),
  list(begin = ymd("2024-07-13"), days = 14, diet = list_assign(diets$seasonals, phase_name = "seasonals"), supplement = list(NULL)),
  list(begin = ymd("2024-07-27"), days = 05, diet = list_assign(diets$baseline, phase_name = "baseline"),  supplement = list(fiber = list_assign(supplements$fiber, dose = 0.5))),
  list(begin = ymd("2024-08-01"), days = 09, diet = list_assign(diets$baseline, phase_name = "baseline"),  supplement = list(fiber = list_assign(supplements$fiber, dose = 1.0))),
  list(begin = ymd("2024-08-10"), days = 14, diet = list_assign(diets$oatgel, phase_name = "oatgel"), supplement = list(NULL)),
  list(begin = ymd("2024-08-24"), days = 05, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(probiotic = list_assign(supplements$probiotic, dose = 5.0))),
  list(begin = ymd("2024-08-29"), days = 09, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(probiotic = list_assign(supplements$probiotic, dose = 10.0))),
  list(begin = ymd("2024-09-07"), days = 14, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(antidiarrheal = supplements$antidiarrheal)),
  list(begin = ymd("2024-09-21"), days = 14, diet = list_assign(diets$low_lectin, phase_name = "low_lectin"), supplement = list(NULL)),
  list(begin = ymd("2024-10-19"), days = 28, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(steroid   = list_assign(supplements$steroid  , dose = 0.2))),
  list(begin = ymd("2024-11-16"), days = 42, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(steroid   = list_assign(supplements$steroid  , dose = 0.1))),
  list(begin = ymd("2024-12-28"), days = 06, diet = list_assign(diets$baseline, phase_name = "baseline"), supplement = list(NULL))
) %>% .[order(map_dbl(., ~ .x$begin))] 



### Medication

In addition to the steroids included in trials, Culi recieved antibiotics for a stint during our collections. We definitely want to keep something like antibiotics factored into our microbiome data analysis, so I will create a table for that.

```{r}
culi.meds <- meds$culi %>%
  rowwise() %>%
  mutate(Date = list(seq.Date(from = start_day, to = last_day, by = "day"))) %>%
  unnest(Date) %>%
  select(Date, 
         med_type,
         med_name,
         med_dose,
         dose_units
  )

warble.meds <- meds$warble %>%
  rowwise() %>%
  mutate(Date = list(seq.Date(from = start_day, to = last_day, by = "day"))) %>%
  unnest(Date) %>%
  select(Date, 
         med_type,
         med_name,
         med_dose,
         dose_units
  )

```


### Housing

Over the course of the study, both lorises moved from neighboring enclosures in one building to a much more expansive set of enclosures with more variable shifting capabilities in another building. They moved at different dates, so I want to score each sample based on those different date ranges.

```{r}
warble.housing <- housing$warble %>%
  rowwise() %>%
  mutate(Date = list(seq.Date(from = start_day, to = last_day, by = "day"))) %>%
  unnest(Date) %>%
  select(Date, 
         enclosure)

culi.housing <- housing$culi %>%
  rowwise() %>%
  mutate(Date = list(seq.Date(from = start_day, to = last_day, by = "day"))) %>%
  unnest(Date) %>%
  select(Date, 
         enclosure)
```


### Bristol Fecal Scores

I only have scores for Culi recorded by keepers, and scores were recorded anywhere from 0 to 3x per day. For Warble, keepers indicated that daily scores should be recorded as "1".

```{r}
bristol <- read.table(here("metadata/loris/bristols.tsv"), sep = "\t", header = T) %>%
  mutate(Date = ymd(date), .keep = "unused")
```


```{r}
culi.bristol <- bristol %>%
  group_by(date) %>%
  summarize(bristol_min  = min(score),
            bristol_mean = round(mean(score), digits = 0),
            bristol_max  = max(score)) %>%
  ungroup() %>%
  full_join(study_days, by = join_by(date == Date)) %>%
  select(Date = date, 
         bristol_min,
         bristol_mean,
         bristol_max) %>%
  arrange(Date)

warble.bristol <- bristol_warble %>%
  group_by(date) %>%
  summarize(bristol_min  = min(score),
            bristol_mean = round(mean(score), digits = 0),
            bristol_max  = max(score)) %>%
  ungroup() %>%
  full_join(study_days, by = join_by(date == Date)) %>%
  select(Date = date, 
         bristol_min,
         bristol_mean,
         bristol_max) %>%
  arrange(Date)

```

### Repro

```{r, warning = FALSE}
access.schedule <- repro.log$access %>%
  rowwise() %>%
  mutate(Date = list(seq.Date(from = start_day, to = last_day, by = "day"))) %>%
  unnest(Date) %>%
  select(Date, 
         access = value)

estrus.schedule <- repro.log$estrus %>%
  rowwise() %>%
  mutate(Date = list(seq.Date(from = start_day, to = last_day, by = "day"))) %>%
  unnest(Date) %>%
  select(Date, 
         estrus = value)


preg.schedule <- repro.log$pregnant %>%
  rowwise() %>%
  mutate(Date = list(seq.Date(from = start_day, to = last_day, by = "day"))) %>%
  unnest(Date) %>%
  select(Date, 
         pregnant = value)

warble.repro.notes <- repro.log$notes %>%
  filter(subject == "warble" | subject == "both") %>%
  select(Date, 
         repro_note) %>%
  group_by(Date) %>%
  summarise(repro_note = str_c(repro_note, collapse = "; "), .groups = "drop")

culi.repro.notes <- repro.log$notes %>%
  filter(subject == "culi" | subject == "both") %>%
  select(Date, 
         repro_note) %>%
  group_by(Date) %>%
  summarise(repro_note = str_c(repro_note, collapse = "; "), .groups = "drop")

```


### Health

Now I will score the misc. notes provided by staff on health observations as variables. 

```{r}
culi.health <- health.log$culi %>%
  group_by(Date) %>%
  summarise(health_note = str_c(health_note, collapse = "; "), .groups = "drop")
```
