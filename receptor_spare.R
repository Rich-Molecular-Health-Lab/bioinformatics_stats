### Tibbles for Labels

```{r}
receptor_labs <- tibble(
  labs_er    = c(rep("ER\u03B1", 3), rep("ER\u03B2", 3)),
  labs_genus = rep(c("<i>Gorilla</i>", "<i>Xanthonycticebus</i>", "<i>Homo</i>"), 2),
  receptor   = c("gorilla_alpha", "loris_alpha", "human_alpha", "gorilla_beta", "loris_beta", "human_beta")
) 


treatment_labs <- tibble(
  labs_treatment = c(
    "E2"            ,
    "E1"            ,
    "E3"            ,
    "Genistein"     ,
    "Daidzein"      ,
    "Coumestrol"    ,
    "Equol"         ,
    "Enterodiol"    ,
    "5M1S"          ,
    "5MA4"          ,
    "5M1G"          ,
    "5MA3"          ,
    "5M6C"          ,
    "5M02"          ,
    "5M02"          ,
    "5MA2"          ,
    "Lettuce"       ,
    "Random?"       ,
    "WI" 
  ),
  groups_treatment = c(
    rep("Estrogen", 3),
    rep("Metabolite", 5),
    rep("Biscuit Extract", 11)
  ),
  treatment      = c(
    "estrogen_estradiol"  ,
    "estrogen_estrone"    ,
    "estrogen_estriol"    ,
    "metabolite_genistein",
    "metabolite_daidzein" ,
    "metabolite_coumestrol",
    "metabolite_equol"     ,
    "metabolite_enterodiol",
    "biscuit_5M1S"   ,
    "biscuit_5MA4"   ,
    "biscuit_5M1G"   ,
    "biscuit_5MA3"   ,
    "biscuit_5M6C"   ,
    "biscuit_5M02"   ,
    "biscuit_5M02"   ,
    "biscuit_5MA2"   ,
    "produce_lettuce"        ,
    "unknown_random"      ,
    "unknown_wi"          
  )             
) %>% mutate(treatment = factor(treatment))

```

```{r}
mutate(receptor  = factor(as.character(str_glue("{Species}_{ER}")))) %>%
  arrange(Date, receptor) %>%
  mutate(plate     = factor(consecutive_id(Date, receptor))) %>%
  mutate(dose_type = case_when(
    str_detect(Dose, "mg/ml") ~ "mg/ml",
    str_detect(Dose, "Veh")   ~ "vehicle",
    Dose == "Control"         ~ "control",
    .default = "molar"
  ),
  dose = as.numeric(str_remove_all(Dose, "[^\\d|-|\\.]"))
  ) %>%
  mutate(dose = if_else(dose_type == "molar", as.numeric(str_glue("1e{Dose}")), dose),
         dose_type = factor(dose_type)) %>%
  arrange(plate, receptor, treatment, dose) %>%
  mutate(Dose = factor(Dose)) %>%
  mutate(dil_series = consecutive_id(Dose), .by = c(plate, treatment)) %>%
  select(receptor,
         plate,
         Date,
         treatment,
         dose_type,
         dil_series,
         dose,
         starts_with("luciferase"),
         starts_with("bgal")
  ) %>%
  pivot_longer(
    c(starts_with("luciferase"), starts_with("bgal")),
    names_pattern = c("(^\\w+)_(\\d$)"),
    names_to     = c("value_type", "replicate"),
    values_to    = "value"
  ) %>%
  mutate(dil_series = if_else(is.na(dose), row_number(), dil_series), 
         .by = c(receptor, plate, treatment, value_type))  %>%
  pivot_wider(
    names_from  = value_type,
    values_from = value
  ) %>%
  rowwise() %>%
  mutate(ratio = luciferase/bgal) %>%
  ungroup() %>%
  mutate(mean_vehicle = mean(ratio[str_starts(treatment, "vehicle")]), .by = "plate") %>%
  rowwise() %>%
  mutate(fold_act = ratio/mean_vehicle) %>%
  ungroup() %>%
  filter(treatment != "control" & !str_starts(treatment, "vehicle")) %>%
  mutate(e2_max = mean(fold_act[treatment=="estrogen_estradiol" & dose == 1e-9]), .by = "plate") %>%
  rowwise() %>%
  mutate(response = fold_act/e2_max) %>%
  ungroup() %>%
  left_join(receptor_labs, by = "receptor") %>%
  left_join(treatment_labs, by = "treatment") %>%
  mutate(group_pooled = factor(str_remove_all(
    as.character(str_glue(
      "{as.character(receptor)}_{as.character(dose_type)}_{as.character(treatment)}"
    )), "/"
  ))) %>%
  mutate(group_plate = factor(as.character(str_glue(
    "{as.character(group_pooled)}_{as.character(plate)}"
  ))),
  date         = mdy(Date),
  labs_plate   = as.character(str_glue(
    "Plate {as.character(plate)} ({date})"
  ))
  ) %>%
  arrange(receptor, dose_type, groups_treatment, treatment, plate, dil_series, replicate) %>%
  select(
    starts_with("labs"),
    receptor,
    date,
    ends_with("plate"),
    ends_with("treatment"),
    starts_with("group"),
    starts_with("dose"),
    dil_series,
    replicate,
    luciferase,
    bgal,
    ratio,
    mean_vehicle,
    fold_act,
    e2_max,
    response
  ) %>%
  filter(treatment != "produce_lettuce" & !str_starts(treatment, "unknown")) %>%
  group_by(group_plate) %>%
  filter(n_distinct(dose) > 2) %>%
  ungroup()

write_csv(esr_data, "data/receptors/esr_assays_tidy.csv")

```

