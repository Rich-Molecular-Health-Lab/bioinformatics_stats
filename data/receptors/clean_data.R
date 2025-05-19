assays <- read_csv("data/receptors/esr_assays_tubbs.csv") %>%
  rename(common = species) %>%
  filter(assay %in% c("luciferase", "b_gal")) %>%
  mutate(date          = mdy(date),
         log10_dose    = log10(dose),
         genus         = case_match(common, 
                                    "gorilla" ~ "Gorilla",
                                    "loris"   ~ "Xanthonycticebus",
                                    "human"   ~ "Homo"
         ),
         species       = case_match(common, 
                                    "gorilla" ~ "gorilla",
                                    "loris"   ~ "pygmaeus",
                                    "human"   ~ "sapiens"
         ),
         receptor      = fct_recode(receptor, !!!receptors),
         column        = fct_recode(treatment, !!!column),
         treat_type    = fct_recode(treatment, !!!treat_type),
         treat_subtype = fct_recode(treatment, !!!treat_subtype),
         treat         = fct_recode(treatment, !!!treatments))  %>%
  left_join(diet_key, by = "treat") %>%
  mutate(plate       = consecutive_id(date, receptor, genus)) %>%
  mutate(dose_rank   = row_number(), .by = c(plate, assay, column)) %>%
  select(date,
         genus,
         receptor,
         plate,
         column,
         treat_type,
         treat_subtype,
         treat,
         treat_name = name,
         treat_form = formula,
         assay,
         dose_type,
         dose_rank,
         log10_dose,
         dose,
         value_1,
         value_2,
         value_3) %>%
  pivot_longer(starts_with("value_"),
               names_to     = "replicate",
               names_prefix = "value_",
               values_to    = "value") %>%
  pivot_wider(names_from    = "assay",
              values_from   = "value")  %>%
  mutate(n_doses     = n_distinct(dose), .by = c(plate, treat))

write_csv(assays, "data/receptors/esr_assays_tidy.csv")

esr_normalized <- assays %>%
  rowwise() %>%
  mutate(normalized = luciferase/b_gal) %>%
  ungroup() %>%
  mutate(mean_veh = mean(normalized[dose_type == "vehicle"]), .by = "plate") %>%
  rowwise() %>%
  mutate(fold_act = normalized/mean_veh) %>%
  ungroup() %>%
  mutate(max_e2_plate = mean(fold_act[treat == "e2" & dose == 1e-9]), .by = "plate") %>%
  rowwise() %>%
  mutate(perc_e2 = (fold_act/max_e2_plate)*100) %>%
  ungroup() %>%
  mutate(mean_veh_e2 = mean(perc_e2[dose_type == "vehicle"]), .by = "plate") %>%
  rowwise() %>%
  mutate(adj_perc_e2 = perc_e2 - mean_veh_e2) %>%
  ungroup()  %>%
  filter(!is.na(dose) & !is.na(fold_act)) %>%
  select(
    plate,
    genus,
    receptor,
    treat_type,
    treat_subtype,
    treat = treat_form,
    treat_name,
    log10_dose,
    dose,
    replicate,
    fold_act,
    perc_e2,
    adj_perc_e2
  ) %>%
  mutate(n_doses = n_distinct(dose), .by = c(plate, treat)) %>%
  filter(n_doses > 2) %>% select(-n_doses)


write_csv(esr_normalized, "data/receptors/esr_assays_normalized.csv")



