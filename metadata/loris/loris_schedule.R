keeper_notes <- list(
  list(date = ymd("2024-01-22"), tag = "food" , subject = list("culi") , note = "removing cauliflower and broccoli from diet rotation"),
  list(date = ymd("2024-02-20"), tag = "food" , subject = list("culi") , note = "removing tomato from diet rotation"),
  list(date = ymd("2024-03-21"), tag = "repro" , subject = list("culi")             , note = "Culi whistling and actively attempting to get to Warble"),
  list(date = ymd("2024-04-04"), tag = "repro" , subject = list("culi", "warble"), note = "Observed several breeding events"),
  list(date = ymd("2023-11-16"), tag = "health", subject = list("culi")             , note = "had what appeared to be dried blood around his anus and tail"),
  list(date = ymd("2023-12-17"), tag = "health", subject = list("culi")             , note = "coprophagy observed"),
  list(date = ymd("2024-01-21"), tag = "health", subject = list("culi")             , note = "coprophagy observed"),
  list(date = ymd("2024-02-10"), tag = "health", subject = list("culi")             , note = "left biscuit and some gum"),
  list(date = ymd("2024-04-01"), tag = "repro" , subject = list("warble")           , note = "swollen vulva"),
  list(date = ymd("2024-04-02"), tag = "repro" , subject = list("culi")             , note = "swollen testicles"),
  list(date = ymd("2024-04-03"), tag = "repro" , subject = list("culi", "warble"), note = "Keeper Zach entered the facility in the morning to clean and found Warble and Culi sleeping together in a hammock in room 3"),
  list(date = ymd("2024-04-03"), tag = "repro" , subject = list("culi", "warble"), note = "Observed breeding event"),
  list(date = ymd("2024-04-04"), tag = "repro" , subject = list("warble")           , note = "Observed whistling"),
  list(date = ymd("2024-04-04"), tag = "repro" , subject = list("culi", "warble"), note = "JM and AMR found Warble and Culi sleeping together in a hammock in room 3"),
  list(date = ymd("2024-04-04"), tag = "repro" , subject = list("warble")         , note = "swollen vulva"),
  list(date = ymd("2024-04-06"), tag = "repro" , subject = list("warble")         , note = "pregnancy suspected"),
  list(date = ymd("2024-04-14"), tag = "health", subject = list("culi")             , note = "weight = 451 g"),
  list(date = ymd("2024-05-13"), tag = "health", subject = list("warble")           , note = "weight = 467 g"),
  list(date = ymd("2024-06-17"), tag = "health", subject = list("culi")             , note = "left pieces of food in room 5"),
  list(date = ymd("2024-08-08"), tag = "health", subject = list("culi")             , note = "extremely liquid feces"),
  list(date = ymd("2024-09-08"), tag = "health", subject = list("culi")             , note = "extremely liquid feces"),
  list(date = ymd("2025-01-23"), tag = "repro" , subject = list("culi", "warble"), note = "Social grooming observed between the barriers. Culi initiated grooming by licking Warble’s head; she licked his arm, and he was observed licking her genitals. No whistling was observed."),
  list(date = ymd("2024-10-09"), tag = "repro" , subject = list("warble")           , note = "gave birth to twins"),
  list(date = ymd("2024-10-10"), tag = "repro" , subject = list("warble")           , note = "double infanticide")
)   %>% .[order(map_dbl(., ~ .x$date))] %>%
  enframe(name = NULL) %>%
  unnest_wider(value) %>%
  unnest_longer(subject) %>%
  mutate(keeper_note = str_glue("{tag}", ": ", "{note}"), .keep = "unused") %>%
  ungroup() %>%
  reframe(keeper_note = str_flatten(keeper_note, collapse = "; "), .by = c(date, subject))

warble_cycles <- list(
  estrus = list(
    list(begin = ymd("2023-10-26"), end = ymd("2023-10-29")),
    list(begin = ymd("2024-03-21"), end = ymd("2024-04-06"))
  ),
  pregnant = list(
    list(begin = ymd("2024-04-07"), end = ymd("2024-10-09"))
  )
) %>%
  enframe(name = "warb_status") %>%
  unnest_longer(value) %>%
  unnest_wider(value) %>%
  mutate(date = map2(begin, end, seq, by = "day")) %>%  
  unnest(date) %>%
  select(-c(begin, end))

access_dates <- list(
  list(begin = ymd("2023-10-26"), end = ymd("2023-10-29")),
  list(begin = ymd("2024-04-02"), end = ymd("2024-04-15"))
) %>%
  enframe(name = NULL) %>%
  unnest_wider(value) %>%
  mutate(pair_access = "y") %>%
  mutate(date = map2(begin, end, seq, by = "day")) %>%  
  unnest(date) %>%
  select(-c(begin, end))


holdings <- list(
  tibble(
    subject = "culi",
    holding = "old",
    collection_date = seq.Date(
      ymd("2023-10-26"), 
      ymd("2024-02-27"),
      by = "day"
      )
  ),
  tibble(
    subject = "culi",
    holding = "new",
    collection_date = seq.Date(
      ymd("2024-02-28"), 
      ymd("2024-12-14"),
      by = "day"
    )
  ),
  tibble(
    subject = "warble",
    holding = "new",
    collection_date = seq.Date(
      ymd("2023-10-26"), 
      ymd("2024-02-28"),
      by = "day"
    )
  ),
  tibble(
    subject = "warble",
    holding = "new",
    collection_date = seq.Date(
      ymd("2024-02-29"), 
      ymd("2024-12-14"),
      by = "day"
    )
  )
) %>%
  list_rbind()

nutrition <- read_tsv("metadata/loris/nutrition.tsv", show_col_types = FALSE) %>%
  select(diet_name, class, item, relative) %>%
  mutate(
    across(c(class, item), ~ as.character(str_replace_all(., "\\s|:", "_"))),
    across(c(class, item), ~ str_remove_all(., "[(),+]"))) %>%
  mutate(item = str_to_lower(item)) %>%
  pivot_wider(names_from  = c("class", "item"),
              names_sep   = "_",
              values_from = "relative",
              values_fill = 0) %>%
  relocate(
    ends_with("_total"),
    starts_with("Total_"), .after = "diet_name") %>%
  mutate(diet_name = str_replace_all(
    str_to_lower(diet_name), "%\\s|,\\s|\\s|/", "_")) %>%
  relocate(diet_name)

supplements <- list(
  list(begin = ymd("2023-10-26"),  probiotic = 00.0, steroid = 00.0, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2023-11-02"),  probiotic = 05.0, steroid = 00.0, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2023-11-07"),  probiotic = 10.0, steroid = 00.0, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2023-12-14"),  probiotic = 00.0, steroid = 00.1, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-01-04"),  probiotic = 00.0, steroid = 00.2, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-01-25"),  probiotic = 10.0, steroid = 00.2, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-02-20"),  probiotic = 00.0, steroid = 00.2, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-02-23"),  probiotic = 10.0, steroid = 00.2, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-04-29"),  probiotic = 10.0, steroid = 00.2, fiber = 00.0, antibiotic = 12.5, antidiarrheal = 00.00),
  list(begin = ymd("2024-05-06"),  probiotic = 10.0, steroid = 00.2, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-05-29"),  probiotic = 00.0, steroid = 00.0, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-07-27"),  probiotic = 00.0, steroid = 00.0, fiber = 00.5, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-08-01"),  probiotic = 00.0, steroid = 00.0, fiber = 01.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-08-10"),  probiotic = 00.0, steroid = 00.0, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-08-24"),  probiotic = 05.0, steroid = 00.0, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-08-29"),  probiotic = 10.0, steroid = 00.0, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-09-07"),  probiotic = 00.0, steroid = 00.2, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.08),
  list(begin = ymd("2024-09-21"),  probiotic = 00.0, steroid = 00.0, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-10-19"),  probiotic = 00.0, steroid = 00.2, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-11-16"),  probiotic = 00.0, steroid = 00.1, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
  list(begin = ymd("2024-12-28"),  probiotic = 00.0, steroid = 00.0, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00)
)
supplement_details <- list(
  fiber         = list(rx = "Metamucil"    , units = "capsules per day", route = "in gum or on biscuits", dose = list(low = 0.5, high = 1.0)),
  probiotic     = list(rx = "OTC"          , units = "drops per day"   , route = "in gum or on biscuits", dose = list(low = 5.0, high = 10.0)),
  steroid       = list(rx = "Budesonide"   , units = "mg per day"      , route = "in gum or on biscuits", dose = list(low = 0.1, high = 0.2)),
  antibiotic    = list(rx = "Metronidazole", units = "mg per day"      , route = "in gum or on biscuits", dose = 12.5 ),
  antidiarrheal = list(rx = "Loperamide"   , units = "mg per day"      , route = "in gum or on biscuits", dose = 0.08)
)  %>%
  enframe(name = "supplement") %>%
  unnest_wider(value) %>%
  unnest_wider(dose, names_sep = "_") %>%
  rowwise() %>%
  mutate(dose_low = replace_na(dose_low, dose_1)) %>%
  mutate(dose_high = replace_na(dose_high, dose_1), .keep = "unused") %>%
  ungroup() %>%
  select(supplement, rx, dose_low, dose_high, units)

diet_trials <- list(
  list(begin = ymd("2023-10-26"), diet = "Baseline"),
  list(begin = ymd("2023-11-23"), diet = "Oatmeal Gel"), 
  list(begin = ymd("2023-12-14"), diet = "Baseline"),
  list(begin = ymd("2024-01-25"), diet = "Oatmeal Gel"),
  list(begin = ymd("2024-05-29"), diet = "Baseline"), 
  list(begin = ymd("2024-06-15"), diet = "Biscuit elimination"),
  list(begin = ymd("2024-06-29"), diet = "Less bugs, more egg"),
  list(begin = ymd("2024-07-13"), diet = "Sweet potato, green beans, carrots, pumpkin"), 
  list(begin = ymd("2024-07-27"), diet = "Baseline"),  
  list(begin = ymd("2024-08-10"), diet = "Oatmeal Gel"), 
  list(begin = ymd("2024-08-24"), diet = "Baseline"), 
  list(begin = ymd("2024-09-21"), diet = "Low lectin"), 
  list(begin = ymd("2024-10-19"), diet = "Baseline")
) %>% .[order(map_dbl(., ~ .x$begin))]

diet_factors <- c(
  "Baseline"                                    = "baseline",
  "Oatmeal Gel"                                 = "oatmeal_gel",
  "Biscuit elimination"                         = "biscuit_elimination",
  "Less bugs, more egg"                         = "less_bugs_more_egg",
  "Sweet potato, green beans, carrots, pumpkin" = "sweet_potato_green_beans_carrots_pumpkin",
  "Low lectin"                                  = "low_lectin",
  "Other"                                       = "36_gum_37_veg_27_insects",
  "Other"                                       = "31_watery_veg_31_root_veg_19_insects_egg_19_gum"
)

meds_warble <- tibble(
  subject       = "warble",
  antibiotic    = 0,
  antidiarrheal = 0,
  fiber         = 0,
  probiotic     = 0,
  steroid       = 0,
  collection_date = seq.Date(
    ymd("2023-10-26"),
    ymd("2024-12-28"),
    by = "day"
  )
)

diets_warble <- tibble(
  subject       = "warble",
  diet          = "Baseline",
  collection_date = seq.Date(
    ymd("2023-10-26"),
    ymd("2024-12-28"),
    by = "day"
  )
)


nutrition_meds_warble <- tibble(
  subject       = "warble",
  diet          = "Baseline",
  antibiotic    = 0,
  antidiarrheal = 0,
  fiber         = 0,
  probiotic     = 0,
  steroid       = 0,
  collection_date = seq.Date(
    ymd("2023-10-26"),
    ymd("2024-12-28"),
    by = "day"
  )
) %>%
  left_join(nutrition, by = join_by(diet == diet_name))

