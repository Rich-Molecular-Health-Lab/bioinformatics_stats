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
  reframe(x = as.character(str_flatten(note, collapse = "; ")), .by = c(date, subject, tag)) %>%
  mutate(start = date, end = date, category = "note", .keep = "unused")

warble_cycles <- list(
  tibble(start = ymd("2023-10-26"), end = ymd("2023-10-29"), x = "estrus"),
  tibble(start = ymd("2024-03-21"), end = ymd("2024-04-06"), x = "estrus"),
  tibble(start = ymd("2024-04-07"), end = ymd("2024-10-09"), x = "pregnant")
) %>%
  list_rbind() %>%
  mutate(category = "warb_cycle", tag = "repro", subject = "warble")

repro_dates <- warble_cycles %>%
  bind_rows(mutate(warble_cycles, subject = "culi"))


access_dates <- list(
  tibble(start = ymd("2023-10-26"), end = ymd("2023-10-29")),
  tibble(start = ymd("2024-04-02"), end = ymd("2024-04-15"))
) %>%
  list_rbind() %>%
  mutate(x = "together", tag = "repro", category = "pair_access")

access_pair <- access_dates %>%
  mutate(subject = "culi") %>%
  bind_rows(mutate(access_dates, subject = "warble"))

holdings <- list(
  culi = list(
    tibble(x = "old", start = ymd("2023-10-26"), end = ymd("2024-02-27")),
    tibble(x = "new", start = ymd("2024-02-28"), end = ymd("2024-12-14"))
    ),
  warble = list(
    tibble(x = "old", start = ymd("2023-10-26"), end = ymd("2024-02-28")),
    tibble(x = "new", start = ymd("2024-02-29"), end = ymd("2024-12-14"))
  )
) %>%
  map_depth(1, \(x) list_rbind(x)) %>%
  enframe(name = "subject") %>%
  unnest(value) %>%
  mutate(category = "environment", tag = "health")

supplements <- list(
  probiotic     = list(
    tibble(start = ymd("2023-11-02"), level = 0.5, end = ymd("2023-11-06")),
    tibble(start = ymd("2023-11-07"), level = 1.0, end = ymd("2023-12-13")),
    tibble(start = ymd("2024-01-25"), level = 1.0, end = ymd("2024-02-19")),
    tibble(start = ymd("2024-02-23"), level = 1.0, end = ymd("2024-05-28")),
    tibble(start = ymd("2024-08-24"), level = 0.5, end = ymd("2024-08-28")),
    tibble(start = ymd("2024-08-29"), level = 1.0, end = ymd("2024-09-06"))
  ),
  steroid       = list(
    tibble(start = ymd("2023-12-14"), level = 0.5, end = ymd("2024-01-03")), 
    tibble(start = ymd("2024-01-04"), level = 1.0, end = ymd("2024-05-28")), 
    tibble(start = ymd("2024-09-07"), level = 1.0, end = ymd("2024-09-20")), 
    tibble(start = ymd("2024-10-19"), level = 1.0, end = ymd("2024-11-15")), 
    tibble(start = ymd("2024-11-16"), level = 0.5, end = ymd("2024-12-27"))
  ),
  fiber         = list(    
    tibble(start = ymd("2024-07-27"), level = 0.5, end = ymd("2024-07-31")),    
    tibble(start = ymd("2024-08-01"), level = 1.0, end = ymd("2024-08-09"))  
    ), 
  antibiotic    = list(    
    tibble(start = ymd("2024-04-29"), level = 1.0, end = ymd("2024-05-05"))  
    ),  
  antidiarrheal = list(    
    tibble(start = ymd("2024-09-07"), level = 1.0, end = ymd("2024-09-20"))
  )
) %>%
  map_depth(1, \(x) list_rbind(x)) %>%
  enframe(name = "x") %>%
  unnest(value) %>%
  mutate(category = "supplements", subject = "culi", tag = "health")


diet_trials <- list(
  tibble(start = ymd("2023-10-26"), end = ymd("2023-11-22"), x = "Baseline"),
  tibble(start = ymd("2023-11-23"), end = ymd("2023-12-13"), x = "Oatmeal Gel"), 
  tibble(start = ymd("2023-12-14"), end = ymd("2024-01-24"), x = "Baseline"),
  tibble(start = ymd("2024-01-25"), end = ymd("2024-05-28"), x = "Oatmeal Gel"),
  tibble(start = ymd("2024-05-29"), end = ymd("2024-06-14"), x = "Baseline"), 
  tibble(start = ymd("2024-06-15"), end = ymd("2024-06-28"), x = "Biscuit elimination"),
  tibble(start = ymd("2024-06-29"), end = ymd("2024-07-12"), x = "Less bugs, more egg"),
  tibble(start = ymd("2024-07-13"), end = ymd("2024-07-26"), x = "Sweet potato, green beans, carrots, pumpkin"), 
  tibble(start = ymd("2024-07-27"), end = ymd("2024-08-09"), x = "Baseline"),  
  tibble(start = ymd("2024-08-10"), end = ymd("2024-08-23"), x = "Oatmeal Gel"), 
  tibble(start = ymd("2024-08-24"), end = ymd("2024-09-20"), x = "Baseline"), 
  tibble(start = ymd("2024-09-21"), end = ymd("2024-10-18"), x = "Low lectin"), 
  tibble(start = ymd("2024-10-19"), end = ymd("2024-12-14"), x = "Baseline")
)  %>%
  list_rbind() %>%
  mutate(category = "diet", subject = "culi", tag = "food") %>%
  bind_rows(
    tibble(
      category = "diet", 
      subject  = "warble", 
      x        = "Baseline", 
      start    = ymd("2023-10-26"),
      end      = ymd("2024-12-14"), 
      tag      = "food"
      )
    )
