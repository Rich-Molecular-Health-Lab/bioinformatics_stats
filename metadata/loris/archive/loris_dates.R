diet_trials <- list(
  list(begin = ymd(path$day1   ), diet = "Baseline"),
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

warble_trials <- list(
  list(begin = ymd(path$day1   ), diet = "Baseline")
) %>% enframe(name = NULL) %>% 
  unnest_wider(value) %>%
  mutate(diet_name = diet, .keep = "unused") %>%
  mutate(end = today(), subject = "warble")

supplement_details <- list(
  fiber         = list(rx = "Metamucil"    , units = "capsules per day", route = "in gum or on biscuits", dose = list(low = 0.5, high = 1.0)),
  probiotic     = list(rx = "OTC"          , units = "drops per day"   , route = "in gum or on biscuits", dose = list(low = 5.0, high = 10.0)),
  steroid       = list(rx = "Budesonide"   , units = "mg per day"      , route = "in gum or on biscuits", dose = list(low = 0.1, high = 0.2)),
  antibiotic    = list(rx = "Metronidazole", units = "mg per day"      , route = "in gum or on biscuits", dose = 12.5 ),
  antidiarrheal = list(rx = "Loperamide"   , units = "mg per day"      , route = "in gum or on biscuits", dose = 0.08)
)

supplements <- list(
  list(begin = ymd(path$day1   ),  probiotic = 00.0, steroid = 00.0, fiber = 00.0, antibiotic = 00.0, antidiarrheal = 00.00),
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
