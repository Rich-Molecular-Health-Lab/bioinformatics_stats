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
