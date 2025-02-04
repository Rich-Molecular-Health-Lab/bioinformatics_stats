meds <- list(
  culi = tribble(
    ~start_day        , ~last_day        , ~med_type   , ~med_name      , ~med_dose, ~dose_units,
    ymd(path$day1)    , ymd("2024-04-28"), "none"      , "none"         , 0        , "mg/day",
    ymd("2024-04-29") , ymd("2024-05-06"), "antibiotic", "Metronidazole", 12.5     , "mg/day",
    ymd("2024-05-06") , ymd(path$last)   , "none"      , "none"         , 0        , "mg/day"
  ),
  warble = tribble(
    ~start_day    , ~last_day     , ~med_type, ~med_name , ~med_dose, ~dose_units,
    ymd(path$day1), ymd(path$last), "none"   , "none"    , 0        , "mg/day"
  )
)

meds.list <- list(
  culi = list(
    antibiotic = tibble(
      start_day  = ymd("2024-04-29"), 
      last_day   = ymd("2024-05-06"), 
      med_name   = "Metronidazole", 
      dose       = 12.5, 
      dose_units = "mg/day"
    )
  )
)