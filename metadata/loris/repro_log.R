repro.log    <- list(
  access = tribble(
    ~value,     ~start_day,  ~last_day,
    1     , ymd(path$day1)   , ymd("2023-10-29"),
    0     , ymd("2023-10-30"), ymd("2024-04-01"),
    1     , ymd("2024-04-02"), ymd("2024-04-15"),
    0     , ymd("2024-04-16"), ymd(path$last)
  ),
  estrus = tribble(
    ~value,     ~start_day,  ~last_day,
    1     , ymd(path$day1)   , ymd("2023-10-29"),
    0     , ymd("2023-10-30"), ymd("2024-03-21"),
    1     , ymd("2024-03-22"), ymd("2024-04-05"),
    0     , ymd("2024-04-06"), ymd(path$last)
  ),
  pregnant = tribble(
    ~value,     ~start_day,  ~last_day,
    0     , ymd(path$day1)   , ymd("2024-04-05"),
    1     , ymd("2024-04-06"), ymd("2024-10-08"),
    0     , ymd("2024-10-09"), ymd(path$last)
  ),
  notes = tribble(
    ~repro_note          , ~subject  ,  ~Date,
    "swollen_testicles"  , "culi"    , ymd("2024-02-29"),
    "swollen_vulva"      , "warble"  , ymd("2024-03-21"),
    "whistling"          , "culi"    , ymd("2024-03-21"),
    "whistling"          , "warble"  , ymd("2024-04-04"),
    "copulation_observed", "both"    , ymd("2024-04-03"),
    "copulation_observed", "both"    , ymd("2024-04-04"),
    "sperm_plug_observed", "warble"  , ymd("2024-04-05"),
    "birth"              , "warble"  , ymd("2024-10-09"),
    "double infanticide" , "warble"  , ymd("2024-10-10")
  )
) 
