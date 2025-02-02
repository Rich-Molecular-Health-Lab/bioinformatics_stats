diet_trials <- list(
  list(begin = ymd(path$day1   ), days = 07, diet = diets$baseline       , supplement = NULL                 , sup_dose = NULL),
  list(begin = ymd("2023-11-02"), days = 05, diet = diets$baseline       , supplement = supplements$probiotic, sup_dose = "low" ),
  list(begin = ymd("2023-11-07"), days = 09, diet = diets$baseline       , supplement = supplements$probiotic, sup_dose = "high"),
  list(begin = ymd("2023-11-23"), days = 14, diet = diets$oatgel         , supplement = supplements$probiotic, sup_dose = "high"),
  list(begin = ymd("2023-12-14"), days = 14, diet = diets$baseline       , supplement = supplements$steroid   , sup_dose = "low"),
  list(begin = ymd("2024-01-04"), days = 14, diet = diets$baseline       , supplement = supplements$steroid   , sup_dose = "high"),
  list(begin = ymd("2024-01-25"), days = 26, diet = diets$oatgel         , supplement = list(supplements$probiotic, supplements$steroid), sup_dose = list("high", "high")),
  list(begin = ymd("2024-02-20"), days = 03, diet = diets$oatgel         , supplement = supplements$steroid    , sup_dose = "high"),
  list(begin = ymd("2024-02-23"), days = 66, diet = diets$oatgel         , supplement = list(supplements$probiotic, supplements$steroid), sup_dose = list("high", "high")),
  list(begin = ymd("2024-04-29"), days = 07, diet = diets$oatgel         , supplement = list(supplements$probiotic, supplements$steroid, supplements$antibiotic), sup_dose = list("high", "high", "default")),
  list(begin = ymd("2024-05-06"), days = 23, diet = diets$oatgel         , supplement = list(supplements$probiotic, supplements$steroid), sup_dose = list("high", "high")),
  list(begin = ymd("2024-05-29"), days = 17, diet = diets$baseline       , supplement = NULL, sup_dose = NULL),
  list(begin = ymd("2024-06-15"), days = 14, diet = diets$biscuit_elim   , supplement = NULL, sup_dose = NULL),
  list(begin = ymd("2024-06-29"), days = 14, diet = diets$lessBug_moreEgg, supplement = NULL, sup_dose = NULL),
  list(begin = ymd("2024-07-13"), days = 14, diet = diets$seasonals      , supplement = NULL, sup_dose = NULL),
  list(begin = ymd("2024-07-27"), days = 05, diet = diets$baseline       , supplement = supplements$fiber, sup_dose = "low"),
  list(begin = ymd("2024-08-01"), days = 09, diet = diets$baseline       , supplement = supplements$fiber, sup_dose = "high"),
  list(begin = ymd("2024-08-10"), days = 14, diet = diets$oatgel         , supplement = NULL, sup_dose = NULL),
  list(begin = ymd("2024-08-24"), days = 05, diet = diets$baseline       , supplement = supplements$probiotic, sup_dose = "low"),
  list(begin = ymd("2024-08-29"), days = 09, diet = diets$baseline       , supplement = supplements$probiotic, sup_dose = "high"),
  list(begin = ymd("2024-09-07"), days = 14, diet = diets$baseline       , supplement = supplements$antidiarrheal, sup_dose = "unknown"),
  list(begin = ymd("2024-09-21"), days = 14, diet = diets$low_lectin     , supplement = NULL, sup_dose = NULL),
  list(begin = ymd("2024-10-19"), days = 28, diet = diets$baseline       , supplement = supplements$steroid    , sup_dose = "high"),
  list(begin = ymd("2024-11-16"), days = 42, diet = diets$baseline       , supplement = supplements$steroid    , sup_dose = "low"),
  list(begin = ymd("2024-12-28"), days = 06, diet = diets$baseline       , supplement = NULL, sup_dose = NULL)
) %>% set_names()

diet_modifications <- list(
  list(begin = ymd("2024-01-22"), change = "removing cauliflower and broccoli from diet rotation"),
  list(begin = ymd("2024-02-20"), change = "removing tomato from diet rotation")
)

environment <- list(
  new_holding_start = list(
    culi   = ymd("2024-02-28"),
    warble = ymd("2024-02-29")
  ),
  pair_access = list(
    list(begin = ymd(path$day1)   , end = ymd("2023-10-29")),
    list(begin = ymd("2024-04-02"), end = ymd("2024-04-15"))
  )
)

warble_cycles <- list(
  estrus = list(
  list(begin = ymd(path$day1)   , end = ymd("2023-10-29")),
  list(begin = ymd("2024-03-21"), end = ymd("2024-04-06"))
  ),
  pregnant = list(
    list(begin = ymd("2024-04-06"), end = ymd("2024-10-09"))
  )
)

log <- list(
  list(date = ymd("2024-03-21"), tag = "repro" , subject = "culi"             , note = "Culi whistling and actively attempting to get to Warble"),
  list(date = ymd("2024-04-04"), tag = "repro" , subject = c("culi", "warble"), note = "Observed several breeding events"),
  list(date = ymd("2023-11-16"), tag = "health", subject = "culi"             , note = "had what appeared to be dried blood around his anus and tail"),
  list(date = ymd("2023-12-17"), tag = "health", subject = "culi"             , note = "coprophagy observed"),
  list(date = ymd("2024-01-21"), tag = "health", subject = "culi"             , note = "coprophagy observed"),
  list(date = ymd("2024-02-10"), tag = "health", subject = "culi"             , note = "left biscuit and some gum"),
  list(date = ymd("2024-04-01"), tag = "repro" , subject = "warble"           , note = "swollen vulva"),
  list(date = ymd("2024-04-02"), tag = "repro" , subject = "culi"             , note = "swollen testicles"),
  list(date = ymd("2024-04-03"), tag = "repro" , subject = c("culi", "warble"), note = "Keeper Zach entered the facility in the morning to clean and found Warble and Culi sleeping together in a hammock in room 3"),
  list(date = ymd("2024-04-03"), tag = "repro" , subject = c("culi", "warble"), note = "Observed breeding event"),
  list(date = ymd("2024-04-04"), tag = "repro" , subject = "warble"           , note = "Observed whistling"),
  list(date = ymd("2024-04-04"), tag = "repro" , subject = c("culi", "warble"), note = "JM and AMR found Warble and Culi sleeping together in a hammock in room 3"),
  list(date = ymd("2024-04-04"), tag = "repro" , subject = "warble"           , note = "swollen vulva"),
  list(date = ymd("2024-04-06"), tag = "repro" , subject = "warble"           , note = "pregnancy suspected"),
  list(date = ymd("2024-04-14"), tag = "health", subject = "culi"             , note = "weight = 451 g"),
  list(date = ymd("2024-05-13"), tag = "health", subject = "warble"           , note = "weight = 467 g"),
  list(date = ymd("2024-06-17"), tag = "health", subject = "culi"             , note = "left pieces of food in room 5"),
  list(date = ymd("2024-08-08"), tag = "health", subject = "culi"             , note = "extremely liquid feces"),
  list(date = ymd("2024-09-08"), tag = "health", subject = "culi"             , note = "extremely liquid feces"),
  list(date = ymd("2025-01-23"), tag = "repro" , subject = c("culi", "warble"), note = "Social grooming observed between the barriers. Culi initiated grooming by licking Warble’s head; she licked his arm, and he was observed licking her genitals. No whistling was observed."),
  list(date = ymd("2024-10-09"), tag = "repro" , subject = "warble"           , note = "gave birth to twins"),
  list(date = ymd("2024-10-10"), tag = "repro" , subject = "warble"           , note = "double infanticide")
)
