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


