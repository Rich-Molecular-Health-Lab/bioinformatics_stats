trials <- list(
  list(begin = ymd(path$day1   ), days = 07, diet = diets$baseline       , supplement = NULL                 , sup_dose = NULL),
  list(begin = ymd("2023-11-02"), days = 05, diet = diets$baseline       , supplement = supplements$probiotic, sup_dose = "low" ),
  list(begin = ymd("2023-11-07"), days = 09, diet = diets$baseline       , supplement = supplements$probiotic, sup_dose = "high"),
  list(begin = ymd("2023-11-23"), days = 14, diet = diets$oatgel         , supplement = supplements$probiotic, sup_dose = "high"),
  list(begin = ymd("2023-12-14"), days = 14, diet = diets$baseline       , supplement = supplements$steroid   , sup_dose = "low"),
  list(begin = ymd("2024-01-04"), days = 14, diet = diets$baseline       , supplement = supplements$steroid   , sup_dose = "high"),
  list(begin = ymd("2024-01-25"), days = 26, diet = diets$oatgel         , supplement = list(supplements$probiotic, supplements$steroid), sup_dose = list("high", "high")),
  list(begin = ymd("2024-02-20"), days = 03, diet = diets$oatgel         , supplement = supplements$steroid    , sup_dose = "high"),
  list(begin = ymd("2024-02-23"), days = 96, diet = diets$oatgel         , supplement = list(supplements$probiotic, supplements$steroid), sup_dose = list("high", "high")),
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
