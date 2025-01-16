# steroid doses are calculated as mg/day while probiotic and oatgel values are binary (included = 1, none = 0)
diet_trials   <- list(
  baseline      = tibble(steroid = 0.00, probiotic = 0, oatgel = 0),
  option1       = tibble(steroid = 0.00, probiotic = 1, oatgel = 0),
  option2       = tibble(steroid = 0.00, probiotic = 1, oatgel = 1),
  option3       = tibble(steroid = 0.10, probiotic = 0, oatgel = 0),
  option4       = tibble(steroid = 0.20, probiotic = 0, oatgel = 0),
  option5       = tibble(steroid = 0.20, probiotic = 1, oatgel = 1),
  option6       = tibble(steroid = 0.20, probiotic = 0, oatgel = 1),
  option7       = tibble(steroid = 0.10, probiotic = 1, oatgel = 1),
  option8       = tibble(steroid = 0.05, probiotic = 1, oatgel = 1)
)

optionals <- list(
  culi = list(
  tomatoes    = tibble(start_day = ymd(path$day1), last_day = ymd("2024-02-19")),
  cauliflower = tibble(start_day = ymd(path$day1), last_day = ymd("2024-01-22")),
  broccoli    = tibble(start_day = ymd(path$day1), last_day = ymd("2024-01-22"))
) )

foods.removed  <- list(
  culi = list(
    tomatoes    = ymd("2024-02-19"),
    cauliflower = ymd("2024-01-22"),
    broccoli    = ymd("2024-01-22")
  ) 
)


culi.diet.schedule <- list(
  baseline = tibble(start_day = ymd(loris$day1)  , last_day = ymd("2023-11-01"), diet = diet_trials$baseline),
  phase1   = tibble(start_day = ymd("2023-11-02"), last_day = ymd("2023-11-22"), diet = diet_trials$option1),
  phase2   = tibble(start_day = ymd("2023-11-23"), last_day = ymd("2023-12-13"), diet = diet_trials$option2),
  phase3   = tibble(start_day = ymd("2023-12-14"), last_day = ymd("2024-01-03"), diet = diet_trials$option3),
  phase4   = tibble(start_day = ymd("2024-01-04"), last_day = ymd("2024-01-24"), diet = diet_trials$option4),
  phase5   = tibble(start_day = ymd("2024-01-25"), last_day = ymd("2024-02-19"), diet = diet_trials$option5),
  phase6   = tibble(start_day = ymd("2024-02-20"), last_day = ymd("2024-02-22"), diet = diet_trials$option6),
  phase7   = tibble(start_day = ymd("2024-02-23"), last_day = ymd("2024-05-14"), diet = diet_trials$option7),
  phase8   = tibble(start_day = ymd("2024-05-15"), last_day = ymd("2024-05-16"), diet = diet_trials$option8)
)

culi.trials <- tribble(
  ~start_day,        ~last_day        , ~diet,
  ymd(loris$day1)  , ymd("2023-11-01"), diet_trials$baseline,
  ymd("2023-11-02"), ymd("2023-11-22"), diet_trials$option1,
  ymd("2023-11-23"), ymd("2023-12-13"), diet_trials$option2,
  ymd("2023-12-14"), ymd("2024-01-03"), diet_trials$option3,
  ymd("2024-01-04"), ymd("2024-01-24"), diet_trials$option4,
  ymd("2024-01-25"), ymd("2024-02-19"), diet_trials$option5,
  ymd("2024-02-20"), ymd("2024-02-22"), diet_trials$option6,
  ymd("2024-02-23"), ymd("2024-05-14"), diet_trials$option7,
  ymd("2024-05-15"), ymd("2024-05-16"), diet_trials$option8
)
