diet_periods <- read.csv(here("metadata/loris/hdz_nutritionData_2025-1-30.csv"), header = T) %>%
  mutate(date   = mdy(Date)) %>%
  select(date, Period, Notes) %>%
  mutate(diet_phase = fct_recode(Period, !!!diet_syntax_key)) %>%
  mutate(diet_phase = fct_expand(diet_phase, c("option6", "option7", "option8"))) %>%
  mutate(diet_phase = case_when(
    between(date, ymd("2024-2-15"), ymd("2024-2-19")) ~ "option5",
    between(date, ymd("2024-2-20"), ymd("2024-2-22")) ~ "option6",
    between(date, ymd("2024-2-23"), ymd("2024-2-22")) ~ "option6"
  ))
left_join(diet_tbl_key, by = join_by(diet_phase)) %>%
  unnest_wider(col = "diet")
