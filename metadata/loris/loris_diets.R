diet.log.culi   <- tribble(
  ~category,     ~event_state,  ~title,                   ~ster_mgpml,  ~ster_dose_ml,  ~ster_npday, ~interval                                                , ~subject,
  "state_trial", "state"    ,  "baseline"                 ,  0        ,       0   ,      0       ,  interval(make_date(2023, 10, 26), make_date(2023, 11, 01)) , "culi"  ,
  "state_trial", "state"    ,  "probiotic"                ,  0        ,       0   ,      0       ,  interval(make_date(2023, 11, 02), make_date(2023, 11, 22)) , "culi"  ,
  "state_trial", "state"    ,  "probiotic_oat"            ,  0        ,       0   ,      0       ,  interval(make_date(2023, 11, 23), make_date(2023, 12, 13)) , "culi"  ,
  "state_trial", "state"    ,  "steroid"                  ,  10       ,    0.01   ,      1       ,  interval(make_date(2023, 12, 14), make_date(2024, 01, 03)) , "culi"  ,
  "state_trial", "state"    ,  "steroid"                  ,  10       ,    0.02   ,      1       ,  interval(make_date(2024, 01, 04), make_date(2024, 01, 24)) , "culi"  ,
  "state_trial", "state"    ,  "steroid_probiotic_oat"    ,  10       ,    0.02   ,      1       ,  interval(make_date(2024, 01, 25), make_date(2024, 02, 19)) , "culi"  ,
  "state_trial", "state"    ,  "steroid_oat"              ,  10       ,    0.02   ,      1       ,  interval(make_date(2024, 02, 20), make_date(2024, 02, 22)) , "culi"  ,
  "state_trial", "state"    ,  "steroid_probiotic_oat"    ,  10       ,    0.02   ,      1       ,  interval(make_date(2024, 02, 23), make_date(2024, 05, 14)) , "culi"  ,
  "state_trial", "state"    ,  "steroid_probiotic_oat"    ,  10       ,    0.01   ,      1       ,  interval(make_date(2024, 05, 15), make_date(2024, 05, 22)) , "culi"  ,
  "state_trial", "state"    ,  "steroid_probiotic_oat"    ,  10       ,    0.01   ,    0.5       ,  interval(make_date(2024, 05, 23),                 today()) , "culi"  ,
  "event_food" , "event"    ,   "tomato removed"          ,  10       ,    0.02   ,      1       ,  interval(make_date(2024, 02, 20), make_date(2024, 02, 20)) , "culi"  ,
  "event_food" , "event"    ,   "cauliflower/broc removed",  10       ,    0.02   ,      1       ,  interval(make_date(2024, 01, 22), make_date(2024, 01, 22)) , "culi"  ) %>% 
  
  mutate(ster_mgpday = ster_mgpml * ster_dose_ml * ster_npday,
         start = int_start(interval),
         end   = int_end(interval)) %>% arrange(start, end) 

diet.log.warble   <- tribble(
  ~category,     ~event_state,  ~title,     ~ster_mgpml,  ~ster_dose_ml,  ~ster_npday, ~interval,                                      ~subject,
  "state_trial", "state"    ,  "baseline"   ,  0        ,       0   ,      0         ,  interval(make_date(2023, 10, 26),  today()),    "warble") %>%
  mutate(ster_mgpday = 0,
         start = int_start(interval),
         end   =   int_end(interval)) %>% arrange(start, end) 

diet.log.both <- bind_rows(diet.log.culi, diet.log.warble)

diet.dt       <- diet.log.both %>% filter(category == "state_trial") %>%
  dplyr::select(-c("interval", 
                   "category",
                   "event_state")) %>%
  mutate(start      = decimal_date(start),
         end        = decimal_date(end),
         diet_trial = title, 
         subject    = factor(subject,
                             levels = c("warble",
                                        "culi",
                                        "unknown",
                                        "ntc")),
         .keep = "unused") %>% 
  as.data.table()
setkey(diet.dt, subject, start, end)

diet.culi.dt       <- diet.log.culi %>% filter(category == "state_trial") %>%
  dplyr::select(start,
                end,
                culi_trial = title) %>%
  mutate(start      = decimal_date(start),
         end        = decimal_date(end),
         .keep = "unused") %>% 
  as.data.table()
setkey(diet.culi.dt, start, end)


diet.table <- diet.log.both %>%
  arrange(start, rev(subject)) %>%
  dplyr::select(Subject  = subject,
                Start    = start,
                Diet     = title,
                Ster_mg  = ster_mgpday) %>%
  mutate(Subject  = str_to_title(Subject),
         Diet     = str_to_title(str_replace_all(Diet, "_", " + "))) %>%
  mutate(Diet =   if_else(str_detect(Diet, "Steroid"), 
                          str_glue("{Diet}", fixed(" ("), "{Ster_mg}", fixed(" mg/day)")), Diet)) %>%
  dplyr::select(-Ster_mg) %>%
  kable() %>%
  kable_styling(full_width = T,
                fixed_thead = T,
                c("hover", "responsive"),
                position = "center") %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1, valign = "top") %>%
  scroll_box(width = "800px", height = "400px")
