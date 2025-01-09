meds    <- tribble(
  ~subject,     ~med_type,      ~med_name,        ~mgpml,     ~dose_ml,   ~dose_npday,           ~interval,  
  "culi"     ,  "steroid"     , "Budesonide"    ,  10       ,   0.01     ,   1         ,    interval(make_date(2023, 12, 14),   make_date(2024, 01, 03)) ,
  "culi"     ,  "steroid"     , "Budesonide"    ,  10       ,   0.02     ,   1         ,    interval(make_date(2024, 01, 04),   make_date(2024, 05, 14)) ,
  "culi"     ,  "steroid"     , "Budesonide"    ,  10       ,   0.01     ,   1         ,    interval(make_date(2024, 05, 15),   make_date(2024, 05, 22)) ,
  "culi"     ,  "steroid"     , "Budesonide"    ,  10       ,   0.01     ,   0.5       ,    interval(make_date(2024, 05, 22),                   today()) ,
  "culi"     ,  "antibiotic"  , "Metronidazole" ,  250      ,   0.05     ,   1         ,    interval(make_date(2024, 04, 29),   make_date(2024, 05, 06)) ,
  "both"     ,  "none"        , "none"          ,  0        ,   0        ,   0         ,    interval(make_date(2023, 10, 26),   make_date(2023, 11, 01)) 
) %>% 
  
  mutate(mg_p_day = mgpml * dose_ml * dose_npday,
         start    = int_start(interval),
         end      = int_end(interval)) %>% arrange(start, end) %>%
  filter(med_type != "none") %>% 
  mutate(subject = 
           factor(subject, 
                  levels = 
                    c("warble",
                      "culi",
                      "unknown",
                      "ntc")))
meds.dt  <- meds %>% filter(med_type != "steroid") %>% 
  mutate(start = decimal_date(start),
         end   = decimal_date(end)) %>%
  dplyr::select(-interval) %>%
  as.data.table()
setkey(meds.dt, subject, start, end)

