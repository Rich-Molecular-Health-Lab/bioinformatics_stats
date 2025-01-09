repro.log    <- tribble(
  
  ~category,     ~event_state,    ~title,                  ~interval,  
  "access"      ,     "state"  ,  "together"              ,   interval(make_date(2023, 10, 01),       make_date(2023, 10, 29))    ,
  "w_cycle"     ,     "state"  ,  "estrus"                ,   interval(make_date(2023, 10, 01),       make_date(2023, 10, 29))    ,
  "access"      ,     "state"  ,  "separated"             ,   interval(make_date(2023, 10, 30),       make_date(2024, 04, 01))    ,
  "w_cycle"     ,     "state"  ,  "estrus"                ,   interval(make_date(2024, 04, 01),       make_date(2024, 04, 05))    ,    
  "access"      ,     "state"  ,  "together"              ,   interval(make_date(2024, 04, 02),       make_date(2024, 04, 15))    ,
  "physiology"  ,     "event"  ,  "swollen_testicles"     ,   interval(make_date(2024, 02, 29),       make_date(2024, 02, 29))    ,
  "physiology"  ,     "event"  ,  "swollen_vulva"         ,   interval(make_date(2024, 03, 21),       make_date(2024, 03, 21))    ,
  "behavior"    ,     "event"  ,  "culi_whistling"        ,   interval(make_date(2024, 03, 21),       make_date(2024, 03, 21))    ,
  "behavior"    ,     "event"  ,  "warble_whistling"      ,   interval(make_date(2024, 04, 04),       make_date(2024, 04, 04))    ,
  "copulation"  ,     "event"  ,  "copulation_observed"   ,   interval(make_date(2024, 04, 03),       make_date(2024, 04, 03))    ,
  "copulation"  ,     "event"  ,  "copulation_observed"   ,   interval(make_date(2024, 04, 04),       make_date(2024, 04, 04))    ,
  "copulation"  ,     "event"  ,  "sperm_plug_observed"   ,   interval(make_date(2024, 04, 05),       make_date(2024, 04, 05))    ,
  "w_cycle"     ,     "state"  ,  "not_pregnant"          ,   interval(make_date(2023, 10, 01),       make_date(2024, 04, 05))    ,
  "w_cycle"     ,     "state"  ,  "pregnant"              ,   interval(make_date(2024, 04, 05),       make_date(2024, 10, 09))    ,
  "w_cycle"     ,     "event"  ,  "birth"                 ,   interval(make_date(2024, 10, 09),       make_date(2024, 10, 09))    ,
  "w_cycle"     ,     "state"  ,  "not_pregnant"          ,   interval(make_date(2024, 10, 09),       now())                      ,
  "w_cycle"     ,     "event"  ,  "infanticide"           ,   interval(make_date(2024, 10, 10),       make_date(2024, 10, 10))    ) %>%
  mutate(start = int_start(interval),
         end = int_end(interval))

warble.cyle.dt        <- repro.log %>% filter(category == "w_cycle")  %>% 
  mutate(warble_cycle = title,
         start        = decimal_date(start),
         end          = decimal_date(end)) %>%  
  dplyr::select(-c(
    "interval", 
    "category",
    "event_state",
    "title")) %>%
  as.data.table()

setkey(warble.cyle.dt, start, end)

breeding.access.dt    <- repro.log %>% filter(category == "access")   %>% 
  mutate(access       = title,
         start        = decimal_date(start),
         end          = decimal_date(end), .keep = "unused") %>% 
  dplyr::select(-c(
    "interval", 
    "category",
    "event_state")) %>%
  as.data.table()

setkey(breeding.access.dt, start, end)
